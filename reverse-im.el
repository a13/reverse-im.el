;;; reverse-im.el --- Reverse mapping for non-default system layouts -*- lexical-binding: t -*-

;; Author: Juri Linkov <juri@jurta.org> (initial idea)
;; Maintainer: DK <a13@users.noreply.github.com>
;; Package-Requires: ((emacs "25.1") (seq "2.23"))
;; Keywords: i18n
;; Homepage: https://github.com/a13/reverse-im.el
;; Version: 0.0.8

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Override the parent keymap of `function-key-map' for the preferred input method
;; to translate input sequences to the default system layout (English), so we can
;; use Emacs bindings while the non-default system layout is active.
;; Usage example:
;; (use-package reverse-im
;;   :ensure t
;;   :custom
;;   (reverse-im-input-methods '("ukrainian-computer")) ; put your input-method(s) here
;;   :config
;;   (reverse-im-mode t))

;; or, alternatively, add the library to your `load-path'
;; and (reverse-im-activate "ukrainian-computer") manually

;;; Code:

(require 'quail)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; Customs
(defgroup reverse-im nil
  "Translate input methods."
  :group 'I18n)

(defcustom reverse-im-input-methods
  nil
  "List of input methods to activate when minor-mode is on."
  :group 'reverse-im
  :type `(repeat (choice (const nil)
                         mule-input-method-string)))

(defcustom reverse-im-modifiers
  '(control meta super)
  "List of modifiers to translate with."
  :type '(repeat symbol)
  :group 'reverse-im)

(defcustom reverse-im-char-fold
  nil
  "Activate reverse mappings for char-fold."
  :type 'boolean
  :group 'reverse-im)

(defcustom reverse-im-avy-action-char
  ?T
  "Char for `reverse-im-avy-action-translate'.  Set to nil to turn avy action off."
  :type 'character
  :group 'reverse-im)

(defcustom reverse-im-read-char-advice-function
  nil
  "Advice `read-char'-like functions if not nil."
  :type '(choice (const :tag "Don't advice" nil)
                 (const :tag "Include mode" reverse-im-read-char-include)
                 (const :tag "Exclude mode" reverse-im-read-char-exclude))
  :group 'reverse-im)

(defcustom reverse-im-read-char-exclude-commands
  '("^avy-.*")
  "List of regexes/commands to match `this-command' to exclude when using `reverse-im-read-char-exclude'."
  :group 'reverse-im
  :type `(repeat (choice regexp symbol)))

(defcustom reverse-im-read-char-include-commands
  '("^mu4e-.*" org-capture org-export-dispatch)
  "List of regexes/commands to match `this-command' to include when using `reverse-im-read-char-include'."
  :group 'reverse-im
  :type `(repeat (choice regexp symbol)))

;;; Storage vars
(defvar reverse-im--default-parent
  nil
  "The default value of `function-key-map' parent keymap.")

(defvar reverse-im--char-fold-include
  nil
  "The default value of `char-fold-include'.")

(defvar reverse-im--keymaps-alist
  nil
  "Alist of pairs input-method/translation keymap.")

;;; Utils
(defun reverse-im--modifiers-combos (modifiers)
  "Return all combinations of MODIFIERS.  See also `reverse-im-modifiers'."
  (seq-reduce (lambda (acc x)
                (append acc
                        (mapcar (apply-partially #'cons x) acc)))
              (seq-uniq modifiers)
              '(nil)))

(defun reverse-im--sanitize-p (layout translation)
  "Check if we should do TRANSLATION for LAYOUT."
  (seq-let (keychar from) translation
    ;; `keychar' and `from' should be different characters
    (and (characterp from) (characterp keychar) (/= from keychar)
         ;; `from' shouldn't be in the `layout'
         (not (member from (append layout nil))))))

(defun reverse-im--add-mods (modifiers key)
  "Generate a single translation binding adding MODIFIERS to KEY."
  (vector (append modifiers (list key))))

;;; Calculate the full translation table
(defun reverse-im--key-def-internal (key-def)
  "Get all reversed translation combos for KEY-DEF."
  (mapcar (lambda (mods)
            (mapcar (apply-partially #'reverse-im--add-mods mods)
                    (reverse key-def)))
          (reverse-im--modifiers-combos reverse-im-modifiers)))

(defun reverse-im--im-to-quail-map (input-method)
  "Get quail map for INPUT-METHOD."
  (with-temp-buffer
    (activate-input-method input-method)
    (when (bufferp quail-completion-buf)
      (kill-buffer quail-completion-buf))
    (and current-input-method
         quail-keyboard-layout
         (cdr (quail-map)))))

(defun reverse-im--to-char (x)
  "Convert X to char, if needed."
  (if (stringp x)
      (string-to-char x)
    x))

(defun reverse-im--normalize-keydef (object)
  "Normalize quail Quail map OBJECT, see `quail-map-p' for format."
  (seq-let (translation &rest alist) object
    (unless (functionp alist)
      (let ((translated (quail-get-translation
                         (car alist)
                         (char-to-string translation) 1)))
        (cond ((and translated (characterp translated))
               (list (list translation translated)))
              ((consp translated)
               (mapcar (lambda (kd)
                         (list translation (reverse-im--to-char kd)))
                       (cdr translated))))))))

;; to test more easily
(defun reverse-im--im-to-pairs (layout input-method)
  "Generate a list of translation pairs for INPUT-METHOD using LAYOUT."
  (thread-last input-method
    reverse-im--im-to-quail-map
    (mapcan #'reverse-im--normalize-keydef)
    (seq-filter (apply-partially #'reverse-im--sanitize-p layout))))


;;; Generate the translation keymap
(defun reverse-im--im-to-keymap (input-method)
  "Translation keymap for INPUT-METHOD, a memoized version."
  ;; alist-get testfn arg appeared in 26.1 so we have to symbolize
  (let ((input-method (intern input-method)))
    (or (alist-get input-method reverse-im--keymaps-alist)
        ;; generate translation pairs
        (let* ((filtered (reverse-im--im-to-pairs quail-keyboard-layout input-method))
               ;; add all modifiers
               (tt (mapcan #'reverse-im--key-def-internal filtered))
               (translation-keymap (make-sparse-keymap)))
          (seq-doseq (translation tt)
            (apply #'define-key translation-keymap translation))
          (add-to-list 'reverse-im--keymaps-alist (cons input-method translation-keymap))
          translation-keymap))))

;;; User-accessible functions

(defun reverse-im-activate (input-method)
  "Activate the reverse mapping for INPUT-METHOD (can be a list).
Example usage: (reverse-im-activate \"ukrainian-computer\")"
  (let* ((input-methods (if (listp input-method)
                            input-method
                          (list input-method)))
         (new-parent (make-composed-keymap
                      (mapcar #'reverse-im--im-to-keymap input-methods)))
         (old-parent (keymap-parent function-key-map)))
    (unless (equal new-parent old-parent)
      (setq reverse-im--default-parent old-parent)
      (set-keymap-parent function-key-map new-parent))))

(defun reverse-im-deactivate (&optional reset)
  "Deactivate translated keymaps.  Optionally RESET `reverse-im--keymaps-alist'."
  (set-keymap-parent function-key-map reverse-im--default-parent)
  (when reset
    (setq reverse-im--keymaps-alist nil)))


;;;###autoload
(defun reverse-im-add-input-method (input-method)
  "Add INPUT-METHOD to `reverse-im-input-methods' list using `customize'."
  (interactive
   (list (read-input-method-name "Translate input method: ")))
  (when input-method
    (add-to-list 'reverse-im-input-methods input-method)
    (customize-save-variable 'reverse-im-input-methods reverse-im-input-methods)))


(declare-function which-key--show-keymap "which-key")

;;;###autoload
(defun reverse-im-which-key-show (input-method)
  "Show translation bindings for INPUT-METHOD using `which-key'."
  (interactive
   (list (read-input-method-name "Translate input method: ")))
  (if (require 'which-key nil t)
      (which-key--show-keymap input-method
                              (reverse-im--im-to-keymap input-method))
    (message "which-key is not installed.")))

;; FIXME calculate translation map even when `reverse-im-mode' is inactive
(defun reverse-im--translation-keymap ()
  "Return a keymap used for translation."
  (keymap-parent function-key-map))

;;; char-folding
(defun reverse-im--generate-char-fold (keymap)
  "Generate a `char-fold' substitutions list for KEYMAP."
  (let ((char-fold))
    (map-keymap (lambda (from value)
                  (when (and (characterp from)
                             (vectorp value))
                    (let* ((fold (mapcar #'string
                                         (seq-filter #'characterp value)))
                           (new-elt (append (list from) fold nil)))
                      (cl-pushnew new-elt char-fold))))
                keymap)
    char-fold))

(defun reverse-im-char-fold-include ()
  "Generate a substitutions list for `char-fold-include'."
  (if-let ((translation-keymap (reverse-im--translation-keymap)))
      (reverse-im--generate-char-fold translation-keymap)
    (message "Keymap is nil, is reverse-im-mode enabled?")
    nil))

(defvar char-fold-include)

(defun reverse-im--char-fold-p ()
  "Check if we have new char-fold.el."
  (and reverse-im-char-fold
       (require 'char-fold nil t)
       (boundp 'char-fold-include)))

;;; read-char hacks
(defun reverse-im--this-command-p (command)
  "Check whether COMMAND does match `this-command'."
  (or (and (symbolp command)
           (eq this-command command))
      (when (and (stringp command)
                 (symbolp this-command))
        (let ((this-command-name (symbol-name this-command)))
          (string-match-p command this-command-name)))))

(defun reverse-im-read-char-include (orig-fun &rest args)
  "An advice for `read-char' compatible ORIG-FUN called with ARGS.
Translate chars only when `this-command' is in `reverse-im-read-char-include-commands'."
  (let ((res (apply orig-fun args)))
    (if (seq-some #'reverse-im--this-command-p reverse-im-read-char-include-commands)
        (reverse-im--translate-char res t)
      res)))

(defun reverse-im-read-char-exclude (orig-fun &rest args)
  "An advice for `read-char' compatible ORIG-FUN called with ARGS.
Translate all chars, unless `this-command' is not in `reverse-im-read-char-exclude-commands'."
  (let ((res (apply orig-fun args)))
    (if (seq-some #'reverse-im--this-command-p reverse-im-read-char-exclude-commands)
        res
      (reverse-im--translate-char res t))))


;;;###autoload
(define-minor-mode reverse-im-mode
  "Toggle reverse-im mode."
  :init-value nil
  :global t
  (if reverse-im-mode
      (progn
        (reverse-im-activate reverse-im-input-methods)
        (when (reverse-im--char-fold-p)
          (setq reverse-im--char-fold-include char-fold-include)
          (customize-set-variable 'char-fold-include
                                  (append char-fold-include
                                          (reverse-im-char-fold-include))))
        (when reverse-im-read-char-advice-function
          (advice-add #'read-char :around reverse-im-read-char-advice-function)
          (advice-add #'read-char-exclusive :around reverse-im-read-char-advice-function)))
    (reverse-im-deactivate t)
    (when (reverse-im--char-fold-p)
      (customize-set-variable 'char-fold-include reverse-im--char-fold-include))
    (when reverse-im-read-char-advice-function
      (advice-remove 'read-char reverse-im-read-char-advice-function)
      (advice-remove 'read-char-exclusive reverse-im-read-char-advice-function))))

;;; Translation functions

(defun reverse-im--translate-char-internal (keymap c strict)
  "Try to translate C using KEYMAP.  Set STRICT if reverse translation is not needed."
  (let ((to))
    (map-keymap (lambda (from value)
                  (if (= c from)
                      (let ((v (aref value 0)))
                        (when (characterp v)
                          (setq to v)))
                    (and (not strict)
                         (member c (append value nil))
                         (characterp from)
                         (setq to from))))
                keymap)
    (or to c)))

(defun reverse-im--translate-char (c &optional strict)
  "Try to translate C using active translation.  Set STRICT if reverse translation is not needed."
  (and c
       (if-let ((translation-keymap (reverse-im--translation-keymap)))
           (reverse-im--translate-char-internal translation-keymap c strict)
         (message "Keymap is nil, is reverse-im-mode enabled?")
         c)))

(defun reverse-im-translate-string (s)
  "Translate string S using active translation keymap."
  (apply #'string
         (mapcar #'reverse-im--translate-char s)))


;;; Interactive translating
;; loosely based on `cider--format-region'
;; TODO: prefix argument to store selection
;;;###autoload
(defun reverse-im-translate-region (start end &optional force)
  "Translate active region from START to END.  FORCE translate even if the region isn't active."
  (interactive "r")
  (when (or (region-active-p)
            force)
    (let* ((original (buffer-substring-no-properties start end))
           (translated (reverse-im-translate-string original)))
      (unless (equal original translated)
        (let* ((pos (point)))
          (delete-region start end)
          (insert translated)
          (goto-char pos))))))

;; loosely based on `transpose-subr'
(defun reverse-im--translate-subr (mover arg)
  "Subroutine to do the work of translating objects.
Works for lines, sentences, paragraphs, etc.  MOVER is a function that
moves forward by units of the given object (e.g. `forward-sentence',
`forward-paragraph').  If ARG is an integer, moves the
current object past ARG following (if ARG is positive) or
preceding (if ARG is negative) objects, leaving point after the
current object."
  (let* ((pos1 (point))
         (_ (funcall mover arg))
         (pos2 (point))
         (start (min pos1 pos2))
         (end (max pos1 pos2))
         (original (buffer-substring-no-properties start end))
         (translated (reverse-im-translate-string original)))
    (unless (equal original translated)
      (delete-region start end)
      (insert translated)
      (goto-char pos1))))

;;;###autoload
(defun reverse-im-translate-word (arg)
  "Translate word before the point.  With prefix ARG translates ARG words instead of the last one, if ARG is 0 - translate until the beginning of line."
  (interactive "p")
  (if (zerop arg)
      (reverse-im--translate-subr #'move-beginning-of-line 1)
    (reverse-im--translate-subr #'backward-word arg)))


;; Avy action
(when (and reverse-im-avy-action-char
           (require 'avy nil t))
  (defvar avy-command)
  (defvar avy-dispatch-alist)

  (defun reverse-im-avy-action-translate (pt)
    "Auto translate word at PT."
    (save-excursion
      (goto-char pt)
      (cond
       ((eq avy-command 'avy-goto-line)
        (reverse-im-translate-region (line-beginning-position) (line-end-position) t))
       ((looking-at-p "\\b")
        (reverse-im--translate-subr #'forward-word 1))
       (t (progn
            (backward-word)
            (when (looking-at-p "\\b")
              (reverse-im--translate-subr #'forward-word 1)))))))

  (cl-pushnew (cons reverse-im-avy-action-char 'reverse-im-avy-action-translate) avy-dispatch-alist))

(provide 'reverse-im)

;;; reverse-im.el ends here
