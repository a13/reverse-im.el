;;; reverse-im.el --- Reverse mapping for non-default system layouts -*- lexical-binding: t -*-

;; Authors: Juri Linkov <juri@jurta.org> (initial idea), Dmitry K. (packager and maintainer)
;; Package-Requires: ((emacs "25.1"))
;; Keywords: i18n
;; Homepage: https://github.com/a13/reverse-im.el
;; Version: 0.0.5

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Overrides `function-key-map' parent for preferred input-method
;; to translate input sequences the default system layout (english)
;; so we can use Emacs bindings while non-default system layout is active.

;; Usage example:
;; (use-package reverse-im
;;   :ensure t
;;   :custom
;;   (reverse-im-input-methods '("russian-computer")) ; use your input-method(s) here
;;   :config
;;   (reverse-im-mode t))

;; or, alternatively, add the library to your load-path and
;; (reverse-im-activate "russian-computer")

;;; Code:

(require 'quail)
(require 'cl-extra)
(require 'cl-lib)

(declare-function which-key--show-keymap "which-key")

;;; Customs
(defgroup reverse-im nil
  "Translate input methods."
  :group 'I18n)

(defcustom reverse-im-input-methods
  nil
  "List of input methods to activate when minor-mode is on."
  :group 'reverse-im
  :set #'(lambda (symbol value)
           (set-default symbol value)
           (reverse-im-activate value))
  :type `(repeat (choice (const nil)
                         mule-input-method-string)))

(defcustom reverse-im-modifiers
  '(control meta super)
  "List of modifiers to translate with."
  :type '(repeat symbol)
  :group 'reverse-im)

;;; Storage vars
(defvar reverse-im--default-parent
  nil
  "The default value of the `function-key-map' parent keymap.")

(defvar reverse-im--keymaps-alist
  nil
  "Alist of pairs input-method/translation keymap.")

;;; Utils
(cl-defun reverse-im--modifiers-combos ((head . tail))
  "All combinations of modifiers from the list argument."
  (let* ((s (if tail
                (reverse-im--modifiers-combos tail)
              '(())))
         (v (mapcar (apply-partially #'cons head) s)))
    (append s v)))

(defun reverse-im--to-char (x)
  "Convert X to char, if needed."
  (if (stringp x)
      (string-to-char x)
    x))

;;; Calculate the full translation table
(defun reverse-im--key-def-internal (keychar from)
  "Get all translating combos from FROM to KEYCHAR."
  (and (characterp from) (characterp keychar) (not (= from keychar))
       ;; don't translate if the char is in default layout
       (not (cl-position from quail-keyboard-layout))
       (mapcar
        (lambda (mod)
          `([,(append mod (list from))]
            [,(append mod (list keychar))]))
        (reverse-im--modifiers-combos reverse-im-modifiers))))

(cl-defun reverse-im--key-def ((keychar def &rest _skip))
  "Return a list of `define-key' '(key def) arguments for quail KEYCHAR and DEF."
  (let ((translation (quail-get-translation def (char-to-string keychar) 1)))
    (cond ((and translation (characterp translation))
           (reverse-im--key-def-internal keychar translation))
          ((consp translation)
           (mapcan (apply-partially #'reverse-im--key-def-internal keychar)
                   (mapcar #'reverse-im--to-char (cdr translation)))))))

(defun reverse-im--translation-table (input-method)
  "Generate a translation table for INPUT-METHOD."
  (with-temp-buffer
    (activate-input-method input-method)
    (when (bufferp quail-completion-buf)
      (kill-buffer quail-completion-buf))
    (when (and current-input-method quail-keyboard-layout)
      (cl-mapcan #'reverse-im--key-def (cdr (quail-map))))))

;;; Generate the translation keymap
(defun reverse-im--im-to-keymap-internal (input-method)
  "Generate a keymap for INPUT-METHOD."
  (let ((new-keymap (make-sparse-keymap))
        (tt (reverse-im--translation-table input-method)))
    (cl-dolist (translation tt new-keymap)
      (apply #'define-key new-keymap translation))))

(defun reverse-im--im-to-keymap (input-method)
  "Translation keymap for INPUT-METHOD, a memoized version of the previous one."
  ;; alist-get testfn arg appeared in 26.1 so we have to symbolize
  (let ((input-method (intern input-method)))
    (or (alist-get input-method reverse-im--keymaps-alist)
        (let ((new-keymap (reverse-im--im-to-keymap-internal input-method)))
          (add-to-list 'reverse-im--keymaps-alist (cons input-method new-keymap))
          new-keymap))))

;;; User-accessible functions

(defun reverse-im-activate (input-method)
  "Activate the reverse mapping for INPUT-METHOD (can be a list).
Example usage: (reverse-im-activate \"russian-computer\")"
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

;;;###autoload
(defun reverse-im-which-key-show (input-method)
  "Show translation bindings for INPUT-METHOD using `which-key'."
  (interactive
   (list (read-input-method-name "Translate input method: ")))
  (if (require 'which-key nil t)
      (which-key--show-keymap input-method
                              (reverse-im--im-to-keymap input-method))
    (message "which-key is not installed.")))

;;;###autoload
(define-minor-mode reverse-im-mode
  "Toggle reverse-im mode."
  :init-value nil
  :global t
  (if reverse-im-mode
      (reverse-im-activate reverse-im-input-methods)
    (reverse-im-deactivate t)))

;;; read-char hack
;; use like (advice-add 'read-char-exclusive :around #'reverse-im-read-char)

(defun reverse-im--translate-char (c)
  "Try to translate C using active translation keymap."
  (let ((to c))
    (map-keymap #'(lambda (type value)
                    (when (= c type)
                      (setq to (aref value 0))))
                (keymap-parent function-key-map))
    to))

(defun reverse-im-read-char (orig-fun &rest args)
  "An advice for `read-char' compatible ORIG-FUN called with ARGS."
  (let ((res (apply orig-fun args)))
    (reverse-im--translate-char res)))


;;; char-folding
(defun reverse-im-char-fold-include ()
  "Generate a substitutions list for `char-fold-include'."
  (let ((char-fold '()))
    (map-keymap
     #'(lambda (from value)
         (when (and (characterp from)
                    (vectorp value))
           (let* ((fold (mapcar #'string
                                (cl-remove-if-not #'characterp value)))
                  (new-elt (append (list from) fold nil)))
             (cl-pushnew new-elt char-fold))))
     (keymap-parent function-key-map))
    char-fold))

(provide 'reverse-im)

;;; reverse-im.el ends here
