;;; reverse-im.el --- Reverse mapping for keyboard layouts other than english. -*- lexical-binding: t -*-

;; Authors: Juri Linkov <juri@jurta.org> (initial idea), Dmitry K. (packager and maintainer)
;; Package-Requires: ((emacs "24.4"))
;; Keywords: input method
;; Homepage: https://github.com/a13/reverse-im.el

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
;; Overrides `function-key-map' for preferred input-method to translate input sequences
;; to english, so we can use Emacs bindings while non-default system layout is active.
;; Example usage: (reverse-im-activate "russian-computer")

;;; Code:

(require 'quail)
(require 'cl-extra)
(require 'cl-macs)
(require 'cl-seq)

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
  '(control meta)
  "List of modifiers to translate with."
  :type '(repeat symbol)
  :group 'reverse-im)

(defvar reverse-im--keymaps-alist
  nil
  "Alist of pairs input-method/translation keymap.")

(defun reverse-im--modifiers-combos (mlist)
  "All combinations of modifiers from MLIST."
  (pcase mlist
    (`(,head . ,tail)
     (let* ((s (reverse-im--modifiers-combos tail))
            (v (mapcar (lambda (x) (cons head x)) s)))
       (append s v)))
    (`() '(nil))))

(defun reverse-im--activate-key-def (keymap kd)
  "Add to KEYMAP KD key/definition list."
  (when kd
    (cl-destructuring-bind (key def) kd
      (define-key keymap key def))))

(defun reverse-im--key-def (map)
  "Return a list of last two arguments for `define-key' for MAP with MOD modifier."
  (pcase map
    (`(,keychar ,def)
     (let ((from (quail-get-translation def (char-to-string keychar) 1)))
       (and (characterp from) (characterp keychar) (not (= from keychar))
            ;; don't translate if the char is in default layout
            (not (cl-position from quail-keyboard-layout))
            (mapcar
             (lambda (mod)
               (list
                (vector (append mod (list from)))
                (vector (append mod (list keychar)))))
             (reverse-im--modifiers-combos reverse-im-modifiers)))))
    (_ nil)))

(defun reverse-im--translation-table (input-method)
  "Generate a translation table for INPUT-METHOD."
  (prog1
      (with-temp-buffer
        (activate-input-method input-method)
        (when (and current-input-method quail-keyboard-layout)
          (cl-mapcan #'reverse-im--key-def (cdr (quail-map)))))
    (when (bufferp quail-completion-buf)
      (kill-buffer quail-completion-buf))))


(defun reverse-im--im-to-keymap (input-method)
  "Translation keymap for INPUT-METHOD."
  (let ((im-sym (intern input-method)))
    (or (alist-get im-sym reverse-im--keymaps-alist nil)
        (let ((new-keymap (make-sparse-keymap)))
          (mapc (apply-partially #'reverse-im--activate-key-def new-keymap)
                (reverse-im--translation-table input-method))
          (add-to-list 'reverse-im--keymaps-alist `(,im-sym . ,new-keymap))
          new-keymap))))

(defun reverse-im-activate (input-method)
  "Activate the reverse mapping for INPUT-METHOD.
Example usage: (reverse-im-activate \"russian-computer\")"
  (set-keymap-parent function-key-map (reverse-im--im-to-keymap input-method)))

(defun reverse-im-deactivate (&optional reset)
  "Deactivate translated keymaps.  Optionally RESET `reverse-im--keymaps-alist'."
  (set-keymap-parent function-key-map nil)
  (when reset
    (setq reverse-im--keymaps-alist nil)))

(defun reverse-im-add-input-method (input-method)
  "Add INPUT-METHOD to `reverse-im-input-methods list'."
  (interactive
   (list (read-input-method-name "Translate input method: ")))
  (when input-method
    (add-to-list 'reverse-im-input-methods input-method)
    (customize-save-variable 'reverse-im-input-methods reverse-im-input-methods)))

(define-minor-mode reverse-im-mode
  "Toggle reverse-im mode."
  :init-value nil
  :global t
  (if reverse-im-mode
      (progn
        (when (null reverse-im-input-methods)
          (call-interactively #'reverse-im-add-input-method))
        (mapc #'reverse-im-activate reverse-im-input-methods))
    (reverse-im-deactivate t)))


(provide 'reverse-im)

;;; reverse-im.el ends here
