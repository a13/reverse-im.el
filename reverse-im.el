;;; reverse-im.el --- Reverse mapping for keyboard layouts other than english. -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "24.4"))
;; Keywords: input method
;; Homepage: https://github.com/a13/reverse-im.el

;;; Commentary:
;; Overrides `function-key-map' for preferred input-method to translate input sequences
;; to english, so we can use Emacs bindings while non-default system layout is active.
;; Example usage: (reverse-im-activate "russian-computer")

;; Main idea and some code taken from http://ru-emacs.livejournal.com/82428.html

;;; Code:

(require 'quail)
(require 'cl-extra)
(require 'cl-macs)

(defvar reverse-im--keymaps-alist
  nil
  "Alist of pairs input-method/translation keymap.")

(defvar reverse-im-modifiers
  '(control meta)
  "List of modifiers to translate with.")

(defun reverse-im--modifiers-combos (mlist)
  "All combinations of modifiers from MLIST."
  (pcase mlist
    (`(,head . ,tail)
     (let* ((s (reverse-im--modifiers-combos tail))
            (v (mapcar (lambda (x) (cons head x)) s)))
       (append s v)))
    ('() '(nil))))

(defun reverse-im--activate-key-def (keymap kd)
  "Add to KEYMAP KD key/definition list."
  (when kd
    (cl-destructuring-bind (key def) kd
      (define-key keymap key def))))

(defun reverse-im--key-def (map mod)
  "Return a list of last two arguments for `define-key' for MAP with MOD modifier."
  (cl-destructuring-bind (keychar def) map
    (let ((from (quail-get-translation def (char-to-string keychar) 1)))
      (and (characterp from) (characterp keychar) (not (= from keychar))
           (list
            (vector (append mod (list from)))
            (vector (append mod (list keychar))))))))

(defun reverse-im--translation-table (input-method)
  "Generate a translation table for INPUT-METHOD."
  (with-temp-buffer
    (activate-input-method input-method)
    (when (and current-input-method quail-keyboard-layout)
      (cl-mapcan
       (lambda (map)
         (mapcar
          (apply-partially #'reverse-im--key-def map)
          (reverse-im--modifiers-combos reverse-im-modifiers)))
       (cdr (quail-map))))))

(defun reverse-im--im-to-keymap (input-method)
  "Translation keymap for INPUT-METHOD."
  (or (alist-get input-method reverse-im--keymaps-alist nil)
      (let ((new-keymap (make-sparse-keymap)))
        (mapc (apply-partially #'reverse-im--activate-key-def new-keymap)
              (reverse-im--translation-table input-method))
        (add-to-list 'reverse-im--keymaps-alist `(,input-method . ,new-keymap))
        new-keymap)))

(defun reverse-im-activate (input-method)
  "Activate the reverse mapping for INPUT-METHOD.
Example usage: (reverse-im-activate \"russian-computer\")"
  (set-keymap-parent function-key-map (reverse-im--im-to-keymap input-method)))

(defun reverse-im-deactivate (&optional reset)
  "Deactivate translated keymaps.  Optionally RESET `reverse-im--keymaps-alist'."
  (set-keymap-parent function-key-map nil)
  (when reset
    (setq reverse-im--keymaps-alist nil)))


(provide 'reverse-im)

;;; reverse-im.el ends here
(reverse-im-deactivate)
