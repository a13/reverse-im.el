;;; reverse-im.el --- Reverse mapping for keyboard layouts other than english. -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "24.4"))
;; Keywords: input method
;; Homepage: https://github.com/a13/reverse-im.el

;;; Commentary:
;; Creates translation mappings for input-methods other than default english one,
;; so all (?) keybindings are usable while non-default system layout is active.
;; Example usage: (reverse-im-activate "russian-computer")

;; Main idea and some code taken from http://ru-emacs.livejournal.com/82428.html

;;; Code:

(require 'quail)
(require 'cl-extra)
(require 'cl-macs)

(defvar reverse-im-im-list
  nil
  "List of input methods to activate on new frame creation.")

(defun reverse-im--key-def (map mod)
  "Return a list of last two arguments for `define-key' for MAP with MOD modifier."
  (pcase map
    (`(,keychar ,def)
     (let ((from (quail-get-translation def (char-to-string keychar) 1)))
       (and (characterp from) (characterp keychar)
            (list
             (vector (append mod (list from)))
             (vector (append mod (list keychar)))))))))

;; TODO: memoize
(defun reverse-im--translation-table (input-method)
  "Generate a translation table for INPUT-METHOD."
  (with-temp-buffer
    (activate-input-method input-method)
    (when (and current-input-method quail-keyboard-layout)
      (cl-mapcan
       (lambda (map)
         (mapcar
          (apply-partially #'reverse-im--key-def map)
          (list nil '(control) '(meta) '(control meta))))
       (cdr (quail-map))))))

(defun reverse-im--activate-key-def (kd)
  "Activate KD key/definition list."
  (when kd
    (cl-destructuring-bind (key def) kd
      (define-key local-function-key-map key def))))

(defun reverse-im-activate-im (input-method)
  "Activate INPUT-METHOD."
  (mapc #'reverse-im--activate-key-def (reverse-im--translation-table input-method)))

(defun reverse-im-frame-hook (f)
  "Create reversed input method for F frame."
  (let ((frame f))
    (run-at-time nil nil
                 #'(lambda ()
                     (unless (and (daemonp) (eq frame terminal-frame))
                       (mapc #'reverse-im-activate-im reverse-im-im-list))))))

(defun reverse-im-read-passwd-override-keymap (orig-fun &rest args)
  "Override `read-passwd` keymap."
  (let ((local-function-key-map nil)
        (read-passwd-map (let ((map read-passwd-map))
                           (set-keymap-parent map minibuffer-local-map)
                           (define-key map [return] #'exit-minibuffer)
                           (define-key map [backspace] #'delete-backward-char)
                           map)))
    (apply orig-fun args)))

(defun reverse-im-activate (input-method)
  "Activates the reverse mapping for INPUT-METHOD.
If daemon mode is active, adds hook to `after-make-frame-functions`
Example usage: (reverse-im-activate \"russian-computer\")"
  (when (daemonp)
    (cl-pushnew input-method reverse-im-im-list :test #'equal)
    (add-hook 'after-make-frame-functions #'reverse-im-frame-hook))
  (advice-add 'read-passwd :around #'reverse-im-read-passwd-override-keymap)
  (reverse-im-activate-im input-method))

(defun reverse-im-deactivate (&optional input-method)
  "Deactivate(partially) INPUT-METHOD.
Remove INPUT-METHOD from `reverse-im-im-list`,
deactivate hook `reverse-im-frame-hook`
remove advice `reverse-im-read-passwd-override-keymap`."
  (when input-method
    (remove input-method reverse-im-im-list))
  (remove-hook 'after-make-frame-functions #'reverse-im-frame-hook)
  (advice-remove 'read-passwd  #'reverse-im-read-passwd-override-keymap))

(provide 'reverse-im)

;;; reverse-im.el ends here
