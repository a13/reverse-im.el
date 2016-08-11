;;; reverse-im.el --- Reverse mapping for keyboard layouts other than english

;; Package-Requires: ((emacs "24.4"))
;; Keywords: input method
;; Homepage: https://github.com/a13/reverse-im.el

;;; Commentary:
;; Creates translation mappings for input-methods other than default english one,
;; so all (?) keybindings are usable while non-default system layout is active.
;; Example usage: (activate-reverse-im "russian-computer")

;; Main idea and code taken from http://ru-emacs.livejournal.com/82428.html

;;; Code:

(require 'quail)

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (when (and input-method (symbolp input-method))
    (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defmacro activate-reverse-im (im)
  "Activates the reverse mapping for IM input method.
If daemon mode is active, adds hook to `after-make-frame-functions`
Example usage: (activate-reverse-im \"russian-computer\")"
  (if (not (daemonp))
      (reverse-input-method im)
    (let ((hname (intern (format "reverse-%s" im))))
      `(progn
         (defun ,hname (f)
           (lexical-let ((frame f))
             (run-at-time nil nil
                          #'(lambda () (unless (and (daemonp) (eq frame terminal-frame))
                                         (reverse-input-method ,im))))))
         (add-hook 'after-make-frame-functions #',hname)))))

(defun read-passwd-override-keymap (orig-fun &rest args)
  "Override `read-passwd` keymap."
  (let ((local-function-key-map nil)
        (read-passwd-map (let ((map read-passwd-map))
                           (set-keymap-parent map minibuffer-local-map)
                           (define-key map [return] #'exit-minibuffer)
                           (define-key map [backspace] #'delete-backward-char)
                           map)))
    (apply orig-fun args)))

(advice-add 'read-passwd :around #'read-passwd-override-keymap)


(provide 'reverse-im)

;;; reverse-im.el ends here
