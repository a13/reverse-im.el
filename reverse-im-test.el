(require 'reverse-im)
(require 'seq)

(defun seq-set-equal-internal-p (s1 s2)
  (seq-set-equal-p s1 s2 'equal))

(defun seq-set-equal-deep-p (ss1 ss2)
  (seq-set-equal-p
   ss1 ss2
   (lambda (s1 s2)
     (seq-set-equal-p s1 s2 'equal))))

(ert-deftest reverse-im--modifiers-combos-test ()
  (should
   (seq-set-equal-deep-p
    (reverse-im--modifiers-combos '(meta control))
    '(nil (meta) (control) (control meta))))
  (should
   (seq-set-equal-deep-p
    (reverse-im--modifiers-combos '(control meta))
    '(nil (meta) (control) (control meta))))
  ;; FIXME:
  ;; (should
  ;;  (null
  ;;   (reverse-im--modifiers-combos '())
  ;;   '())) ;or '(nil?)
  ;; (should
  ;;  (null
  ;;   (reverse-im--modifiers-combos '(control control))
  ;;   '(nil (control))))
  (should
   (equal
    (reverse-im--modifiers-combos '(control))
    '(nil (control)))))

(ert-deftest reverse-im--key-def-internal-test ()
  (let ((reverse-im-modifiers '(control)))
    (seq-set-equal-deep-p
     (reverse-im--key-def-internal 113 1093)
     '(([(1093)] [(113)]) ([(control 1093)] [(control 113)]))))
  ;; already in default layout
  (let ((reverse-im-modifiers '(control))
        (quail-keyboard-layout '(1093)))
    (null
     (reverse-im--key-def-internal 113 1093)))
  ;; the same
  (let ((reverse-im-modifiers '(control)))
    (null
     (reverse-im--key-def-internal 1093 1093))))
