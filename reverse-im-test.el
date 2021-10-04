(require 'reverse-im)
(require 'seq)

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
   (or (string= "26.3" emacs-version)
       (seq-set-equal-deep-p
        (reverse-im--modifiers-combos '(control meta))
        '(nil (meta) (control) (control meta)))))
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
  (should (let ((reverse-im-modifiers '(control)))
            (seq-set-equal-deep-p
             (reverse-im--key-def-internal 113 1093)
             '(([(1093)] [(113)]) ([(control 1093)] [(control 113)])))))
  ;; already in default layout
  (should (let ((reverse-im-modifiers '(control))
                (quail-keyboard-layout '(1093)))
            (null
             (reverse-im--key-def-internal 113 1093))))
  ;; the same
  (should (let ((reverse-im-modifiers '(control)))
            (null
             (reverse-im--key-def-internal 1093 1093))))
  ;; not chars
  (should (let ((reverse-im-modifiers '(control)))
            (null
             (reverse-im--key-def-internal 'foo 'bar)))))


;; keychar 124 def ((0 0 0 0 nil) . [124 |]) skip nil
;; keychar 61 def [=] skip nil
;; keychar 113 def [х] skip nil

(ert-deftest reverse-im--key-def-test ()
  (should (null (reverse-im--key-def '(124 ((0 0 0 0 nil) . [124 |])))))
  (should
   (null
    (let ((reverse-im-modifiers '(control)))
      (reverse-im--key-def '(61 (61))))))
  (should
   (seq-set-equal-deep-p
    (let ((reverse-im-modifiers '(control)))
      (reverse-im--key-def '(61 (1230))))
    '(([(1230)] [(61)]) ([(control 1230)] [(control 61)])))))
