(require 'reverse-im)
(require 'cl-lib)

(ert-deftest reverse-im--modifiers-combos-test ()
  (should
   (null
    (cl-set-difference
     (reverse-im--modifiers-combos '(control meta super))
     '(nil (super) (meta) (meta super) (control)
           (control super) (control meta) (control meta super))
     :test 'equal))))
