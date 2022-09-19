(require 'reverse-im)
(require 'seq)

(defun seq-set-equal-deep-p (ss1 ss2)
  (seq-set-equal-p
   ss1 ss2
   (lambda (s1 s2)
     (seq-set-equal-p s1 s2 'equal))))

(ert-deftest reverse-im--modifiers-combos-test ()
  (if (string= "26.3" emacs-version)
      (should (equal (reverse-im--modifiers-combos '(control meta))
                     '(nil (control) (meta) (meta control))))
    (should
     (seq-set-equal-deep-p
      (reverse-im--modifiers-combos '(meta control))
      '(nil (meta) (control) (control meta)))))
  (should
   (equal
    (reverse-im--modifiers-combos '())
    '(nil)))
  (should
   (equal
    (reverse-im--modifiers-combos '(control control))
    '(nil (control))))
  (should
   (equal
    (reverse-im--modifiers-combos '(control))
    '(nil (control)))))

(ert-deftest reverse-im--key-def-internal-test ()
  (should (let ((reverse-im-modifiers '(control)))
            (seq-set-equal-deep-p
             (reverse-im--key-def-internal '(113 1093))
             '(([(1093)] [(113)]) ([(control 1093)] [(control 113)]))))))

(ert-deftest reverse-im--normalize-keydef-test ()
  (should (equal (reverse-im--normalize-keydef '(124 ((0 0 0 0 nil) . [124 |])))
                 '((124 124) (124 |))))
  (should (equal (reverse-im--normalize-keydef '(61 (61)))
                 '((61 61))))
  (should (equal (reverse-im--normalize-keydef '(61 (1023)))
                 '((61 1023)))))

(ert-deftest reverse-im--im-to-pairs-test ()
  (should
   (equal
    (reverse-im--im-to-pairs "russian-computer")
    '((62 1070) (60 1041) (77 1068) (78 1058) (66 1048) (86 1052) (67 1057)
      (88 1063) (90 1071) (34 1069) (58 1046) (76 1044) (75 1051) (74 1054)
      (72 1056) (71 1055) (70 1040) (68 1042) (83 1067) (65 1060) (125 1066)
      (123 1061) (80 1047) (79 1065) (73 1064) (85 1043) (89 1053) (84 1045)
      (82 1050) (69 1059) (87 1062) (81 1049) (126 1025) (35 8470) (46 1102)
      (44 1073) (109 1100) (110 1090) (98 1080) (118 1084) (99 1089) (120 1095)
      (122 1103) (39 1101) (59 1078) (108 1076) (107 1083) (106 1086) (104 1088)
      (103 1087) (102 1072) (100 1074) (115 1099) (97 1092) (93 1098) (91 1093)
      (112 1079) (111 1097) (105 1096) (117 1075) (121 1085) (116 1077) (114 1082)
      (101 1091) (119 1094) (113 1081) (96 1105)))))

(ert-deftest reverse-im-sanity-test ()
  (let ((reverse-im--keymaps-alist nil))
    (should (mapcar (lambda (m)
                      (reverse-im--im-to-pairs (car m)))
                    input-method-alist))
    (should (mapcar (lambda (m)
                      (reverse-im--im-to-keymap (car m)))
                    input-method-alist))))
