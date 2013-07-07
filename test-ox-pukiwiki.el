;;; test-ox-pukiwiki --- Tests for ox-pukiwiki.el

;; Copyright (C) 2012, 2013  Nicolas Goaziou

;; Author: Yasushi SHOJI <yasushi.shoji at gmail dot com>

;;; Code:

(require 'ox-pukiwiki)

(unless (featurep 'ox)
  (signal 'missing-test-dependency "org-export"))

(ert-deftest test-ox-pukiwiki/headline ()
  "Test Headline conversion."
  (should
   (string=
    "* H1\n** H2\n** H3\n"
    (org-test-with-parsed-data "* H1\n** H2\n** H3"
      (org-test-with-backend pw
	(org-export-as 'pw nil nil nil nil))))))

(ert-deftest test-ox-pukiwiki/plain-list ()
  "Test plain list conversion."

  ;; simple list
  (org-test-with-temp-text "- a\n- b"
    (org-test-with-backend 'pukiwiki
      (should
       (string-match "- a\n- b"
	(org-export-as 'pukiwiki nil nil nil nil)))))

  ;; second level list
  (org-test-with-temp-text "
- list 1
  - list 2
"
    (org-test-with-backend pukiwiki
      (should
       (string-match "- list 1
-- list 2"
	(org-export-as 'pukiwiki nil nil nil nil)))))

)

(provide 'test-ox-pukiwiki)
;;; test-ox-pukiwiki.el end here
