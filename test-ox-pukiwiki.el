;;; test-ox-pukiwiki --- Tests for ox-pukiwiki.el

;; Copyright (C) 2012, 2013  Nicolas Goaziou

;; Author: Yasushi SHOJI <yasushi.shoji at gmail dot com>

;;; Code:
(require 'org-test)
(require 'ox-pukiwiki)

(unless (featurep 'ox)
  (signal 'missing-test-dependency "org-export"))

(defun org-pukiwiki-test-transcode-body (str1 str2)
  (should (equal (org-test-with-temp-text str1
                                          (org-export-as 'pukiwiki nil nil t))
		 str2)))

;;; Heading
(ert-deftest test-ox-pukiwiki/headline ()
  "Test Headline conversion."
  (org-pukiwiki-test-transcode-body "
* H1
** H2
** H3"

   "\n* H1

** H2

** H3
"))

;;; List
(ert-deftest test-ox-pukiwiki/plain-list ()
  "Test plain list conversion."

  ;; simple list
  (org-pukiwiki-test-transcode-body
   "- a\n- b"
   "- a\n- b\n")

  ;; second level list
  (org-pukiwiki-test-transcode-body
   "
- list 1
  - list 2
"

"- list 1
-- list 2\n"))

(provide 'test-ox-pukiwiki)
;;; test-ox-pukiwiki.el end here
