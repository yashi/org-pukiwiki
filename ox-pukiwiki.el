;;; ox-pukiwiki.el --- Pukiwiki Back-End for Org Export Engine

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Yasushi SHOJI <yasushi.shoji@gmail.com>
;; Keywords: org, pukiwiki

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an Pukiwiki back-end for Org exporter.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-pukiwiki-export-as-pukiwiki' (temporary buffer) and
;; `org-pukiwiki-export-to-pukiwiki' (file).

;;; Code:
(require 'ox)

(defgroup org-export-pukiwiki nil
  "Options for exporting Org mode files to Pukiwiki."
  :tag "Org Export Pukiwiki"
  :group 'org-export)

(org-export-define-backend 'pukiwiki
  '((babel-call . org-pukiwiki-identity)
    (bold . org-pukiwiki-identity)
    (center-block . org-pukiwiki-identity)
    (clock . org-pukiwiki-identity)
    (code . org-pukiwiki-identity)
    (comment . (lambda (&rest args) ""))
    (comment-block . (lambda (&rest args) ""))
    (diary-sexp . org-pukiwiki-identity)
    (drawer . (lambda (&rest args) ""))
    (dynamic-block . org-pukiwiki-identity)
    (entity . org-pukiwiki-identity)
    (example-block . org-pukiwiki-identity)
    (fixed-width . org-pukiwiki-identity)
    (footnote-definition . org-pukiwiki-identity)
    (footnote-reference . org-pukiwiki-identity)
    (headline . org-pukiwiki-headline)
    (horizontal-rule . org-pukiwiki-identity)
    (inline-babel-call . org-pukiwiki-identity)
    (inline-src-block . org-pukiwiki-identity)
    (inlinetask . org-pukiwiki-identity)
    (italic . org-pukiwiki-identity)
    (item . org-pukiwiki-item)
    (keyword . org-pukiwiki-keyword)
    (latex-environment . org-pukiwiki-identity)
    (latex-fragment . org-pukiwiki-identity)
    (line-break . org-pukiwiki-identity)
    (link . org-pukiwiki-identity)
    (node-property . org-pukiwiki-identity)
    (paragraph . org-pukiwiki-identity)
    (plain-list . org-pukiwiki-plain-list)
    (planning . org-pukiwiki-identity)
    (property-drawer . (lambda (&rest args) ""))
    (quote-block . org-pukiwiki-identity)
    (quote-section . org-pukiwiki-identity)
    (radio-target . org-pukiwiki-identity)
    (section . org-pukiwiki-identity)
    (special-block . org-pukiwiki-identity)
    (src-block . org-pukiwiki-identity)
    (statistics-cookie . org-pukiwiki-identity)
    (strike-through . org-pukiwiki-identity)
    (subscript . org-pukiwiki-identity)
    (superscript . org-pukiwiki-identity)
    (table . org-pukiwiki-table)
    (table-cell . org-pukiwiki-table-cell)
    (table-row . org-pukiwiki-table-row)
    (target . org-pukiwiki-identity)
    (timestamp . org-pukiwiki-identity)
    (underline . org-pukiwiki-identity)
    (verbatim . org-pukiwiki-identity)
    (verse-block . org-pukiwiki-identity))
  :filters-alist '((:filter-final-output . org-pukiwiki-final-function))
  :menu-entry
  '(?p "Export to Pukiwiki"
       ((?p "As Pukiwiki buffer"
	    (lambda (a s v b) (org-pukiwiki-export-as-pukiwiki a s v)))
	(?P "As Pukiwiki file"
	    (lambda (a s v b) (org-pukiwiki-export-to-pukiwiki a s v)))
	(?o "As Pukiwiki file and open"
	    (lambda (a s v b)
	      (if a (org-pukiwiki-export-to-pukiwiki t s v)
		(org-open-file (org-pukiwiki-export-to-pukiwiki nil s v))))))))

(defun org-pukiwiki-identity (blob contents info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (org-export-expand blob contents))

(defun org-pukiwiki-plain-list (blob contents info)
  (setq contents (replace-regexp-in-string "^\\+" "++" contents))
  (replace-regexp-in-string "^-" "--" contents))

(defun org-pukiwiki-item (blob contents info)
  (let* ((parent (org-export-get-parent blob))
	 (type (org-element-property :type parent))
	 (bullet (cond
		  ((eq type 'unordered) "-")
		  ((eq type 'ordered) "+"))))
    (concat bullet " " contents)))

(defun org-pukiwiki-final-function (contents backend info)
  ;; remove extra "-" and "+" generated from org-pukiwiki-plain-list
  ;; and org-pukiwiki-item
  (setq contents (replace-regexp-in-string "^--" "-" contents))
  (setq contents (replace-regexp-in-string "^\\+\\+" "+" contents))
  ;; truncate deeper list to the max
  (replace-regexp-in-string "^----+" "---" contents))

(defun org-pukiwiki-headline (headline contents info)
  "Transcode HEADLINE element into Pukiwiki format.
CONTENTS is the headline contents."
  (let* ((level (org-export-get-relative-level headline info))
	 (title (org-export-data (org-element-property :title headline) info))
	 (limit (plist-get info :headline-levels)))
    (if (org-export-low-level-p headline info)
	(concat (make-string (- level limit) ?-) " " title "\n"
		(when contents
		    (replace-regexp-in-string "^-" "--" contents)))
      (concat (make-string level ?*) " " title "\n" contents))))


(defun org-pukiwiki-keyword (keyword contents info)
  "Transcode KEYWORD element into Pukiwiki format."
  (unless (member (org-element-property :key keyword)
		  (mapcar
		   (lambda (block-cons)
		     (and (eq (cdr block-cons) 'org-element-export-block-parser)
			  (car block-cons)))
		   org-element-block-name-alist))
    (org-element-keyword-interpreter keyword nil)))

(setq ox-puki-a 0)
(setq ox-puki-row 0)
(setq ox-puki-cell 0)
(setq ox-puki-header nil)

(defun org-pukiwiki-table (keyword contents info)
  "Transcode TABLE element into Pukiwiki format."
  (setq ox-puki-a (1+ ox-puki-a))
  contents)

(defun org-pukiwiki-table-row (keyword contents info)
  "Transcode TABLE ROW element into Pukiwiki format."
  (setq ox-puki-row (1+ ox-puki-row))
  ;; pukiwiki does not support holizontal separator, ignore it
  (when contents
    (if (org-export-table-row-ends-header-p keyword info)
	(concat contents " |h")
      (concat contents " |"))))

(defun org-pukiwiki-table-cell (keyword contents info)
  "Transcode TABLE CELL element into Pukiwiki format."
  (setq ox-puki-cell (1+ ox-puki-cell))
  (concat "| " contents " "))


;;;###autoload
(defun org-pukiwiki-export-as-pukiwiki (&optional async subtreep visible-only)
  "Export current buffer to a buffer in Pukiwiki format.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org PUKIWIKI Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org PUKIWIKI Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (text-mode)
	      (org-export-add-to-stack (current-buffer) 'pukiwiki)))
	`(org-export-as 'pukiwiki ,subtreep ,visible-only))
    (let ((outbuf
	   (org-export-to-buffer
	    'pukiwiki "*Org PUKIWIKI Export*" subtreep visible-only)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-pukiwiki-export-to-pukiwiki (&optional async subtreep visible-only)
  "Export current buffer to a Pukiwiki file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".txt" subtreep)))
    (if async
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'org))
	  `(expand-file-name
	    (org-export-to-file
	     'pukiwiki ,outfile ,subtreep ,visible-only)))
      (org-export-to-file 'pukiwiki outfile subtreep visible-only))))

(provide 'ox-pukiwiki)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-pukiwiki.el ends here
