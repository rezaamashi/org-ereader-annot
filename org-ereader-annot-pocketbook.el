;;; org-ereader-annot-pocketbook.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Reza A'masyi
;;
;; Author: Reza A'masyi <mnurrreza@gmail.com>
;; Maintainer: Reza A'masyi <mnurrreza@gmail.com>
;; Created: March 06, 2023
;; Modified: March 06, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/muna/org-ereader-annot-pocketbook
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Specific file that handles `pocketbook' annotation parsing and insertion
;;; Code:

(require 'xml)

(defun org-ereader-annot--pocketbook-parse (pocketbook-annot-file sort-by-page level)
  "Parser function for `org-ereader-annot--pocketbook-result'. This
function parse highlight color, page number, text content, and
note content. Respectively, it is then inserted into `tag',
`properties' content, `header' (akin to `org-noter'), and
`content' of the text.

But as the nature of the annotation file is sorted by time of
collection, if SORT-BY-PAGE is non-nil the parsed data will be
produced as is. LEVEL is an integer to define the header level
produced. it is defined through `org-ereader-annot-header-level'."
  (with-temp-buffer
    (insert-file-contents pocketbook-annot-file)
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          (results '()))
      (dolist (div (dom-by-tag dom 'div))
        (when (string-match "^bookmark bm-color-\\(.*\\)$" (cdr (assoc 'class (dom-attributes div))))
          (let* ((color (match-string 1 (cdr (assoc 'class (dom-attributes div)))))
                 (page (string-to-number (dom-text (car (dom-by-class div "bm-page")))))
                 (text-div (car (dom-by-class div "bm-text")))
                 (text (dom-text (car (dom-by-tag text-div 'p))))
                 (note-div (car (dom-by-class div "bm-note")))
                 (note (when note-div (dom-text (car (dom-by-tag note-div 'p))))))

            (push (if sort-by-page (list page (format "%s  %s :%s:\n :PROPERTIES:\n:PAGE: %s\n:END:\n%s\n"
                                                      (make-string (+ 1 level) ?*)text color page (if note (concat "\n" (make-string (+ 2 level) ?*)"Note :note:\n" note "\n") "")))
                              (format "%s  %s :%s:\n :PROPERTIES:\n:PAGE: %s\n:END:\n%s\n"
                                      (make-string (+ 1 level) ?*)text color page (if note (concat "\n" (make-string (+ 2 level) ?*) "Note :note:\n" note "\n") "")))
                  results))))

      (if sort-by-page
          (progn
            (setq results (sort results (lambda (x y) (> (car x) (car y)))))
            (mapconcat 'identity (reverse (mapcar 'cadr results)) ""))
        (mapconcat 'identity (reverse results) "")))))

(defun org-ereader-annot--pocketbook-result (sort-by-page)
  "Parse PocketBook HTML annotation file and insert it `at-point' of
   the buffer. If SORT-BY-PAGE is non-nil, sort the annotations by pages."
  (let* ((file (abbreviate-file-name
                (expand-file-name
                 (if sort-by-page
                     (read-file-name "Select Pocketbook annotation file: " org-ereader-annot-directory)
                   (read-file-name "Select Pocketbook annotation file [unsort]: " org-ereader-annot-directory))))))
   (let ((level (if org-ereader-annot-header-level
                  org-ereader-annot-header-level 1)))
    (insert (concat (make-string level ?*) " Annotation\n:PROPERTIES:\n:FILE: " file
                    "\n:CREATED: " (format-time-string "[%Y-%m-%d %a %R]")
                    "\n:END:\n\n"
                    (if sort-by-page
                        (org-ereader-annot--pocketbook-parse file t level)
                      (org-ereader-annot--pocketbook-parse file nil level)))))))

(defun org-ereader-annot-pocketbook-insert ()
  "Wrapper around `org-ereader-annot--pocketbook-result' that sorts the annotations by page."
  (interactive)
  (org-ereader-annot--pocketbook-result t))

(defun org-ereader-annot-pocketbook-insert-unsorted ()
  "Wrapper around `org-ereader-annot--pocketbook-result' that
doesn't sort the annotations. Usually it means by creation time."
  (interactive)
  (org-ereader-annot--pocketbook-result nil))

(provide 'org-ereader-annot-pocketbook)
;;; org-ereader-annot-pocketbook.el ends here
