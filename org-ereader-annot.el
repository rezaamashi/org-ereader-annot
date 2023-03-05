;;; org-ereader-annot.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Reza A'masyi
;;
;; Author: Reza A'masyi <mnurrreza@gmail.com>
;; Maintainer: Reza A'masyi <mnurrreza@gmail.com>
;; Created: February 04, 2023
;; Modified: February 04, 2023
;; Version: 0.0.1
;; Keywords: convenience data docs files lisp local tools unix
;; Homepage: https://github.com/rezaamashi/org-ereader-annot.git
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Provide extraction interface from ereader exported annotation
;; into `org-mode' notes
;;; Code:

;;(require 'org)
;;(require 'libxml-parse-html)
(require 'xml)

(defcustom org-ereader-annot-app ""
  "Define the application that exported the annotation, such as:
'kindle', 'pocketbook', 'moon+ reader', etc."
  :type '(list string)
  :group 'org-ereader-annot)

(defcustom org-ereader-annot-directory ""
  "Define the target directory of annotation files."
  :type 'string
  :group 'org-ereader-annot)

(defcustom org-ereader-annot-level ""
  "Define the target heading level of the org-file."
  :type 'string
  :group 'org-ereader-annot)

(defun org-ereader-annot--pocketbook-parse (pocketbook-annot-file)
  (with-temp-buffer
    (insert-file-contents pocketbook-annot-file)
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          (results ()))
      (dolist (div (dom-by-tag dom 'div))
        (when (string-match "^bookmark bm-color-\\(.*\\)$" (cdr (assoc 'class (dom-attributes div))))
          (let* ((color (match-string 1 (cdr (assoc 'class (dom-attributes div)))))
                 (page (dom-text (car (dom-by-class div "bm-page"))))
                 (text-div (car (dom-by-class div "bm-text")))
                 (text (dom-text (car (dom-by-tag text-div 'p))))
                 (note-div (car (dom-by-class div "bm-note")))
                 (note (when note-div (dom-text (car (dom-by-tag note-div 'p))))))
            (push (format "** %s :%s:\n :PROPERTIES:\n:PAGE: %s\n:END:\n%s\n"
                          text color page (if note (concat "\n*** Note :note:\n" note "\n") "")) results))))
      (mapconcat 'identity (reverse results) ""))))

(defun org-ereader-annot--pocketbook-parse-and-sort (pocketbook-annot-file)
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

            (push (list page (format "** %s :%s:\n :PROPERTIES:\n:PAGE: %s\n:END:\n%s\n"
                                     text color page (if note (concat "\n*** Note :note:\n" note "\n") "")))
                  results))))

      (setq results (sort results (lambda (x y) (> (car x) (car y)))))
      (mapconcat 'identity (reverse (mapcar 'cadr results)) ""))))

(defun org-ereader-annot-pocketbook-insert ()
  "Parse and sort PocketBook HTML annotation file and insert it `at-point' of
   the buffer, and sort it by page"
  (interactive)
  (let ((file (expand-file-name (read-file-name "Select HTML file: "))))
    (insert (concat ":PROPERTIES:\n:FILE: " file
                    "\n:CREATED: " (format-time-string "[%Y-%m-%d %a %R]")
                    "\n:END:\n\n"
                    (org-ereader-annot--pocketbook-parse-and-sort file)))))

(defun org-ereader-annot-pocketbook-insert-unsorted ()
  "Parse PocketBook HTML annotation file and insert it `at-point' of
   the buffer"
  (interactive)
  (let ((file (expand-file-name (read-file-name "Select HTML file: "))))
    (insert (concat ":PROPERTIES:\n:FILE: " file
                    "\n:CREATED: " (format-time-string "[%Y-%m-%d %a %R]")
                    "\n:END:\n\n"
                    (org-ereader-annot--pocketbook-parse file)))))

(provide 'org-ereader-annot)
;;; org-ereader-annot.el ends here
