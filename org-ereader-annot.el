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

(require 'org-ereader-annot-pocketbook)

(defcustom org-ereader-annot-app ""
  "Variable that used in the generic `org-ereader-annot-insert'.
Define the app or device that exported the annotation, such as:
'kindle', 'pocketbook', 'moonreader','kindleapp', etc."
  :type '(choice "pocketbook")
  :group 'org-ereader-annot)

(defcustom org-ereader-annot-directory ""
  "Define the target directory of annotation files."
  :type 'directory
  :group 'org-ereader-annot)

(defcustom org-ereader-annot-header-level 1
  "Define the heading level of the `Annotation'. Default is `1'"
  :type 'integer
  :group 'org-ereader-annot)

(defun org-ereader-annot-insert ()
 "Function to parse annotation from an app that is defined on
`org-ereader-annot-app'. Currently only support `pocketbook'"
  (interactive)
  (cond ((string= org-ereader-annot-app "pocketbook") (org-ereader-annot-pocketbook-insert))
        ;; ((string= org-ereader-annot-app "kindleapp") (org-ereader-annot-kindleapp-insert))
        ;; ((string= org-ereader-annot-app "kindle") (org-ereader-annot-kindle-insert))
        ;; ((string= org-ereader-annot-app "moonreader") (org-ereader-annot-moonreader-insert))
        (t (message "Invalid value for `org-ereader-annot-app'."))))

(provide 'org-ereader-annot)
;;; org-ereader-annot.el ends here
