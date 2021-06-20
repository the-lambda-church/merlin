;;; merlin-imenu.el --- Merlin and imenu integration.   -*- coding: utf-8; lexical-binding: t -*-
;; Licensed under the MIT license.

;; Author: tddsg (Ta Quang Trung)
;; Version: 0.3
;; Release log:
;;   - v0.1: July 2016
;;   - v0.2: 27 April 2017
;;   - v0.3: 21 August 2019
;; Keywords: ocaml, imenu, merlin
;; URL:

(require 'imenu)
(require 'subr-x)
(require 'merlin)

;; lists of different outline items
(defvar-local merlin-imenu--value-list nil)
(defvar-local merlin-imenu--type-list nil)
(defvar-local merlin-imenu--exception-list nil)
(defvar-local merlin-imenu--module-list nil)
(defvar-local merlin-imenu--modtyp-list nil)
(defvar-local merlin-imenu--class-list nil)

(defun merlin-imenu-compute-position (line col)
  "Get location of the item."
  (save-excursion
    (condition-case nil
        (progn
          (goto-char (point-min))
          (forward-line (- line 1))
          (move-to-column col)
          (point))
      (error -1))))

(defun merlin-imenu-create-entry (prefix name type kind line col)
  (let* ((name (concat prefix name))
         (type (cond ((not (string= kind "Value")) "null")
                     ((not (string= type "null")) type)
                     (t (let* ((types (merlin-call
                                       "type-enclosing"
                                       "-position" (format "%d:%d" line col)
                                       "-expression" name)))
                          (cdr (nth 3 (car types)))))))
         (type (replace-regexp-in-string "\n" " " type))
         (type (propertize type 'face 'font-lock-doc-face)))
    (if (string= type "null") name (concat name " : " type))))

(defun merlin-imenu-parse-outline (prefix outline)
  (dolist (item outline)
    (let* ((line (cdr (assoc 'line (assoc 'start item))))
           (col (cdr (assoc 'col (assoc 'start item))))
           (name (cdr (assoc 'name item)))
           (kind (cdr (assoc 'kind item)))
           (type (cdr (assoc 'type item)))
           (sub-trees (cdr (assoc 'children item)))
           (entry (merlin-imenu-create-entry prefix name type kind line col))
           (position (merlin-imenu-compute-position line col))
           (marker (cons entry (set-marker (make-marker) position))))
      (cond ((string= kind "Value")
             (setq merlin-imenu--value-list (cons marker merlin-imenu--value-list)))
            ((string= kind "Type")
             (setq merlin-imenu--type-list (cons marker merlin-imenu--type-list)))
            ((string= kind "Module")
             (setq merlin-imenu--module-list (cons marker merlin-imenu--module-list)))
            ((string= kind "Modtype")
             (setq merlin-imenu--modtype-list (cons marker merlin-imenu--modtype-list)))
            ((string= kind "Class")
             (setq merlin-imenu--class-list (cons marker merlin-imenu--class-list)))
            ((string= kind "Exn")
             (setq merlin-imenu--exception-list (cons marker merlin-imenu--exception-list))))
      (when sub-trees
        (merlin-imenu-parse-outline (concat prefix entry ".") sub-trees)))))

(defun merlin-imenu-create-index ()
  "Create data for imenu using the merlin outline feature."
  ;; Reset local vars
  (setq merlin-imenu--value-list nil
        merlin-imenu--type-list nil
        merlin-imenu--module-list nil
        merlin-imenu--modtype-list nil
        merlin-imenu--class-list nil
        merlin-imenu--exception-list nil)
  ;; Read outline tree
  (merlin-imenu-parse-outline "" (merlin-call "outline"))
  (let ((index nil))
    (when merlin-imenu--value-list
      (push (cons "Value" merlin-imenu--value-list) index))
    (when merlin-imenu--exception-list
      (push (cons "Exception" merlin-imenu--exception-list) index))
    (when merlin-imenu--type-list
      (push (cons "Type" merlin-imenu--type-list) index))
    (when merlin-imenu--module-list
      (push (cons "Module" merlin-imenu--module-list) index))
    (when merlin-imenu--modtype-list
      (push (cons "Modtype" merlin-imenu--modtype-list) index))
    (when merlin-imenu--class-list
      (push (cons "Class" merlin-imenu--class-list) index))
    index))

;;;###autoload
(defun merlin-use-merlin-imenu ()
  "Merlin: use the custom imenu feature from Merlin"
  (interactive)
  ;; change the index function and force a rescan of imenu-index
  (setq imenu-create-index-function 'merlin-imenu-create-index)
  (imenu--cleanup)
  (setq imenu--index-alist nil))

(provide 'merlin-imenu)
;;; merlin-imenu.el ends here
