;;; merlin-company.el --- Merlin and company mode integration.   -*- coding: utf-8; lexical-binding: t -*-
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;; Created: 15 May 2015
;; Version: 0.1
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "25.1") (merlin "3") (company "0.9"))
;; URL: http://github.com/ocaml/merlin

;;; Commentary:

;; (require 'merlin-company) should be enough to get merlin to work within
;; company.
;;
;; If you always want company-mode to be available, consider adding:
;;   (add-hook 'after-init-hook #'global-company-mode)
;; in your .emacs.

;;; Code:

(require 'merlin)
(require 'company)
(require 'cl-lib)

;; It would be nice to define a proper (somewhat stable) interface in merlin.el
;; to be used by other modules.

(defcustom merlin-company-everywhere t
  "Non-nil to offer completions in comments and strings."
  :type 'boolean
  :group 'merlin)

;; Internal functions

(defun merlin-company--get-candidate-type (candidate)
  (get-text-property 0 'merlin-compl-type candidate))

(defun merlin-company--get-candidate-doc (candidate)
  (get-text-property 0 'merlin-compl-doc candidate))

(defun merlin-company--is-module (candidate)
  (string-equal (merlin-company--get-candidate-type candidate) " <module>"))

(defun merlin-company--has-doc (candidate)
  (not (or (string-equal (merlin-company--get-candidate-doc candidate) "")
           (merlin-company--is-module candidate))))

(defun merlin-company--doc-buffer (candidate)
  "Computes the /doc/ of CANDIDATE and returns the buffer where it printed it"
  (cond
   ((merlin-company--has-doc candidate)
    (let* ((doc (merlin-company--get-candidate-doc candidate))
                                        ; We add (** and *) around documentation so we can reuse the type buffer
                                        ; without getting some weird highlighting.
           (doc (concat
                 "val " candidate " : "
                 (merlin-company--get-candidate-type candidate)
                 "\n\n(** " doc " *)")))
      (merlin-display-in-type-buffer doc)))

   ((merlin-company--is-module candidate)
    (merlin-display-in-type-buffer
     (merlin-call "type-expression"
                  "-position" (merlin-unmake-point (point))
                  "-expression" (substring-no-properties candidate))))

   (t (merlin-display-in-type-buffer
       (merlin-company--get-candidate-type candidate))))
  (get-buffer merlin-type-buffer-name))

(defun merlin-company--meta (candidate)
  "Computes the information to display in the minibuffer for CANDIDATE"
  (let* ((arg-type (get-text-property 0 'merlin-arg-type candidate))
         (entry-ty (merlin-company--get-candidate-type candidate))
         (default  (if (and merlin-completion-arg-type arg-type)
                     (concat "Expected argument type: " arg-type)
                     entry-ty)))
    (cond
      ((merlin-company--has-doc candidate)
       (concat default " (press F1 to display documentation of " candidate ")"))
      ((merlin-company--is-module candidate)
       (concat "Press F1 to display the signature of module " candidate
               " (successive calls will expand aliases)"))
      (t default))))

;; Public functions
;;;###autoload
(defun merlin-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (when merlin-mode
    (cl-case command
      (interactive (company-begin-backend 'merlin-company-backend))
      (prefix
       (let* ((bounds (merlin-completion-bounds))
              (result (merlin-buffer-substring (car bounds) (cdr bounds))))
         (when (and (boundp 'company-candidates-cache)
                    (or (string-match-p "\\.$" result)
                        (member '("" "") company-candidates-cache)))
           ;; for some reason, company doesn't always clear its cache
           (setq company-candidates-cache nil))
         result))
      (no-cache t)
      (sorted t)
      (init t)
      (require-match 'never)
      (doc-buffer (merlin-company--doc-buffer arg))
      (location
       (ignore-errors
         (let ((data (merlin-call-locate arg)))
           (when (listp data)
             (let ((filename (merlin-lookup 'file data (buffer-file-name)))
                   (linum (cdr (assoc 'line (assoc 'pos data)))))
               (cons filename linum))))))
      (candidates
       (when (or merlin-company-everywhere (not (company-in-string-or-comment)))
         (let ((prefix (merlin-completion-prefix arg)))
           (cl-loop for x in (merlin-complete arg)
                    collect
                    (propertize (merlin-completion-entry-text prefix x)
                                'merlin-compl-type
                                (merlin-completion-entry-short-description x)
                                'merlin-arg-type (cdr (assoc 'argument_type x))
                                'merlin-compl-doc (cdr (assoc 'info x)))))))
      (post-completion
       (let ((minibuffer-message-timeout nil))
         (minibuffer-message "%s : %s" arg (merlin-company--get-candidate-type arg))))
      (meta (merlin-company--meta arg))
      (annotation
       (concat " : " (merlin-company--get-candidate-type arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register into company-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'company-backends 'merlin-company-backend)

(provide 'merlin-company)
;;; merlin-company.el ends here
