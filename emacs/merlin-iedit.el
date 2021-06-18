;;; merlin-iedit.el --- Merlin and iedit integration.   -*- coding: utf-8; lexical-binding: t -*-
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;; Created: 27 June 2014
;; Version: 0.1
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "25.1") (merlin "3") (iedit "0.9"))
;; URL: http://github.com/ocaml/merlin

;;; Commentary:

;; Provides the command `merlin-iedit-occurrences', which allows the
;; user to edit all the occurrences of the identifier at point using
;; `iedit'.

;;; Code:

(require 'merlin)
(require 'cl-lib)
(require 'iedit)

(defun merlin-iedit--printable (&rest _args)
  "Stub substituting `iedit-printable' during merlin-iedit-occurrences."
  "merlin-iedit-occurrences")

(defun merlin-iedit--make-occurrences-overlays (occurrences)
  "Stub substituting `iedit-make-occurrences-overlays' during merlin-iedit-occurrences."
  (setq iedit-aborting nil)
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  (save-excursion
    (save-window-excursion
      (dolist (pos occurrences)
        (let* ((start (assoc 'start pos))
               (end   (assoc 'end   pos))
               (beginning (merlin-make-point start))
               (ending    (merlin-make-point end)))
          (if (text-property-not-all beginning ending 'read-only nil)
              (push (iedit-make-read-only-occurrence-overlay beginning ending)
                    iedit-read-only-occurrences-overlays)
            (push (iedit-make-occurrence-overlay beginning ending)
                  iedit-occurrences-overlays))))))
  (length occurrences))

;;;###autoload
(defun merlin-iedit-occurrences ()
  "Edit occurrences of identifier under cursor using `iedit'."
  (interactive)
  (if iedit-mode (iedit-mode -1)
    (let ((r (merlin-call "occurrences"
                          "-identifier-at" (merlin-unmake-point (point)))))
      (when r
        (if (listp r)
            (cl-letf (((symbol-function 'iedit-printable) #'merlin-iedit--printable)
                      ((symbol-function 'iedit-make-occurrences-overlays)
                       (lambda (a _b _c)
                         (merlin-iedit--make-occurrences-overlays a))))
              (iedit-start r (point-min) (point-max)))
          (message r))))))

(provide 'merlin-iedit)
;;; merlin.el ends here
