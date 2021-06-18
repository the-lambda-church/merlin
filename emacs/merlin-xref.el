;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'xref)
(require 'merlin)

;;;###autoload
(defun merlin-xref-backend ()
  "Merlin backend for Xref."
  'merlin-xref)

(defun merlin-xref--line (loc)
  (save-excursion
    (goto-char loc)
    (buffer-substring (line-beginning-position) (line-end-position))))

(cl-defmethod xref-backend-references ((_backend (eql merlin-xref)) _symbol)
  (mapcar
   (lambda (loc)
     (let ((pt (merlin-make-point (alist-get 'start loc))))
       (xref-make (merlin-xref--line pt)
                  (xref-make-buffer-location (current-buffer) pt))))
   (merlin--occurrences)))

(cl-defmethod xref-backend-definitions ((_backend (eql merlin-xref)) _symbol)
  (let* ((loc (merlin-call-locate))
         (file (alist-get 'file loc))
         (pos (alist-get 'pos loc))
         (line (alist-get 'line pos))
         (col (alist-get 'col pos)))
    (save-excursion
      (find-file file)
      (let ((desc (merlin-xref--line (merlin-make-point pos))))
        (list (xref-make desc (xref-make-file-location file line col)))))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql merlin-xref)))
  nil)

(provide 'merlin-xref)
