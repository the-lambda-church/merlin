;;; merlin-counsel.el --- Merlin counsel integration.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Created: 25 Feb 2020
;; Version: 0.1
;; Keywords: ocaml languages
;; URL: http://github.com/ocaml/merlin
(require 'merlin)
(require 'counsel)

(defun merlin-counsel--search-update-counsel-candidates (process)
  "Update the counsel candidates using the result fom merlin search PROCESS."
  (let (result error)      
    (with-current-buffer (process-buffer process)
      (setq result (buffer-string))
      (condition-case err
	  (setq result (car (read-from-string result)))
	(error
	 (setq error (format "merlin: error %s trying to parse answer: %s" err result))
	 (setq result nil)))
      (when result
	(let* ((notifications (cdr-safe (assoc 'notifications result)))
	       (timing (cdr-safe (assoc 'timing result)))
	       (class (cdr-safe (assoc 'class result)))
	       (value (cdr-safe (assoc 'value result))))
	  (cond ((string-equal class "return") (setq result value))
		((string-equal class "failure")
		 (setq error (format "merlin-mode failure: %s" value))
		 (setq result nil))
		((string-equal class "error")
		 (setq error (format "merlin: %s" value))
		 (setq result nil))
		(t (setq error (format "unknown answer: %S" class))
		   (setq result nil))))
	(cond
	 (result
	  (let* ((entries (cdr (assoc 'entries result)))
		 (transform
		  (lambda (entry)
		    (let ((text (merlin/completion-entry-text "" entry))
			  (desc (merlin/completion-entry-short-description entry)))
		      (concat text " : " desc))))
		 (result (mapcar transform entries)))
	    (ivy--set-candidates result)
	    (length result)))
	 (t
	  (ivy--set-candidates (list  (format "%s" error)))
	  1 ))))))

(defun merlin-counsel--search-async-sentinel (process _msg)
  "Sentinel function to be called once asynchronous merlin search PROCESS exits."
  (when (eq (process-status process) 'exit)
    (if (zerop (process-exit-status process))
	(progn
	  (merlin-counsel--search-update-counsel-candidates process)
	  (when counsel--async-start
	    (setq counsel--async-duration
		  (time-to-seconds (time-since counsel--async-start))))
	  (if ivy--all-candidates
              (ivy--exhibit)
            (ivy--insert-minibuffer ""))
	  )
      (setq ivy--all-candidates
            (let ((status (process-exit-status process))
                  (plist (plist-get counsel--async-exit-code-plist
                                    (ivy-state-caller ivy-last))))
              (list (or (plist-get plist status)
                        (format "error code %d" status)))))
      (ivy--exhibit)
      )))

(defun merlin-counsel--search-async-filter (process str)
  "Receive output from merlin search PROCESS on query string STR.
Update the minibuffer with the amount of lines collected every
`counsel-async-filter-update-time' microseconds since the last
update."
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (time-less-p (list 0 0 counsel-async-filter-update-time)
		     (time-since counsel--async-time))
    (let (numlines)
      (setq numlines
	    (merlin-counsel--search-update-counsel-candidates process))
      (when numlines
	(let ((ivy--prompt (format "%d++ %s" numlines (ivy-state-prompt ivy-last))))
        (ivy--insert-minibuffer (ivy--format ivy--all-candidates)))
	))
    (setq counsel--async-time (current-time))
    ))

(defun merlin-counsel--call-counsel-async (command buffer &rest args)
  "Invoke merlin binary with the proper setup to execute the command passed as argument (lookup appropriate binary, setup logging, pass global settings)"
  (let ((binary      (merlin-command))
	(flags       (merlin-lookup 'flags merlin-buffer-configuration))
	(process-environment (copy-list process-environment))
	(dot-merlin  (merlin-lookup 'dot-merlin merlin-buffer-configuration))
	(logfile     (or (merlin-lookup 'logfile merlin-buffer-configuration)
			 merlin-logfile))
	(extensions  (merlin--map-flatten (lambda (x) (cons "-extension" x))
					  merlin-buffer-extensions))
	(packages    (merlin--map-flatten (lambda (x) (cons "-package" x))
					  merlin-buffer-packages))
	(filename    (buffer-file-name (buffer-base-buffer))))
    ;; Update environment
    (dolist (binding (merlin-lookup 'env merlin-buffer-configuration))
      (let* ((equal-pos (string-match-p "=" binding))
	     (prefix (if equal-pos
			 (substring binding 0 (1+ equal-pos))
		       binding))
	     (is-prefix (lambda (x) (string-prefix-p prefix x))))
	(setq process-environment (delete-if is-prefix process-environment))
	(when equal-pos
	  (setq process-environment (cons binding process-environment)))))
    ;; Compute verbosity
    (when (eq merlin/verbosity-context t)
      (setq merlin/verbosity-context (cons command args)))
    (if (not merlin/verbosity-context)
	(setq merlin--verbosity-cache nil)
      (if (equal merlin/verbosity-context (car-safe merlin--verbosity-cache))
	  (setcdr merlin--verbosity-cache (1+ (cdr merlin--verbosity-cache)))
	(setq merlin--verbosity-cache (cons merlin/verbosity-context 0))))
    ;; Compute full command line.
    (setq args (merlin--map-flatten-to-string
		"server" command "-protocol" "sexp"
		(when dot-merlin
		  (list "-dot-merlin" dot-merlin))
		;; Is debug mode enabled
		(when merlin-debug '("-log-file" "-"))
		;; If command is repeated, increase verbosity
		(when merlin/verbosity-context
		  (list "-verbosity" (cdr merlin--verbosity-cache)))
		packages
		extensions
		(unless (string-equal merlin-buffer-flags "")
		  (cons "-flags" merlin-buffer-flags))
		(when filename
		  (cons "-filename" filename))
		args))
    ;; Call merlin process
    ;; (message "calling %s" (string-join (cons binary args) " "))
    (let ((proc (counsel--async-command (cons binary args)
					#'merlin-counsel--search-async-sentinel
					#'merlin-counsel--search-async-filter)))
      ;; send current buffer text
      (with-current-buffer buffer
	(condition-case err
	    (progn
	      	(process-send-region proc (point-min) (point-max))
		(process-send-eof proc)
		)
	  ;; sometimes the merlin search process quits before we can send
	  ;; the buffer (i.e if the search string is badly malformed)
	  (file-error nil)
	  )
	)
      proc)))

(defun merlin-counsel--search-function (buffer str)
  "Invoke async merlin process to handle query STR on BUFFER."
  (or (ivy-more-chars)
   (progn
     (merlin-counsel--call-counsel-async
      "search-by-polarity" buffer "-query" str
      "-position" (merlin/unmake-point (point)))
     '("" "working...")
     )))

(defun merlin-counsel-search (&optional initial-input)
  "Call merlin search asynchronously using counsel on the current buffer.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (let ((buffer (current-buffer)))
    (ivy-read "Search: " (lambda (str) (merlin-counsel--search-function buffer str))
	      :initial-input initial-input
	      :dynamic-collection t
	      :action (lambda (entry)
			(setq entry (car (split-string entry ":")))
			(when entry
			  (with-current-buffer buffer
			    (insert (s-trim entry))
			    )))
	      :matcher nil
	      :unwind #'counsel-delete-process
	      :caller 'merlin-counsel-search)
    ))

(provide 'merlin-counsel)
