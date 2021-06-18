;;; merlin.el --- Mode for Merlin, an assistant for OCaml   -*- coding: utf-8; lexical-binding: t -*-

;; Licensed under the MIT license.

;; Author: Frédéric Bour <frederic.bour(_)lakaban.net>
;; Created: 30 August 2016
;; Version: 3.0
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/ocaml/merlin

;;; Commentary:
;; merlin-mode is an Emacs interface to merlin.  It allows you to perform
;; queries such as getting the type of an expression, completion, and so on.

;; Installation:
;; You need merlin installed on your system (ocamlmerlin binary) for merlin-mode
;; to work.

;;; Usage:
;; TODO

;;; Code:

(require 'cl-lib)
(require 'crm) ;; for completing-read-multiple
;; caml-types for highlighting
;; (https://github.com/ocaml/merlin/issues/331)
(require 'caml-types nil 'noerror)

;; silence free variable warning
(defvar merlin-mode)

(defgroup merlin nil
  "merlin binding mode allowing completion and typing in OCaml files."
  :group 'languages :prefix "merlin-")

;;
;; Faces
;;

(defface merlin-type-face
  '((t :inherit caml-types-expr-face))
  "Face for highlighting a typed expr."
  :group 'merlin)

(defface merlin-compilation-warning-face
  '((t :inherit compilation-warning))
  "Face to use to highlight merlin warnings."
  :group 'merlin)

(defface merlin-compilation-error-face
  '((t :inherit compilation-error))
  "Face to use to highlight merlin errors."
  :group 'merlin)

;;
;; Customizable vars
;;

(defcustom merlin-show-instance-in-lighter t
  "Show the current instance of the buffer in the lighter."
  :group 'merlin :type 'boolean)

(defcustom merlin-report-errors-in-lighter nil
  "Report absence of .merlin or errors in .merlin in the lighter."
  :group 'merlin :type 'boolean)

(defcustom merlin-client-log-function nil
  "The function takes four arguments:
   - the path to the merlin binary
   - the name of the command
   - the total time spent in the server (or -1 if that information is not available)
   - the resulting state (\"return\", \"failure\" or \"interrupted\")
Its return value is ignored."
  :group 'merlin :type 'symbol)

(defcustom merlin-configuration-function nil
  "The function takes no argument and returns the configuration for the current
buffer, in a form suitable for `merlin-buffer-configuration'."
  :group 'merlin :type 'symbol)

(defcustom merlin-grouping-function nil
  "Deprecated, see `merlin-configuration-function'."
  :group 'merlin :type 'symbol)

(defcustom merlin-command 'opam
  "The path to merlin in your installation."
  :group 'merlin :type '(choice (file :tag "Filename (default binary is \"ocamlmerlin\")")
                                (function :tag "Function returning path to the binary")
                                (const :tag "Use current opam switch" opam)))

(defcustom merlin-completion-with-doc nil
  "If non-nil, tries to retrieve ocamldoc comments associated with each completion candidate"
  :group 'merlin :type 'boolean)

(defcustom merlin-completion-dwim t
  "If non-nil, fallback to fuzzier completion when normal completion gives no result."
  :group 'merlin :type 'boolean)

(defcustom merlin-completion-types t
  "If non-nil, print the types of the variables during completion."
  :group 'merlin :type 'boolean)

(defcustom merlin-completion-arg-type t
  "If non-nil, print the type of the expected argument during completion on an application."
  :group 'merlin :type 'boolean)

(defcustom merlin-debug nil
  "If non-nil, log the data sent and received from merlin into `merlin-log-buffer-name' buffer."
  :group 'merlin :type 'boolean)

(defcustom merlin-report-warnings t
  "If non-nil, report warnings, otherwise ignore them."
  :group 'merlin :type 'boolean)

(defcustom merlin-occurrences-buffer-name "*merlin-occurrences*"
  "The name of the buffer listing occurrences of an identifier after a call to `merlin-occurrences'."
  :group 'merlin :type 'string)

(defcustom merlin-type-buffer-name "*merlin-types*"
  "The name of the buffer storing module signatures."
  :group 'merlin :type 'string)

(defcustom merlin-log-buffer-name "*merlin-log*"
  "The name of the buffer storing log messages and debug information. See `merlin-debug'."
  :group 'merlin :type 'string)

(defcustom merlin-favourite-caml-mode nil
  "The OCaml mode to use for the *merlin-types* buffer."
  :group 'merlin :type 'symbol)

(defcustom merlin-error-after-save '("ml" "mli")
  "Determines whether merlin should check for errors after saving.
If t, always check for errors after saving.
If nil, never check.
If a string list, check only if the extension of the buffer-file-name is in the list."
  :group 'merlin :type '(choice (repeat string) boolean))

(defcustom merlin-error-in-fringe (>= emacs-major-version 24)
  "If non-nil, display errors in fringe"
  :group 'merlin :type 'boolean)

(defcustom merlin-error-on-single-line nil
  "Only highlight first line of multi-line error messages"
  :group 'merlin :type 'boolean)

(defcustom merlin-error-check-then-move t
  "If t, merlin-error-next and merlin-error-prev first update the errors then move the cursor.
If nil, they both update and move at the same time."
  :group 'merlin :type 'boolean)

(defcustom merlin-default-flags nil
  "The flags to pass to ocamlmerlin."
  :group 'merlin :type '(repeat string))

(defcustom merlin-occurrences-show-buffer 'other
  "Determine how to display the occurrences list after a call to `merlin-occurrences'."
  :group 'merlin :type '(choice (const :tag "Don't show list" never)
                                (const :tag "Show in the current window" same)
                                (const :tag "Show in another window" other)))

(defcustom merlin-locate-in-new-window 'diff
  "Determine whether to display results of `merlin-locate' in a new window or not."
  :group 'merlin :type '(choice (const :tag "Always open a new window" always)
                                (const :tag "Never open a new window" never)
                                (const :tag "Open a new window only if the target file is different from current buffer." diff)))

(defcustom merlin-locate-preference 'ml
  "Determine whether locate should in priority look in ml or mli files."
  :group 'merlin :type '(choice (const :tag "Look at implementation" ml)
                                (const :tag "Look at interfaces" mli)))

(defcustom merlin-locate-focus-new-window t
  "If non-nil, when locate opens a new window it will give it the focus."
  :group 'merlin :type 'boolean)

(defcustom merlin-logfile nil
  "If non-nil, use this file for the log file (should be an absolute path)."
  :group 'merlin :type 'filename)

(defcustom merlin-arrow-keys-type-enclosing t
  "If non-nil, after a type enclosing, C+up and C+down arrow are used to go up and down the AST. As well, C+w copy the type to the kill ring and C+d destructure the expression."
  :group 'merlin :type 'boolean)

(defcustom merlin-type-after-locate nil
  "If non-nil, use type-enclosing after locate."
  :group 'merlin :type 'boolean)

(defcustom merlin-allow-sit-for t
  "When user attention is required, merlin will use `sit-for' only if `merlin-allow-sit-for' is `t'."
  :group 'merlin :type 'boolean)

(defalias 'merlin-find-file 'find-file-other-window
  "The function called when merlin try to open a file (doesn't apply to
merlin-locate, see `merlin-locate-in-new-window').")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer local settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local merlin-buffer-configuration nil
  "An association list describing the configuration of merlin binary for the current buffer.  Customize `merlin-configuration-function` to initialize it.
The association list can contain the following optional keys:
- `flags': extra flags to give merlin

- `command': command to run

- `env': list of strings (of the shape VARIABLE=FOO) (see
`process-environment') that will be prepended to the environment of merlin

- `dot-merlin': path to a .merlin file

- `logfile': path to the logfile

- `name': a short name for this configuration, displayed in user notifications.

- `do-not-cache-config': if set, refreshes the config on every command"
)

(defvar-local merlin-buffer-packages nil
   "List of packages loaded in the buffer")

(defvar-local merlin-buffer-packages-path nil
   "List of path of packages loaded in the buffer")

(defvar-local merlin-buffer-extensions nil
   "List of syntax extensions active in the buffer")

(defvar-local merlin-buffer-flags ""
   "Additional flags to pass to merlin")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar merlin-opam-bin-path nil)

;; If user did not specify its merlin-favourite-caml-mode, try to guess it from
;; the buffer being edited
(defvar merlin-guessed-favorite-caml-mode nil)

(defvar merlin--idle-timer nil)

;; Errors related variables

(defvar-local merlin-erroneous-buffer nil
  "Whether the buffer is erroneous or not")

(defvar merlin-highlight-overlay nil
  "Merlin overlay used for highlights.")

;; Type related variables

(defvar-local merlin-enclosing-types nil
  "List containing the enclosing type.")

(defvar-local merlin-enclosing-offset nil
  "Current offset in `merlin-enclosing-types'.")

;; Locate

(defvar merlin-position-stack nil)

;; Verbosity

(defconst merlin-verbosity-context nil
  "If non-nil, a simple key used to determine verbosity")

(defvar-local merlin--verbosity-cache nil
  "Cache last command to determine verbosity level")

(defvar-local merlin-debug-last-commands nil
  "Last merlin commands (for debugging)")

;; Misc

(defvar-local merlin--project-cache nil
  "Cache for merlin--project-get")

(defvar-local merlin--dwimed nil
  "Remember if we used dwim for the current completion or not")

;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;

(defun merlin--completion-map-with-space (&optional map)
  "Return a map suitable for `minibuffer-local-completion-map' but not overriding SPC binding"
  (unless map (setq map minibuffer-local-completion-map ))
  (setq map (make-composed-keymap nil map))
  (define-key map (kbd "SPC") nil)
  map)

(defun merlin-debug (message &rest args)
  "Output S to `merlin-log-buffer-name' if `merlin-debug' is non-nil in the current buffer."
  (when merlin-debug
    (with-current-buffer (get-buffer-create merlin-log-buffer-name)
      (goto-char (point-max))
      (if args (insert (apply 'format message args))
        (insert message)))))

(defun merlin-enable-debug ()
  "Start recording merlin debug information to `merlin-log-buffer-name'."
  (interactive)
  (setq merlin-debug t)
  (message "merlin: logging to %S buffer" merlin-log-buffer-name))

(defun merlin-disable-debug ()
  "Stop recording debug information."
  (interactive)
  (setq merlin-debug nil))

(defun merlin-debug-last-commands ()
  "Display last commands executed and their result (if any)"
  (interactive)
  (let (buf)
    (dolist (command merlin-debug-last-commands)
      (push (concat "- result: " (or (cdr command) "failed")) buf)
      (push (mapconcat 'identity
                       (merlin--map-flatten-to-string "command: " (car command))
                       " ") buf))
    (message "Last commands executed, most recent at the end:\n%s"
             (mapconcat 'identity buf "\n"))))

(defun merlin-buffer-substring (start end)
  "Return content of buffer between two points or empty string if points are not valid"
  (if (< start end) (buffer-substring-no-properties start end) ""))

(defsubst merlin-lookup (key list &optional default)
  "Lookup KEY in LIST which is a list of pairs. If not found,
return DEFAULT or the value associated to KEY."
  (assoc-default key list nil default))

(defun merlin--differs-from-current-file (path)
  (not (string-equal path (buffer-file-name))))

(defun merlin--rev-map-flatten (f xs &optional acc)
  (while (consp xs)
    (setq acc (if (listp (car xs))
                  (merlin--rev-map-flatten f (car xs) acc)
                (cons (funcall f (car xs)) acc)))
    (setq xs (cdr xs)))
  (when xs
    (setq acc (cons xs acc)))
  acc)

(defun merlin--map-flatten (f &rest xs)
  (nreverse (merlin--rev-map-flatten f xs)))

(defun merlin--map-flatten-to-string (&rest xs)
  (merlin--map-flatten
    (lambda (x) (if (stringp x) x (prin1-to-string x))) xs))

(defun merlin--goto-file-and-point (data)
  "Go to the file and position indicated by DATA which is an assoc list
containing fields file, line and col."
  (let* ((file (assoc 'file data))
         (open-window (cond ((equal merlin-locate-in-new-window 'never) nil)
                            ((equal merlin-locate-in-new-window 'always))
                            (file (merlin--differs-from-current-file (cdr file)))))
         (filename (if file (cdr file) (buffer-file-name (buffer-base-buffer))))
         (focus-window (or (not open-window) merlin-locate-focus-new-window))
         (do-open (lambda ()
                    (push-mark)
                    (if open-window
                      (find-file-other-window filename)
                      (find-file filename))
                    (merlin--goto-point (cdr (assoc 'pos data))))))
    (if focus-window
        (progn
          (push (cons (buffer-name) (point)) merlin-position-stack)
          (funcall do-open)
          (message "Use %s to go back."
                   (substitute-command-keys "\\[merlin-pop-stack]")))
      (save-excursion (save-selected-window (funcall do-open))))))

(defun merlin-add-display-properties (overlay bitmap string &optional face)
  "Add the necessary properties to OVERLAY to display it nicely."
  (let ((prop (if window-system
                  `(left-fringe ,bitmap . ,(if face (list face) nil))
                `((margin left-margin) ,string))))
    (when face (overlay-put overlay 'face face))
    (overlay-put overlay 'before-string
                 (propertize " " 'display prop))))

(defun merlin--highlight (bounds face)
  "Create an overlay on BOUNDS (of the form (START . END)) and give it FACE."
  (remove-overlays nil nil 'merlin-kind 'highlight)
  (let ((overlay (make-overlay (car bounds) (cdr bounds))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'merlin-kind 'highlight)
    (if merlin-allow-sit-for
        (unwind-protect (sit-for 60) (delete-overlay overlay)))
      (run-with-idle-timer 0.5 nil
        (lambda () (delete-overlay overlay)))))

;; Position management

(defun merlin--goto-point (data)
  "Go to the point indicated by DATA which must be an assoc list with fields
line and col. If narrowing is in effect, widen if DATA is outside the visible region."
  (let ((target-pos (merlin--point-of-pos data)))
    ;; If our target position is outside the narrowed region, we'll
    ;; have to widen.
    (when (or (< target-pos (point-min))
              (> target-pos (point-max)))
      (widen))
    (goto-char target-pos)))

(defun merlin--point-of-pos (data)
  "Transform DATA (a remote merlin position) into a point.
DATA must be an assoc list with fields line and col."
  (let ((line-num (merlin-lookup 'line data 0))
        (col-byte-offset (merlin-lookup 'col data 0)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- line-num))
        ;; Find the target position, converting the byte position to a
        ;; character offset.
        (let* ((bol-offset (position-bytes (point)))
               (col-offset (max 0 col-byte-offset))
               (target-off (+ bol-offset col-offset)))
          (byte-to-position target-off))))))

(defun merlin-make-point (data)
  "Transform DATA (a remote merlin position) into a point."
  (merlin--point-of-pos data))

(defun merlin-unmake-point (point)
  "Destruct POINT to line / col."
  (save-excursion
   (save-restriction
     (widen)
     (goto-char point)
     (format "%d:%d" (line-number-at-pos)
             (- (position-bytes (point))
                (position-bytes (line-beginning-position)))))))

(define-obsolete-function-alias 'merlin/unmake-point 'merlin-unmake-point "2021-01-27")

(defun merlin--make-bounds (data)
  "From a remote merlin object DATA {\"start\": LOC1; \"end\": LOC2},
return (LOC1 . LOC2)."
  (cons
   (merlin-make-point (cdr (assoc 'start data)))
   (merlin-make-point (cdr (assoc 'end data)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCESS MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--call-process (path args)
  "Some workarounds for piping buffer content to a process"
  (merlin-debug "# calling binary: %S with arguments: %S.\n" path args)
  (let ((ib  (current-buffer))
        (tmp (when merlin-debug (make-temp-file "merlin")))
        (wd  (expand-file-name default-directory))
        result)
    (with-temp-buffer
      (let ((ob (current-buffer)))
        (with-current-buffer ib
          (save-restriction
            (widen)
            (let ((default-directory wd))
              (apply 'call-process-region (point-min) (point-max) path nil
                     (list ob tmp) nil args)))))
      (setq result (buffer-string))
      (merlin-debug "# stdout\n%s" result)
      (when tmp
        (with-demoted-errors "Error when trying to read merlin log: %S"
          (with-current-buffer merlin-log-buffer-name
            (goto-char (point-max))
            (insert "# stderr\n")
            (insert-file-contents tmp)
            (delete-file tmp))))
      result)))

(defun merlin--call-merlin (command &rest args)
  "Invoke merlin binary with the proper setup to execute the command passed as argument (lookup appropriate binary, setup logging, pass global settings)"
  ;; Really start process
  (let ((binary      (merlin-command))
        ;; (flags       (merlin-lookup 'flags merlin-buffer-configuration))
        (process-environment (cl-copy-list process-environment))
        (dot-merlin  (merlin-lookup 'dot-merlin merlin-buffer-configuration))
        ;; FIXME use logfile
        ;; (logfile     (or (merlin-lookup 'logfile merlin-buffer-configuration)
        ;;                  merlin-logfile))
        (extensions  (merlin--map-flatten (lambda (x) (cons "-extension" x))
                                          merlin-buffer-extensions))
        (packages    (merlin--map-flatten (lambda (x) (cons "-I" x))
                                          merlin-buffer-packages-path))
        (filename    (buffer-file-name (buffer-base-buffer))))
    ;; Update environment
    (dolist (binding (merlin-lookup 'env merlin-buffer-configuration))
      (let* ((equal-pos (string-match-p "=" binding))
             (prefix (if equal-pos
                       (substring binding 0 (1+ equal-pos))
                       binding))
             (is-prefix (lambda (x) (string-prefix-p prefix x))))
        (setq process-environment (cl-delete-if is-prefix process-environment))
        (when equal-pos
          (setq process-environment (cons binding process-environment)))))
    ;; Compute verbosity
    (when (eq merlin-verbosity-context t)
      (setq merlin-verbosity-context (cons command args)))
    (if (not merlin-verbosity-context)
        (setq merlin--verbosity-cache nil)
      (if (equal merlin-verbosity-context (car-safe merlin--verbosity-cache))
          (setcdr merlin--verbosity-cache (1+ (cdr merlin--verbosity-cache)))
        (setq merlin--verbosity-cache (cons merlin-verbosity-context 0))))
    ;; Compute full command line.
    (setq args (merlin--map-flatten-to-string
                "server" command "-protocol" "sexp"
                (when dot-merlin
                  (list "-dot-merlin" dot-merlin))
                ;; Is debug mode enabled
                (when merlin-debug '("-log-file" "-"))
                ;; If command is repeated, increase verbosity
                (when merlin-verbosity-context
                  (list "-verbosity" (cdr merlin--verbosity-cache)))
                packages
                extensions
                (unless (string-equal merlin-buffer-flags "")
                  (cons "-flags" merlin-buffer-flags))
                (when filename
                  (cons "-filename" filename))
                args))
    ;; Log last commands
    (setq merlin-debug-last-commands
          (cons (cons (cons binary args) nil) merlin-debug-last-commands))
    (let ((cdr (nthcdr 5 merlin-debug-last-commands)))
      (when cdr (setcdr cdr nil)))
    ;; Call merlin process
    (setcdr (car merlin-debug-last-commands) (merlin--call-process binary args))))

(defun merlin-client-logger (binary cmd timing result)
  (when merlin-client-log-function
    (funcall merlin-client-log-function binary cmd timing result)))

(defun merlin-call (command &rest args)
  "Execute a command and parse output: return an sexp on success or throw an error"
  (let* ((binary (merlin-command))
         (result (merlin--call-merlin command args)))
    (condition-case err
        (setq result (car (read-from-string result)))
      (error
        (merlin-client-logger binary command -1 "failure")
        (error "merlin: error %s trying to parse answer: %s"
               err result))
      (quit
        (merlin-client-logger binary command -1 "interrupted")))
    (let* ((notifications (cdr-safe (assoc 'notifications result)))
           (timing (cdr-safe (assoc 'timing result)))
           (class (cdr-safe (assoc 'class result)))
           (value (cdr-safe (assoc 'value result))))
      (merlin-client-logger binary command timing class)
      (dolist (notification notifications)
        (message "(merlin) %s" notification))
      (pcase class
        ("return" value)
        ("failure" (error "merlin-mode failure: %s" value))
        ("error" (error "merlin: %s" value))
        (_ (error "unknown answer: %S:%S" class value))))))

(define-obsolete-function-alias 'merlin/call 'merlin-call "2021-01-27")

(defun merlin-stop-server ()
  "Shutdown merlin server."
  (interactive)
  (unless merlin-mode (message "Buffer is not managed by merlin."))
  (when merlin-mode
    (merlin--call-merlin "stop-server")
    ;; These are buffer-local variables, so reset them in all buffers.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (kill-local-variable 'merlin-buffer-configuration)
        (kill-local-variable 'merlin-erroneous-buffer)))))

;;;;;;;;;;;;;;;;;;;;
;; FILE SWITCHING ;;
;;;;;;;;;;;;;;;;;;;;

(defun merlin-switch-list-by-ext (&rest exts)
  "List filenames ending by any of EXTS in the path."
  (merlin-call "list-modules"
               (merlin--map-flatten (lambda (x) (cons "-ext" x)) exts)))

(defun merlin-switch-to (name &rest exts)
  "Switch to NAME.EXTS."
  (let ((file (merlin-call "path-of-source"
               (merlin--map-flatten
                 (lambda (ext) (cons "-file" (concat name ext))) exts))))
    (when file (merlin-find-file file))))

(defun merlin-switch-to-ml (name)
  "Switch to the ML file corresponding to the module NAME (fallback to MLI if no ML is provided)."
  (interactive (list (ido-completing-read "Module: "
                                          (merlin-switch-list-by-ext '(".ml" ".mli")))))
  (merlin-switch-to name '(".ml" ".mli")))

(defun merlin-switch-to-mli (name)
  "Switch to the MLI file corresponding to the module NAME (fallback to ML if no MLI is provided)."
  (interactive (list (ido-completing-read "Module: "
                                          (merlin-switch-list-by-ext '(".mli" ".ml")))))
  (merlin-switch-to name '(".mli" ".ml")))

;;;;;;;;;;;;;;;;;;
;; ERROR REPORT ;;
;;;;;;;;;;;;;;;;;;

(defvar-local merlin--last-edit nil
  "Coordinates (start . end) of last edition or nil, to prevent error messages from flickering when cursor is around edition.")

(defun merlin--on-edit (start end _length)
  "Memorize coordinates of last edition to avoid flickering error messages around the cursor"
  (setq merlin--last-edit (cons start end)))

(defun merlin--error-position-delta (point err)
  "Distance between point and error."
  (setq err (cdr (assoc 'bounds err)))
  (cond ((< point (car err)) (cons (- (car err) point) 0))
        ((> point (cdr err)) (cons (- point (cdr err)) 0))
        (t (cons 0 (min (- (cdr err) point) (- point (car err)))))))

(defun merlin--error-at-position (point errors)
  "Returns error from ERRORS list most relevant at POINT"
  (let ((err nil) (d nil))
    (dolist (err- errors err)
      (let ((d- (merlin--error-position-delta point err-)))
        (when (or (not err) (< (car d-) (car d))
                  (and (= (car d-) (car d)) (< (cdr d-) (cdr d))))
          (setq d d-) (setq err err-))))))

(defun merlin-show-error-on-current-line ()
  "Show the error of the current line in the echo area.  If there is no error, do nothing."
  (when (and merlin-mode (not (current-message)))
    (let* ((errors (overlays-in (line-beginning-position) (line-end-position)))
           (err nil))
      (when (or (not merlin--last-edit)
                (not (or (= (point) (car merlin--last-edit))
                         (= (point) (cdr merlin--last-edit)))))
        (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors)))
        (setq err (merlin--error-at-position (point) errors))
        (when err (message "%s" (cdr (assoc 'message err))))))))

(defun merlin--overlay-next-property-set (point prop &optional limit)
  "Find next point where PROP is set (like next-single-char-property-change but ensure that prop is not-nil)."
  (setq point (next-single-char-property-change point prop nil limit))
  (unless (cl-find-if (lambda (a) (overlay-get a prop)) (overlays-at point))
    (setq point (next-single-char-property-change point prop nil limit)))
  point)

(defun merlin--overlay-previous-property-set (point prop &optional limit)
  "Find previous point where PROP is set (like previous-single-char-property-change but ensure that prop is not-nil)."
  (setq point (previous-single-char-property-change point prop nil limit))
  (unless (cl-find-if (lambda (a) (overlay-get a prop)) (overlays-at point))
    (setq point (previous-single-char-property-change point prop nil limit)))
  point)

(defun merlin--has-error-group-overlay-at-point (point group)
  (cl-some (lambda (err) (eq (overlay-get err 'merlin-error-group) group))
           (overlays-at point)))

(defun merlin--error-group-next (point group &optional limit)
  (let ((point (merlin--overlay-next-property-set point 'merlin-pending-error limit)))
    (when group
      (while (not (or (eq point (point-max))
                      (merlin--has-error-group-overlay-at-point point group)))
        (setq point (merlin--overlay-next-property-set point 'merlin-pending-error limit))))
    point))

(defun merlin--error-group-prev (point group &optional limit)
  (let ((point (merlin--overlay-previous-property-set point 'merlin-pending-error limit)))
    (when group
      (while (not (or (eq point (point-min))
                      (merlin--has-error-group-overlay-at-point point group)))
        (setq point (merlin--overlay-next-property-set point 'merlin-pending-error limit))))
    point))

(defun merlin--errors-at-position (point)
  (remove nil (mapcar 'merlin--overlay-pending-error (overlays-at point))))

(defun merlin--error-prev-cycle (group)
  "Returns previous error, cycling when reaching beginning of buffer"
  (let ((point (point)) (errors nil) (err nil))
    (setq point (merlin--error-group-prev point group))
    (unless (eq point (point)) (setq errors (merlin--errors-at-position point))
    (unless errors
      (setq point (merlin--error-group-prev (point-max) group (point)))
      (setq errors (merlin--errors-at-position point)))
    (setq err (merlin--error-at-position point errors))
    (if err (cons point err) nil))))

(defun merlin--error-next-cycle (group)
  "Returns next error, cycling when reaching end of buffer"
  (let ((point (point)) (errors nil) (err nil))
    (setq point (merlin--error-group-next point group))
    (when (eq point (point-max))
      (setq point (point-min))
      (setq errors (merlin--errors-at-position point))
      (unless errors
        (setq point (merlin--error-group-next (point-min) group (point)))))
    (unless errors
      (setq errors (merlin--errors-at-position point)))
    (setq err (merlin--error-at-position point errors))
    (if err (cons point err) nil)))

(defun merlin--after-save ()
  (when (and merlin-mode merlin-error-after-save) (merlin-error-check)))

(defadvice basic-save-buffer (after merlin--after-save activate)
  "The save hook is called only if buffer was modified, but user might want fresh errors anyway"
  (merlin--after-save))

(defun merlin-error-prev (&optional group)
  "Jump back to previous error."
  (interactive)
  (let ((old-errors merlin-erroneous-buffer))
    (merlin--error-check nil)
    (let ((err (merlin--error-prev-cycle group)))
      (unless (or err merlin-erroneous-buffer) (message "No errors"))
      (when err
        (if (and merlin-error-check-then-move
                 (not (equal old-errors merlin-erroneous-buffer)))
            (message "(%d pending errors, use %s to jump)"
                     (length merlin-erroneous-buffer)
                     (substitute-command-keys "\\[merlin-error-prev]"))
          (goto-char (car err))
          (message "%s" (cdr (assoc 'message (cdr err))))
          (merlin--highlight (cdr (assoc 'bounds (cdr err))) 'next-error))))))

(defun merlin-error-next (&optional group)
  "Jump to next error."
  (interactive)
  (let ((old-errors merlin-erroneous-buffer))
    (merlin--error-check nil)
    (let ((err (merlin--error-next-cycle group)))
      (unless (or err merlin-erroneous-buffer) (message "No errors"))
      (when err
        (if (and merlin-error-check-then-move
                 (not (equal old-errors merlin-erroneous-buffer)))
            (message "(%d pending errors, use %s to jump)"
                     (length merlin-erroneous-buffer)
                     (substitute-command-keys "\\[merlin-error-next]"))
          (goto-char (car err))
          (message "%s" (cdr (assoc 'message (cdr err))))
          (merlin--highlight (cdr (assoc 'bounds (cdr err))) 'next-error))))))

(defun merlin-error-next-in-group ()
  "Jump to next error in same group, if any, next error otherwise."
  (interactive)
  (let ((err (merlin--error-at-position
               (point) (merlin--errors-at-position (point)))))
    (merlin-error-next (when err (overlay-get err 'merlin-error-group)))))

(defun merlin-error-prev-in-group ()
  "Jump to previous error in same group, if any, previous error otherwise."
  (interactive)
  (let ((err (merlin--error-at-position
               (point) (merlin--errors-at-position (point)))))
    (merlin-error-prev (when err (overlay-get err 'merlin-error-group)))))

(defun merlin--error-warning-p (msg)
  "Tell if the message MSG is a warning."
  (string-match-p "^Warning" msg))

(defun merlin-error-reset ()
  "Clear error list."
  (interactive)
  (setq merlin-erroneous-buffer nil)
  (remove-overlays nil nil 'merlin-kind 'error))

(defun merlin--overlay-pending-error (overlay)
  "Returns non-nil if OVERLAY is about a pending error."
  (if overlay (overlay-get overlay 'merlin-pending-error) nil))

(defun merlin--kill-error-if-edited (overlay is-after _beg _end &optional _length)
  "Remove an error from the pending error lists if it is edited by the user."
  (when is-after (delete-overlay overlay)))

(defun merlin--transform-add-error-bounds (err)
  (let ((bounds (merlin--make-bounds err))
        (subs (cdr-safe (assoc 'sub err))))
    (when merlin-error-on-single-line
      (setq bounds (cons (car bounds)
                         (min (cdr bounds)
                              (save-excursion
                                (goto-char (car bounds))
                                (line-end-position))))))
    (when (= (car bounds) (cdr bounds))
      (setq bounds (if (> (car bounds) (point-min))
                       (cons (1- (car bounds)) (cdr bounds))
                     (cons (car bounds) (1+ (cdr bounds))))))
    (setq bounds (cons (copy-marker (car bounds))
                       (copy-marker (cdr bounds))))
    (cl-acons 'sub (mapcar 'merlin--transform-add-error-bounds subs)
              (cl-acons 'bounds bounds err))))

(defun merlin-transform-display-errors (errors)
  "Populate the error list with ERRORS, transformed into an emacs-friendly
form. Do display of error list."
  (setq errors (mapcar 'merlin--transform-add-error-bounds errors))
  (dolist (main errors)
    (let ((subs (cdr-safe (assoc 'sub main))))
      (dolist (err (cons main subs))
        (let* ((bounds (cdr (assoc 'bounds err)))
               (overlay (make-overlay (car bounds) (cdr bounds))))
        (overlay-put overlay 'merlin-kind 'error)
        (overlay-put overlay 'merlin-pending-error err)
        (overlay-put overlay 'merlin-error-group main)
        (push #'merlin--kill-error-if-edited
              (overlay-get overlay 'modification-hooks))
        (when (and merlin-error-in-fringe
                   (not (and (eq err main) subs)))
          (if (merlin--error-warning-p (cdr (assoc 'message err)))
              (merlin-add-display-properties overlay
                                             'question-mark
                                             "?"
                                             'merlin-compilation-warning-face)
            (merlin-add-display-properties overlay
                                           'exclamation-mark
                                           "!"
                                           'merlin-compilation-error-face)))))))
  errors)

(defun merlin--error-check (view-errors-p)
  "Check for errors.
Return t if there were not any or nil if there were.  Moreover, it displays the
errors in the fringe.  If VIEW-ERRORS-P is non-nil, display a count of them."
  (merlin-error-reset)
  (let* ((errors (merlin-call "errors"))
         (no-loc (cl-remove-if (lambda (e) (assoc 'start e)) errors)))
    (setq errors (cl-remove-if-not (lambda (e) (assoc 'start e)) errors))
    (unless merlin-report-warnings
      (setq errors (cl-remove-if (lambda (e)
                                   (or
                                    (eq (cdr-safe (assoc 'message e)) "warning")
                                    (merlin--error-warning-p (cdr (assoc 'message e)))))
                                 errors)))
    (setq merlin-erroneous-buffer (or errors no-loc))
    (dolist (e no-loc)
      (message "%s" (cdr (assoc 'message e))))
    (merlin-transform-display-errors errors)
    (when view-errors-p
      (let ((prefix (current-message)))
        (setq prefix (if prefix (concat prefix " ") ""))
        (if merlin-erroneous-buffer
          (message "%s(%d pending errors, use %s to jump)"
                   prefix
                   (length errors)
                   (substitute-command-keys "\\[merlin-error-next]"))
          (message "%sNo errors" prefix))))))

(defun merlin-error-after-save ()
  "Determine whether the buffer should be checked for errors depending on the value of merlin-error-after-save setting."
  (cond
    ((equal merlin-error-after-save t) t)
    ((equal merlin-error-after-save nil) nil)
    ((and (listp merlin-error-after-save)
          (buffer-file-name (buffer-base-buffer)))
     (member (file-name-extension (buffer-file-name (buffer-base-buffer)))
             merlin-error-after-save))))

(defun merlin-toggle-view-errors ()
  "Toggle the viewing of errors in the buffer."
  (interactive)
  (setq merlin-error-after-save (not (merlin-error-after-save)))
  (if (merlin-error-after-save)
      (progn
        (merlin--after-save)
        (message "Errors are now reported. Use %s to stop reporting them."
                 (substitute-command-keys "\\[merlin-toggle-view-errors]")))
    (progn
      (merlin-error-reset)
      (message "Errors are not reported anymore. Use %s to start again reporting them."
               (substitute-command-keys "\\[merlin-toggle-view-errors]")))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION HELPERS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-completion-entry-short-description (entry)
  "Return a short string describing the content a completion entry (e.g kind of
identifier, type of a value, etc)."
  (let* ((kind (cdr (assoc 'kind entry)))
         (desc (or  (cdr (assoc 'desc entry)) (cdr (assoc 'type entry))))
         (type (cond ((member kind '("Module" "module")) " <module>")
                     ((string-equal kind "Type") (format " [%s]" desc))
                     (t desc))))
    (replace-regexp-in-string "[\n ]+" " " type)))

(defun merlin-completion-entry-text (compl-prefix entry)
  "Return the text that should replace COMPL-PREFIX in the buffer if the user
chooses this completion entry.
COMPL-PREFIX is the prefix that was used to start completion."
  (let ((entry-name (cdr (assoc 'name entry))))
    (if merlin--dwimed entry-name (concat compl-prefix entry-name))))

(defun merlin-completion-prefix (ident)
  "Compute the prefix of IDENT.  The prefix of `Foo.bar' is `Foo.' and the
prefix of `bar' is `'."
  (car (merlin-completion-split-ident ident)))

(defun merlin-completion-split-ident (ident)
  "Split IDENT into a (cons prefix suffix). See merlin-completion-prefix."
  (let* ((l (split-string ident "\\."))
         (s (mapconcat 'identity (butlast l) "."))
         (suffix (if l (car (last l)) ident))
         (prefix (if (string-equal s "") s (concat s "."))))
    (cons prefix suffix)))

(defun merlin--completion-prepare-labels (labels prefix)
  ;; Remove non-matching entry, adjusting optional labels if needed
  (cl-loop for x in labels
           for name = (cdr (assoc 'name x))
           when (or (string-prefix-p prefix name)
                    (when (equal (aref name 0) ??)
                      (aset name 0 ?~)
                      (string-prefix-p prefix name)))
           collect (append x '((kind . "Label") (info . nil)))))

(defun merlin-complete (ident)
  "Return the data for completion of IDENT, i.e. a list of tuples of the form
  '(NAME TYPE KIND INFO)."
  (setq-local merlin--dwimed nil)
  (let* ((merlin-verbosity-context t) ; increase verbosity level if necessary
         (ident- (merlin-completion-split-ident ident))
         (suffix (cdr ident-))
         (data   (merlin-call "complete-prefix"
                              "-position" (merlin-unmake-point (point))
                              "-prefix" ident
                              "-doc" (if merlin-completion-with-doc "y" "n")))
         ;; all classic entries
         (entries (cdr (assoc 'entries data)))
         ;; context is 'null or ('application ...)
         (context (cdr (assoc 'context data)))
         (application (and (listp context)
                           (equal (car context) "application")
                           (cadr context)))
         ;; Argument-type
         (expected-ty (and application
                           (not (string-equal "'_a"
                                  (cdr (assoc 'argument_type application))))
                           (cdr (assoc 'argument_type application))))
         ;; labels
         (labels (and application (cdr (assoc 'labels application)))))
    (setq labels (merlin--completion-prepare-labels labels suffix))
    ;; DWIM completion
    (when (and merlin-completion-dwim (not labels) (not entries))
      (setq data (merlin-call "expand-prefix"
                              "-position" (merlin-unmake-point (point))
                              "-prefix" ident))
      (setq entries (cdr (assoc 'entries data)))
      (setq-local merlin--dwimed t))
    ;; Concat results
    (let ((result (append labels entries)))
      (if expected-ty
          (cl-loop for x in result
                   collect (append x `((argument_type . ,expected-ty))))
        result))))

;; FIXME: merlin shouldn't rely on editor to compute bounds
(defun merlin-bounds-of-ocaml-atom-at-point ()
  "Return the start and end points of an ocaml atom near point.
An ocaml atom is any string containing [a-z_0-9A-Z`.]."
  (save-excursion
    (skip-chars-backward "a-z0-9A-Z_'.")
    (skip-chars-backward "~?`" (1- (point)))
    (save-match-data
      (if (or (looking-at "[~?`]?['a-z_0-9A-Z.]*['a-z_A-Z0-9]")
              (looking-at "[~?`]"))
          (cons (point) (match-end 0)) ; returns the bounds
        nil)))) ; no atom at point

(put 'ocaml-atom 'bounds-of-thing-at-point
     'merlin-bounds-of-ocaml-atom-at-point)

(defun merlin-completion-bounds ()
  "Returns a pair (start . end) of the content to complete"
  (let ((bounds (bounds-of-thing-at-point 'ocaml-atom)))
    (cons (if bounds (car bounds) (point))
          (point))))

;;;;;;;;;;;;;;;;;;;;;
;; POLARITY SEARCH ;;
;;;;;;;;;;;;;;;;;;;;;

(defun merlin--search (query)
  (merlin-call "search-by-polarity"
               "-query" query
               "-position" (merlin-unmake-point (point))))

(defun merlin-search (query)
  (interactive "sSearch pattern: ")
  (let* ((result (merlin--search query))
         (entries (cdr (assoc 'entries result)))
         (transform
          (lambda (entry)
            (let ((text (merlin-completion-entry-text "" entry))
                  (desc (merlin-completion-entry-short-description entry)))
              (vector (concat text " : " desc)
                      `(lambda () (insert ,text)))))))
    (popup-menu (easy-menu-create-menu "Results" (mapcar transform entries)))))

;;;;;;;;;;;;;;;;;
;; TYPE BUFFER ;;
;;;;;;;;;;;;;;;;;

(defun merlin--is-short (text)
  (let ((count 0)
        (pos   0))
    (save-match-data
      (while (and (<= count 8)
                  (string-match "\n" text pos))
        (setq pos (match-end 0))
        (setq count (1+ count))))
    (<= count 8)))

(defvar merlin-types-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" nil)
    map)
  "Keymap for types buffer.")

(defun merlin-display-in-type-buffer (text)
  "Change content of type-buffer."
  (let ((curr-dir default-directory))
    (with-current-buffer (get-buffer-create merlin-type-buffer-name)
      (when (member major-mode '(nil fundamental-mode))
        ;; Guess value for merlin-favourite-caml-mode
        (let ((caml-mode (or merlin-favourite-caml-mode
                             merlin-guessed-favorite-caml-mode)))
          (when caml-mode
            (with-demoted-errors "Error when setting up merlin type-buffer: %S"
              (funcall caml-mode)))))
      (read-only-mode 0)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (read-only-mode 1)
      (use-local-map merlin-types-buffer-map)
      ;; finally make sure that the type buffer directory is the same as the last
      ;; (ml) buffer we were in.
      ;; Indeed if people move to that buffer and start looking for a file we
      ;; want them to be in the directory they were in when they last requested a
      ;; type, not in the directory they were in when they first requested a
      ;; type (for long lived emacs sessions that directory might not even exist
      ;; anymore).
      (setq default-directory curr-dir))))

(define-obsolete-function-alias 'merlin/display-in-type-buffer 'merlin-display-in-type-buffer "2021-01-27")


;;;;;;;;;;;;;;;;;;;;;;;
;; EXPRESSION TYPING ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--type-expression (exp callback-if-success &optional _callback-if-exn)
  "Get the type of EXP inside the local context."
  (when exp
    (funcall callback-if-success
             (merlin-call "type-expression"
                          "-position" (merlin-unmake-point (point))
                          "-expression" exp))
    ;; FIXME: callback-if-exn
    ))

(defun merlin--type-display (bounds type &optional quiet)
  "Display the type TYPE of the expression occurring at BOUNDS.
If QUIET is non nil, then an overlay and the merlin types can be used."
  (if (not type)
      (unless quiet (message "<no information>"))
    (merlin-display-in-type-buffer type)
    (if (merlin--is-short type)
        (message "%s"
                 (with-current-buffer merlin-type-buffer-name
                   (font-lock-fontify-region (point-min) (point-max))
                   (buffer-string)))
      (display-buffer merlin-type-buffer-name))
    (if (and (not quiet) bounds)
        (merlin--highlight bounds 'merlin-type-face))))

(defun merlin--type-region ()
  "Show the type of the region."
  (let*
      ((substring  (merlin-buffer-substring (region-beginning) (region-end)))
       (on-success (lambda (type) (merlin--type-display nil type nil)))
       (on-error   (lambda (err)
                     (let ((msg (assoc 'message err))
                           (typ (assoc 'type err)))
                       (cond ((and typ (equal (cdr typ) "parser"))
                              (message "Error: the content of the region failed to parse."))
                             (msg (message "Error: %s" (cdr msg)))
                             (t
                              (message "Unexpected error")))))))
    (merlin--type-expression substring on-success on-error)))

(defun merlin-type-expr (exp)
  "Prompt the user for expression EXP, then show its type."
  (interactive "s# ")
  (let ((on-success (lambda (type) (merlin--type-display nil type nil)))
        (on-error   (lambda (err)
                      (let ((msg (assoc 'message err)))
                        (if msg (message "Error: %s" (cdr msg))
                          (message "unknown error"))))))
    (merlin--type-expression exp on-success on-error)))

(defvar merlin-type-enclosing-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-<up>") #'merlin-type-enclosing-go-up)
    (define-key keymap (kbd "C-<down>") #'merlin-type-enclosing-go-down)
    (define-key keymap (kbd "C-d") #'merlin-destruct-enclosing)
    (define-key keymap (kbd "C-w") #'merlin-copy-enclosing)
    keymap)
  "The local map to navigate type enclosing.")

(defun merlin--type-enclosing-reset ()
  "Clear enclosing information, necessary for destruct"
  (setq merlin-enclosing-types nil)
  (setq merlin-enclosing-offset -1))

(defun merlin--type-enclosing-reset-hooked ()
  "Reimplement on-exit logic from set-temporary-overlay-map for emacs pre 24.4"
  (let ((map merlin-type-enclosing-map))
    (unless (or (not (eq map (cadr overriding-terminal-local-map)))
                (eq this-command (lookup-key map (this-command-keys-vector))))
      (merlin--type-enclosing-reset)
      (remove-hook 'pre-command-hook #'merlin--type-enclosing-reset-hooked))))

(defun merlin--type-enclosing-text (item)
  (if (stringp (car item))
      (car item)
    (with-demoted-errors "Error retrieving type enclosing: %S"
      (let* ((key (car item))
             (index     (elt key 0))
             (position  (elt key 1))
             (tail      (elt key 2))
             (verbosity (elt key 3))
             (types (merlin-call
                      "type-enclosing" "-position" position "-index" index
                      (when verbosity (cons "-verbosity" verbosity))))
             (obj (elt types index))
             (type (cdr (assoc 'type obj))))
        (setcar item (concat type tail)))
      (car item))))

(defun merlin--type-enclosing-query ()
  "Get the enclosings around point from merlin and sets MERLIN-ENCLOSING-TYPES."
  (merlin--type-enclosing-reset)
  (let* ((merlin-verbosity-context t) ; increase verbosity level if necessary
         (position (merlin-unmake-point (point)))
         (verbosity (cdr-safe merlin--verbosity-cache))
         (types (merlin-call "type-enclosing" "-position" position "-index" 0))
         (types (ignore-errors
                  (mapcar (lambda (obj)
                            (let* ((tail (cdr (assoc 'tail obj)))
                                   (tail (cond ((equal tail "position")
                                                " (* tail position *)")
                                               ((equal tail "call")
                                                " (* tail call *)")
                                               (t "")))
                                   (type (cdr (assoc 'type obj))))
                              (cons (if (stringp type) (concat type tail)
                                      (list type position tail verbosity))
                                    (merlin--make-bounds obj))))
                          types)))
         (types (delq nil types)))
    (when types
      (setq merlin-enclosing-types types)
      (setq merlin-enclosing-offset -1)
      merlin-enclosing-types)))

(defun merlin--type-enclosing-go ()
  "Highlight the given corresponding enclosing data (of the form (TYPE . BOUNDS)."
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (if (cddr data)
        (merlin--type-display (cdr data) (merlin--type-enclosing-text data)))))

(defun merlin-type-enclosing-go-up ()
  "Go up in the enclosing type list."
  (interactive)
  (when merlin-enclosing-types
    (if (>= merlin-enclosing-offset (1- (length merlin-enclosing-types)))
        (setq merlin-enclosing-offset -1))
    (setq merlin-enclosing-offset (1+ merlin-enclosing-offset))
    (merlin--type-enclosing-go)))

(defun merlin-type-enclosing-go-down ()
  "Go down in the enclosing type list."
  (interactive)
  (when merlin-enclosing-types
    (if (<= merlin-enclosing-offset 0)
        (setq merlin-enclosing-offset (length merlin-enclosing-types)))
    (setq merlin-enclosing-offset (1- merlin-enclosing-offset))
    (merlin--type-enclosing-go)))

(defun merlin-copy-enclosing ()
  (interactive)
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (when (cddr data)
      (setq data (merlin--type-enclosing-text data))
      (message "Copied %s to kill-ring" data)
      (kill-new data))))

(defun merlin--type-enclosing-after ()
  (when (and (fboundp 'set-temporary-overlay-map)
             merlin-arrow-keys-type-enclosing)
    (if (version< emacs-version "24.4")
        (progn
          (set-temporary-overlay-map merlin-type-enclosing-map t)
          (add-hook 'pre-command-hook #'merlin--type-enclosing-reset-hooked))
      (set-temporary-overlay-map merlin-type-enclosing-map t
                                 'merlin--type-enclosing-reset))))

(defun merlin-type-enclosing ()
  "Print the type of the expression under point (or of the region, if it exists).
If called repeatedly, increase the verbosity of the type shown."
  (interactive)
  (if (region-active-p)
      (merlin--type-region)
    (when (merlin--type-enclosing-query)
      (merlin-type-enclosing-go-up)
      (merlin--type-enclosing-after))))

(defun merlin--find-extents (list low high)
  "Return the smallest extent in LIST that LOW and HIGH fit
strictly within, or nil if there is no such element."
  (cl-find-if (lambda (extent)
                (let ((start (merlin--point-of-pos (assoc 'start extent)))
                      (end (merlin--point-of-pos (assoc 'end extent))))
                  (or (and (> low start)
                           (<= high end))
                      (and (< high end)
                           (>= low start)))))
              list))

(defun merlin-enclosing-expand ()
  "Select the construct enclosing point (or the region, if it is active)."
  (interactive)
  (let* ((enclosing-extents
           (merlin-call "enclosing"
                        "-position" (merlin-unmake-point (point))))
         (extents (if (use-region-p)
                      (merlin--find-extents enclosing-extents
                                            (region-beginning)
                                            (region-end))
                    (cl-first enclosing-extents))))
    (if (not extents)
              (error "No enclosing construct")
      (merlin--goto-point (cdr (assoc 'start extents)))
      (push-mark (merlin--point-of-pos (cdr (assoc 'end extents)))
                 t t))))

;;;;;;;;;;;
;; HOLES ;;
;;;;;;;;;;;

(defun merlin--holes ()
  "Query the list of holes (and their types)"
  (merlin-call "holes"))

(defun merlin--first-hole-aux (holes current-point comp)
  "Returns the first `hole` of the list such that
    `(funcall comp hole current-point)`"
  (when holes
    (let* ((head (car holes))
           (tail (cdr holes))
           (start (merlin-lookup 'start head))
           (hole-point (merlin-make-point start)))
      (if (funcall comp hole-point current-point)
        head
        (merlin--first-hole-aux tail current-point comp)))))

(defun merlin--first-hole (holes current-point comp)
  "Returns the first `hole` of the list that such that
    `(funcall comp hole current-point)`. If no hole match
    that condition the first one of the list is returned."
  (let ((hole (merlin--first-hole-aux holes current-point comp)))
    (if hole hole (car holes))))

(defun merlin-previous-hole ()
  "Jump to the previous hole and print its type"
  (interactive)
  (let* ((current-point (point))
         (holes (reverse (merlin--holes)))
         (hole (merlin--first-hole holes current-point '<)))
    (when hole
      (progn
        (merlin--goto-point (merlin-lookup 'start hole))
        (message "%s" (merlin-lookup 'type hole))))))

(defun merlin--next-hole-between (pmin pmax)
  "Jump to the next hole and print its type only if it is in the given range"
  (let* ((current-point (point))
         (hole (merlin--first-hole (merlin--holes) current-point '>)))
    (when hole
      (let* ((start (merlin-lookup 'start hole))
             (typ (merlin-lookup 'type hole))
             (hole-point (merlin-make-point start)))
        (if (and
              (>= hole-point pmin)
              (<= hole-point pmax))
          (progn
            (merlin--goto-point start)
            (message "%s" typ)))))))

(defun merlin-next-hole ()
  "Jump to the next hole and print its type"
  (interactive)
  (let* ((current-point (point))
         (hole (merlin--first-hole (merlin--holes) current-point '>)))
    (when hole
      (progn
        (merlin--goto-point (merlin-lookup 'start hole))
        (message "%s" (merlin-lookup 'type hole))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESTRUCT / CASE ANALYSIS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--replace-buff-portion (start stop txt)
  (let ((start (merlin--point-of-pos start))
        (stop  (merlin--point-of-pos stop)))
    (progn
      (save-excursion
        (delete-region start stop)
        (goto-char start)
        (insert txt)
        (indent-region start (point)))
      (merlin--next-hole-between start (+ start (length txt))))))

(defun merlin--destruct-bounds (bounds)
  "Execute a case analysis on BOUNDS"
  (let ((result (merlin-call "case-analysis"
                            "-start" (merlin-unmake-point (car bounds))
                            "-end" (merlin-unmake-point (cdr bounds)))))
    (when result
      (let* ((loc   (car result))
             (start (cdr (assoc 'start loc)))
             (stop  (cdr (assoc 'end loc))))
        (merlin--replace-buff-portion start stop (cadr result))))
    (merlin--type-enclosing-reset)))

(defun merlin-destruct-enclosing ()
  "Case analyse the current type enclosing"
  (interactive)
  (merlin--destruct-bounds
    (cdr (elt merlin-enclosing-types merlin-enclosing-offset))))

(defun merlin-destruct ()
  "Case analyse the current point or region"
  (interactive)
  (merlin--destruct-bounds (if (region-active-p)
                             (cons (region-beginning) (region-end))
                             (cons (point) (point)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE, PROJECT AND FLAGS MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-get-packages ()
  "Get the list of available findlib package."
  (let* ((packages-string (shell-command-to-string "ocamlfind list"))
        (packages-list (split-string packages-string "\n")))
    (mapcar 'car (mapcar 'split-string packages-list))))

(defun merlin--project-get ()
  "Returns a pair of two string lists (dot_merlins . failures) with a list of
.merlins file loaded and a list of error messages, if any error occurred during
loading"
  (let ((ret (merlin-call "check-configuration")))
    (setq merlin--project-cache
          (cons (cdr (assoc 'dot_merlins ret))
                (cdr (assoc 'failures ret))))))

(defun merlin-use (&rest pkgs)
  "Load PKGS in merlin."
  (interactive
   (list (let ((crm-separator "[ 	]*[, 	][ 	]*")
               (crm-local-completion-map
                (merlin--completion-map-with-space crm-local-completion-map)))
           (completing-read-multiple
            "Packages to use: " (merlin-get-packages) nil nil
            (mapconcat 'identity merlin-buffer-packages " ")))))
  (setq merlin-buffer-packages
        (delete-dups (merlin--map-flatten 'identity pkgs)))
  (let* ((arguments (cons "ocamlfind query" merlin-buffer-packages))
        (command (mapconcat 'identity arguments " "))
        (paths (shell-command-to-string command)))
    (setq merlin-buffer-packages-path (split-string paths "\n")))
  (merlin-error-reset)
  (merlin-configuration-check t))

(defun merlin-extensions (&rest extensions)
  "Enable EXTENSIONS in merlin."
  (interactive
   (list (completing-read-multiple
          "Enabled extensions (separate with ','): "
          (merlin-call "extension-list") nil nil
          (mapconcat 'identity merlin-buffer-extensions ","))))
  (setq merlin-buffer-extensions
        (delete-dups (merlin--map-flatten 'identity extensions)))
  (merlin-error-reset)
  (merlin-configuration-check t))

(defun merlin-goto-project-file ()
  "Goto the merlin file corresponding to the current file."
  (interactive)
  (let ((dot_merlins (car (merlin--project-get))))
    (if (consp dot_merlins)
        (merlin-find-file (car dot_merlins))
      (message "No project file for the current buffer."))))

(defun merlin-flags (&rest flags)
  "Set user flags for current buffer."
  (interactive (list
                (let ((crm-separator " ")
                      (crm-local-completion-map
                       (merlin--completion-map-with-space crm-local-completion-map)))
                  (completing-read-multiple
                   "Flags: " (merlin-call "flags-list") nil nil
                   merlin-buffer-flags))))
  (setq merlin-buffer-flags
        (mapconcat 'identity (merlin--map-flatten 'identity flags) " "))
  (merlin-error-reset)
  (merlin-configuration-check t))

;;;;;;;;;;;;
;; LOCATE ;;
;;;;;;;;;;;;

(defun merlin-call-locate (&optional ident)
  "Locate the identifier IDENT at point."
  (let ((result (merlin-call "locate"
                             (when ident (list "-prefix" ident))
                             "-position" (merlin-unmake-point (point))
                             "-look-for" merlin-locate-preference)))
    (unless result
      (error "Not found. (Check *Messages* for potential errors)"))
    (unless (listp result)
      (error "%S" result))
    result))

(defun merlin--locate-result (result)
  "Default actions after getting results from locate"
  (merlin--goto-file-and-point result)
  (when merlin-type-after-locate (merlin-type-enclosing)))

(defun merlin-locate-ident (ident)
  "Locate the inputed identifier"
  (interactive "s> ")
  (merlin--locate-result (merlin-call-locate ident)))

(defun merlin-locate ()
  "Locate the identifier under point"
  (interactive)
  (merlin--locate-result (merlin-call-locate)))

(defun merlin-pop-stack ()
  "Go back to the last position where the user did a locate."
  (interactive)
  (let ((r (pop merlin-position-stack)))
    (cond ((not r) (message "empty stack"))
          ((equal merlin-locate-in-new-window 'never)
           (switch-to-buffer (car r)))
          ((or (equal merlin-locate-in-new-window 'always)
               (not (equal (buffer-name) (car r))))
           (select-window (display-buffer (car r)))))
    (when r (goto-char (cdr r)))))

;;;;;;;;;;
;; JUMP ;;
;;;;;;;;;;

(defun merlin-call-jump (&optional target)
  "Jump to the TARGET"
  (if (or (not target) (equal target ""))
      (setq target "fun let module match"))
  (let ((result (merlin-call "jump"
                             "-position" (merlin-unmake-point (point))
                             "-target" target)))
    (unless result
      (error "Not found. (Check *Messages* for potential errors)"))
    (unless (listp result)
      (error result))
    result))

(defun merlin-jump (&optional target)
  "Jump to enclosing fun, let, module or match.

Any combination of the above may be entered, separated by spaces, ex.:

fun let or module or module fun match

Empty string defaults to jumping to all these."
  (interactive "sfun, let, module or match > ")
  (merlin--goto-file-and-point (merlin-call-jump target)))

(defun merlin-call-phrase (target)
  "Move to next phrase (TARGET = 'next) or previous phrase (TARGET = 'prev)"
  (if (or (not target) (equal target ""))
      (setq target "fun let module match"))
  (let ((result (merlin-call "phrase"
                             "-position" (merlin-unmake-point (point))
                             "-target" target)))
    (unless result
      (error "Not found. (Check *Messages* for potential errors)"))
    (unless (listp result)
      (error result))
    result))

(defun merlin-phrase-next ()
  "Go to the beginning of the next phrase."
  (interactive)
  (merlin--goto-file-and-point (merlin-call-phrase 'next)))

(defun merlin-phrase-prev ()
  "Go to the beginning of the previous phrase."
  (interactive)
  (merlin--goto-file-and-point (merlin-call-phrase 'prev)))

;;;;;;;;;;;;;;
;; DOCUMENT ;;
;;;;;;;;;;;;;;

(defun merlin--document-pos (ident)
  "Document the identifier IDENT at point and return the result."
  (merlin-call "document"
               "-position" (merlin-unmake-point (point))
               (when ident (cons "-identifier" ident))))

(defun merlin--document-pure (&optional ident)
  "Document the identifier IDENT at point."
  (let* ((raw-doc  (merlin--document-pos ident))
         (doc      (concat "(*" raw-doc "*)")))
    (merlin-display-in-type-buffer doc)
    (with-current-buffer merlin-type-buffer-name
      (if (> (line-number-at-pos (point-max)) 8)
          (display-buffer merlin-type-buffer-name)
        (font-lock-fontify-region (point-min) (point-max))
        (message "%s" (buffer-string))))))

(defun merlin-document ()
  "Document the identifier under point"
  (interactive)
  (merlin--document-pure))

;;;;;;;;;;;;;;;;;
;; OCCURRENCES ;;
;;;;;;;;;;;;;;;;;

(defun merlin--occurrence-text (line-num marker start end source-buf)
  (concat (propertize (format "%7d:" line-num)
                      'font-lock-face 'shadow
                      'occur-prefix t
                      'occur-target marker
                      'follow-link t
                      'front-sticky t
                      'rear-nonsticky t
                      'mouse-face '(highlight))
          (propertize (replace-regexp-in-string
                       "\n"
                       "\n       :"
                       (with-current-buffer source-buf
                         (buffer-substring
                          (progn
                            (goto-char start)
                            (line-beginning-position))
                          (progn
                            (goto-char end)
                            (line-end-position)))))
                      'follow-link t
                      'mouse-face '(highlight)
                      'occur-target marker)
          (propertize "\n" 'occur-target marker)))

(defun merlin--get-occ-buff ()
  (get-buffer-create merlin-occurrences-buffer-name))

(defun merlin-occurrences-populate-buffer (lst)
  (let ((src-buff (buffer-name))
        (occ-buff (merlin--get-occ-buff))
        (positions
         (mapcar (lambda (pos)
                   (cons
                    (cons 'marker
                          (copy-marker
                           (merlin--point-of-pos (assoc 'start pos))))
                    pos))
                 lst)))
    (with-current-buffer occ-buff
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (pending-line)
            (pending-lines-text))
        (erase-buffer)
        (occur-mode)
        (insert (propertize (format "%d occurrences in buffer: %s"
                                    (length lst)
                                    src-buff)
                            'font-lock-face list-matching-lines-buffer-name-face
                            'read-only t
                            'occur-title (get-buffer src-buff)))
        (insert "\n")
        (dolist (pos positions)
          (let* ((marker (cdr (assoc 'marker pos)))
                 (start (assoc 'start pos))
                 (end (assoc 'end pos))
                 (line (cdr (assoc 'line start)))
                 (start-buf-pos (with-current-buffer src-buff
                                  (merlin--point-of-pos start)))
                 (end-buf-pos (with-current-buffer src-buff
                                (merlin--point-of-pos end)))
                 (prefix-length 8)
                 (start-offset (+ prefix-length
                                  (cdr (assoc 'col start))))
                 (lines-text
                  (if (equal line pending-line)
                      pending-lines-text
                    (merlin--occurrence-text line
                                            marker
                                            start-buf-pos
                                            end-buf-pos
                                            src-buff))))

            ;; Insert the critical text properties that occur-mode
            ;; makes use of
            (add-text-properties start-offset
                                 (+ start-offset
                                    (- end-buf-pos start-buf-pos))
                                 (list 'occur-match t
                                       'face list-matching-lines-face)
                                 lines-text)

            ;; Inserting text is delayed until non-equal lines are
            ;; found in order to accumulate multiple matches within
            ;; one line.
            (when (and pending-lines-text
                       (not (equal line pending-line)))
              (insert pending-lines-text))
            (setq pending-line line)
            (setq pending-lines-text lines-text)))

        ;; Catch final pending text
        (when pending-lines-text
          (insert pending-lines-text))
        (goto-char (point-min))))))

(defun merlin-occurrences-list (lst)
  (save-excursion
    (merlin-occurrences-populate-buffer lst)
    (cl-case merlin-occurrences-show-buffer
      ('same
       (switch-to-buffer (merlin--get-occ-buff)))
      ('other
       (switch-to-buffer-other-window (merlin--get-occ-buff)))
      (t nil))))

(defun merlin--occurrences ()
  (merlin-call "occurrences" "-identifier-at" (merlin-unmake-point (point))))

(defun merlin-occurrences ()
  "List all occurrences of identifier under cursor in buffer."
  (interactive)
  (let ((r (merlin--occurrences)))
    (when r
      (if (listp r)
          (merlin-occurrences-list r)
        (error "%s" r)))))

;;;;;;;;;;;;;;;;;;;
;; OPEN REFACTOR ;;
;;;;;;;;;;;;;;;;;;;

(defun merlin--refactor-open (mode)
  "Refactor open statement under cursor. mode can be 'qualify or 'unqualify"
  (save-excursion
    (dolist (occurrence (nreverse (merlin-call
                                   "refactor-open"
                                   "-position" (merlin-unmake-point (point))
                                   "-action" mode)))
      (let ((bounds (merlin--make-bounds occurrence))
            (content (cdr (assoc 'content occurrence))))
        (goto-char (car bounds))
        (delete-char (- (cdr bounds) (car bounds)))
        (insert content)))))

(defun merlin-refactor-open ()
  "Refactor open statement under cursor: unqualify paths"
  (interactive)
  (merlin--refactor-open 'unqualify))

(defun merlin-refactor-open-qualify ()
  "Refactor open statement under cursor: qualify paths"
  (interactive)
  (merlin--refactor-open 'qualify))

;;;;;;;;;;;;;;;;;;;;;;;
;; SEMANTIC MOVEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-error-check ()
  "Update merlin to the end-of-file, reporting errors."
  (interactive)
  (when merlin-mode (merlin--error-check t)))

(defun merlin-configuration-check (&optional only-failures)
  "Display loaded .merlin files and eventual errors."
  (interactive)
  (let* ((project (merlin--project-get))
         (dot_merlins (car project))
         (messages (cdr project))) ; failures list
    (unless only-failures
      (when merlin-buffer-configuration
        (push (format "Custom merlin setup: %S" merlin-buffer-configuration)
              messages))
      (push (format
              "Custom buffer settings:\n- packages: %S\n- flags: %S\n- extensions: %S"
              (or merlin-buffer-packages 'none)
              (or merlin-buffer-flags 'none)
              (or merlin-buffer-extensions 'none))
            messages)
      (push (if dot_merlins
                (concat "Loaded .merlin files: " (mapconcat 'identity dot_merlins ", "))
              "No .merlin loaded")
            messages))
    (message "%s" (mapconcat 'identity messages "\n"))))

(defun merlin-customize ()
  "Open the customize buffer for the group merlin."
  (interactive)
  (customize-group 'merlin))

(defun merlin-version ()
  "Print the version of the ocamlmerlin binary."
  (interactive)
  (with-demoted-errors "Error invoking merlin: %S"
    (message "%s" (merlin--call-merlin "-version"))))

(defun merlin--configuration ()
  (when (or merlin-configuration-function merlin-grouping-function)
    (with-demoted-errors
      "merlin-command: invalid configuration (%S)"
      (funcall (or merlin-configuration-function merlin-grouping-function)))))

(defun merlin-command ()
  "Return or update path of ocamlmerlin binary selected by configuration"
  (when (or (not merlin-buffer-configuration)
            (merlin-lookup 'do-not-cache-config merlin-buffer-configuration))
    (setq merlin-buffer-configuration (merlin--configuration)))

  (let ((command (merlin-lookup 'command merlin-buffer-configuration)))
    (unless command
      (setq
       command
       (cond
        ((functionp merlin-command) (funcall merlin-command))
        ((stringp merlin-command) merlin-command)
        ((equal merlin-command 'opam)
         (with-temp-buffer
           (if (eq (call-process-shell-command
                    "opam var bin" nil (current-buffer) nil) 0)
               (let ((bin-path
                      (replace-regexp-in-string "\n$" "" (buffer-string))))
                 ;; the opam bin dir needs to be on the path, so if merlin
                 ;; calls out to sub binaries (e.g. ocamlmerlin-reason), the
                 ;; correct version is used rather than the version that
                 ;; happens to be on the path

                 ;; this was originally done via `opam exec' but that does not
                 ;; work for opam 1, and added a performance hit
                 (setq merlin-opam-bin-path (list (concat "PATH=" bin-path)))
                 (concat bin-path "/ocamlmerlin"))

             ;; best effort if opam is not available, lookup for the binary in
             ;; the existing env
             (progn
               (message "merlin-command: opam var failed (%S)"
                        (buffer-string))
               "ocamlmerlin"))))))

      ;; cache command in merlin-buffer configuration to avoid having to shell
      ;; out to `opam` each time.
      (push (cons 'command command) merlin-buffer-configuration)
      (when merlin-opam-bin-path
        (push (cons 'env merlin-opam-bin-path) merlin-buffer-configuration)))

    command))

;;;;;;;;;;;;;;;;
;; MODE SETUP ;;
;;;;;;;;;;;;;;;;

(defvar merlin-mode-map
  (let ((merlin-map (make-sparse-keymap))
        (merlin-menu-map (make-sparse-keymap))
        (merlin-show-type-map (make-sparse-keymap)))
    (define-key merlin-map (kbd "C-c C-x") #'merlin-error-next)
    (define-key merlin-map (kbd "C-c C-l") #'merlin-locate)
    (define-key merlin-map (kbd "C-c &"  ) #'merlin-pop-stack)
    (define-key merlin-map (kbd "C-c C-r") #'merlin-error-check)
    (define-key merlin-map (kbd "C-c C-t") #'merlin-type-enclosing)
    (define-key merlin-map (kbd "C-c C-d") #'merlin-destruct)
    (define-key merlin-map (kbd "C-c C-n") #'merlin-phrase-next)
    (define-key merlin-map (kbd "C-c C-p") #'merlin-phrase-prev)
    (define-key merlin-menu-map [customize]
      '("Customize merlin-mode" . merlin-customize))
    (define-key merlin-menu-map [separator]
      '("-"))
    (define-key merlin-show-type-map [point]
      '(menu-item "around the cursor" merlin-type-enclosing
                  :help "Show the type of the smallest subexpression near cursor"))
    (define-key merlin-show-type-map [exp]
      '(menu-item "of an expression" merlin-type-expr
                  :help "Input an expression and show its type"))
    (define-key merlin-menu-map [showtype]
      (cons "Show type..." merlin-show-type-map))
    (define-key merlin-menu-map [use]
      '(menu-item "Select packages" merlin-use
                  :help "Load findlib packages."))
    (define-key merlin-menu-map [error]
      '(menu-item "Check for errors" merlin-error-check
                  :help "Check current buffer for any error."))
    (define-key merlin-menu-map [dot-merlin]
      '(menu-item "Check configuration" merlin-configuration-check
                  :help "Display status of '.merlin'."))
    (define-key merlin-menu-map [setflags]
      '(menu-item "Set compiler flags" merlin-flags
                  :help "Pass specific compiler flags for current buffer."))
    (define-key merlin-menu-map [extensions]
      '(menu-item "Syntax extensions" merlin-extensions
                  :help "Enable support for some dialects of OCaml."))
    (define-key merlin-menu-map [restartmerlin]
      '(menu-item "Shutdown merlin server" merlin-stop-server
                  :help "Stop merlin server."))
    (define-key merlin-menu-map [versionmerlin]
      '(menu-item "Version" merlin-version
                  :help "Print version of the merlin binary."))
    (define-key merlin-map [menu-bar merlin] (cons "Merlin" merlin-menu-map))
    merlin-map
    ))

(defun merlin-can-handle-buffer ()
  "Simple sanity check (used to avoid running merlin on, e.g., completion buffer)."
  (cond ((equal (buffer-name) merlin-type-buffer-name) nil)
        ((buffer-file-name (buffer-base-buffer)) t)))

(defun merlin-lighter ()
  "Return the lighter for merlin which indicates the status of merlin process."
  (let (messages
        (num-errors (length merlin-erroneous-buffer)))
    (when merlin-report-errors-in-lighter
      (cond ((not merlin--project-cache) nil)
            ((cdr-safe merlin--project-cache)
             (push "check config!" messages))
            ((not (car-safe merlin--project-cache))
             (push "no .merlin" messages))))
    (unless (zerop num-errors)
      (push (format "%d error%s" num-errors (if (> num-errors 1) "s" ""))
            messages))
    (when (and merlin-show-instance-in-lighter
               (merlin-lookup 'name merlin-buffer-configuration))
      (push (merlin-lookup 'name merlin-buffer-configuration)
            messages))
    (if messages
        (concat " Merlin (" (mapconcat 'identity messages ",") ")")
      " Merlin")))

;;; DEPRECATED FUNCTIONS

(define-obsolete-function-alias 'merlin-project-check 'merlin-configuration-check "v3.0.0")

(define-obsolete-function-alias 'merlin--copy-enclosing 'merlin-copy-enclosing "v3.0.0")
(define-obsolete-function-alias 'merlin--destruct-enclosing 'merlin-destruct-enclosing "v3.0.0")

(define-obsolete-function-alias 'merlin-restart-process 'merlin-stop-server "v3.0.0")

;;;###autoload
(define-minor-mode merlin-mode
  "Minor mode for interacting with a merlin process.
Runs a merlin process in the background and perform queries on it.

Short cuts:
\\{merlin-mode-map}"
  nil
  :lighter (:eval (merlin-lighter))
  :keymap merlin-mode-map
  (if merlin-mode
      ;; When enabling merlin
      (progn
        (when (derived-mode-p 'tuareg-mode 'caml-mode 'reason-mode)
	  (setq merlin-guessed-favorite-caml-mode major-mode))
        (if (merlin-can-handle-buffer)
            (progn
              (let ((configuration (merlin--configuration)))
                (when configuration (setq merlin-buffer-configuration configuration)))
              (add-to-list 'after-change-functions 'merlin--on-edit)
              (add-hook 'xref-backend-functions #'merlin-xref-backend nil t)
              ;; TODO: Sanity check for selected merlin version
              (unless merlin--idle-timer
                (setq merlin--idle-timer
                      (run-with-idle-timer 0.5 t 'merlin-show-error-on-current-line))))
          (merlin-mode -1)))
    ;; When disabling merlin
    (progn
      (when merlin-highlight-overlay
        (delete-overlay merlin-highlight-overlay))
      (remove-overlays nil nil 'merlin-kind 'highlight)
      (remove-overlays nil nil 'merlin-kind 'error)
      (remove-hook 'xref-backend-functions #'merlin-xref-backend t))))

(provide 'merlin)

;; Load these after (provide 'merlin) because they (require 'merlin)
(require 'merlin-cap)
(require 'merlin-xref)

;;; merlin.el ends here
