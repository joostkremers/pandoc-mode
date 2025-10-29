;;; pandoc-mode.el --- Minor mode for interacting with Pandoc  -*- lexical-binding: t -*-

;; Copyright (c) 2009-2025 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 31 Oct 2009
;; Version: 2.34
;; Keywords: text, pandoc
;; URL: http://joostkremers.github.io/pandoc-mode/

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Pandoc-mode is a minor mode for interacting with Pandoc, a 'universal
;; document converter': <http://johnmacfarlane.net/pandoc/>.
;;
;; See the pandoc-mode manual for usage and installation instructions.

;;; Code:

(require 'easymenu)
(require 'transient)
(require 'pandoc-mode-utils)
(require 'cl-lib)
(require 'thingatpt)

(declare-function ebib "ext:ebib.el" (&optional file key))

(defvar-local pandoc--@-counter 0 "Counter for (@)-lists.")

(defvar pandoc--window-config nil
  "Stores the window configuration before calling pandoc--select-@.")

(defvar pandoc--pre-select-buffer nil
  "Buffer from which pandoc--@-select is called.")

(defvar pandoc--@-buffer nil
  "Buffer for selecting an (@)-element.")

(defvar pandoc--@-overlay nil
  "Overlay for pandoc--@-buffer.")

(defun pandoc--@-counter-inc ()
  "Increment pandoc--@-counter and return the new value."
  (when (= pandoc--@-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(@\\([0-9]+?\\))" (point-max) t)
        (let ((label (string-to-number (match-string 1))))
          (when (> label pandoc--@-counter)
            (setq pandoc--@-counter label))))))
  (setq pandoc--@-counter (1+ pandoc--@-counter)))

(defvar pandoc-@-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'pandoc-quit-@-select)
    (define-key map "j" #'pandoc-next-@)
    (define-key map "n" #'pandoc-next-@)
    (define-key map [down] #'pandoc-next-@)
    (define-key map "k" #'pandoc-prev-@)
    (define-key map "p" #'pandoc-prev-@)
    (define-key map [up] #'pandoc-prev-@)
    (define-key map [return] #'pandoc-select-current-@)
    (define-key map [home] #'pandoc-goto-first-@)
    (define-key map [prior] #'pandoc-goto-first-@)
    (define-key map [end] #'pandoc-goto-last-@)
    (define-key map [next] #'pandoc-goto-first-@)
    map)
  "Keymap for pandoc-@-mode.")

(defun pandoc-quit-@-select ()
  "Leave pandoc--@-select-buffer without selecting an (@)-label."
  (interactive)
  (remove-overlays)
  (set-window-configuration pandoc--window-config)
  (switch-to-buffer pandoc--pre-select-buffer))

(defun pandoc-next-@ ()
  "Highlight next (@)-definition."
  (interactive)
  (if (= (count-lines (point) (point-max)) 2)
      (beep)
    (forward-line 2)
    (move-overlay pandoc--@-overlay (point) (line-end-position))))

(defun pandoc-prev-@ ()
  "Highlight previous (@)-definition."
  (interactive)
  (if (= (point) (point-min))
      (beep)
    (forward-line -2)
    (move-overlay pandoc--@-overlay (point) (line-end-position))))

(defun pandoc-goto-first-@ ()
  "Highlight the first (@)-definition."
  (interactive)
  (goto-char (point-min))
  (move-overlay pandoc--@-overlay (point) (line-end-position)))

(defun pandoc-goto-last-@ ()
  "Highlight the last (@)-definition."
  (interactive)
  (goto-char (point-max))
  (forward-line -2)
  (move-overlay pandoc--@-overlay (point) (line-end-position)))

(defun pandoc-select-current-@ ()
  "Leave pandoc--@-select-buffer and insert selected (@)-label at point."
  (interactive)
  (looking-at " \\((@.*?)\\)")
  (let ((label (match-string 1)))
    (remove-overlays)
    (set-window-configuration pandoc--window-config)
    (switch-to-buffer pandoc--pre-select-buffer)
    (insert label)))

(define-derived-mode pandoc-@-mode
  fundamental-mode "Pandoc-select"
  "Major mode for the Pandoc-select buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defvar pandoc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c/" #'pandoc-main-transient)
    map)
  "Keymap for pandoc-mode.")

;;;###autoload
(define-minor-mode pandoc-mode
  "Minor mode for interacting with Pandoc."
  :init-value nil :lighter (:eval (concat " Pandoc/" (pandoc--get 'writer))) :global nil
  (cond
   (pandoc-mode    ; pandoc-mode is turned on
    (setq pandoc--local-settings (copy-tree pandoc--options))
    (pandoc--set 'reader (cdr (assq major-mode pandoc-major-modes)))
    (setq pandoc--settings-modified-flag nil)
    ;; Make sure the output buffer exists.
    (get-buffer-create pandoc--output-buffer-name)
    (pandoc-faces-load))
   ((not pandoc-mode)    ; pandoc-mode is turned off
    (setq pandoc--local-settings nil
          pandoc--settings-modified-flag nil)
    (pandoc-faces-unload))))

;;;###autoload
(defun conditionally-turn-on-pandoc ()
  "Turn on pandoc-mode if a Pandoc defaults file exists.
This is for use in major mode hooks."
  (when (and (buffer-file-name)
             (file-exists-p (pandoc--create-defaults-filename 'local (buffer-file-name) "default")))
    (pandoc-mode 1)))

(defun pandoc-toggle-extension (extension rw)
  "Toggle the value of EXTENSION.
RW is either `reader' or `writer', indicating whether the extension
should be toggled for the input or the output format."
  (pandoc--set-extension
   extension
   rw
   (cond
    ((memq (pandoc--get-extension extension rw) '(?+ ?-)) ; If the value is set explicitly,
     nil)               ; we can simply return it to the default.
    ((if (eq (pandoc--extension-in-format extension (car (split-string (pandoc--get rw) "[-+]")))
             :enabled)) ; If the extension is enabled in the current format,
     ?-)                ; we explicitly unset it.
    (t ?+)))) ; Otherwise we explicitly set it.

(defun pandoc--create-defaults-filename (type filename output-format)
  "Create a defaults filename.
TYPE is the type of defaults file, either `local' or `project'.
FILENAME is name of the file for which the defaults file is to be
created, OUTPUT-FORMAT the output format of the defaults file,
which is recorded in its name.  The return value is an absolute
filename."
  (setq filename (expand-file-name filename))
  (cond
   ((eq type 'local)
    (concat (file-name-directory filename) "." (file-name-nondirectory filename) "." output-format ".pandoc"))
   ((eq type 'project)
    (concat (file-name-directory filename) "Project." output-format ".pandoc"))))

(defun pandoc--create-global-defaults-filename (format)
  "Create a global defaults filename.
FORMAT is the output format to use."
  (concat (file-name-as-directory pandoc-data-dir) format ".pandoc"))

(defun pandoc--compose-output-file-name (&optional pdf input-file)
  "Create an output file name for the current buffer based on INPUT-FILE.
If INPUT-FILE is non-nil, use the file the current buffer is
visiting.  If the current buffer's output file is set to t, or if
the target format is odt, epub or docx, create an output file
name based on INPUT-FILE.  If an output directory is set, use
this directory, otherwise use the directory of INPUT-FILE (which
should be a fully qualified file path).

If an explicit output file is set, use that file, combined with
the output directory, if given.  If an output file name is set
but no output directory, use the directory of INPUT-FILE.

If PDF is non-nil, use `pdf' as the extension.

If the current buffer's settings do not specify an output
file (i.e., if the output file is set to nil), return nil."
  (or input-file
      (setq input-file (expand-file-name (buffer-file-name))))
  (cond
   ((or (eq (pandoc--get 'output) t) ; If the user set the output file to T
        (and (null (pandoc--get 'output)) ; or if the user set no output file but either
             (or pdf                      ; (i) we're converting to pdf, or
                 (member (pandoc--get 'writer) ; (ii) the output format is one of these:
                         '("odt" "epub" "docx" "pptx")))))
    (format "%s/%s%s"                   ; we create an output file name.
            (expand-file-name (or (pandoc--get 'output-dir)
                                  (file-name-directory input-file)))
            (file-name-sans-extension (file-name-nondirectory input-file))
            (if pdf
                ".pdf"
              (cadr (assoc (pandoc--get 'writer) pandoc-output-format-extensions)))))
   ((stringp (pandoc--get 'output))     ; If the user set an output file,
    (format "%s/%s"               ; we combine it with the output directory
            (expand-file-name (or (pandoc--get 'output-dir)
                                  (file-name-directory input-file)))
            (if pdf                 ; and check if we're converting to pdf.
                (concat (file-name-sans-extension (pandoc--get 'output)) ".pdf")
              (pandoc--get 'output))))
   (t nil)))

(defun pandoc--format-all-options (output-file &optional pdf)
  "Create a list of strings with pandoc options for the current buffer.
OUTPUT-FILE the name of the output file.  If PDF is non-nil, an
output file is always set, which gets the suffix `.pdf'.  If the
output format is \"odt\", \"epub\" or \"docx\" but no output file
is specified, one will be created."
  (let ((read (format "--read=%s%s" (pandoc--get 'reader) (pandoc--format-extensions (pandoc--get 'read-extensions))))
        (write (if pdf
                   (if (member (pandoc--get 'writer) pandoc--pdf-able-formats)
                       (format "--write=%s" (pandoc--get 'writer))
                     "--write=latex")
                 (format "--write=%s%s" (pandoc--get 'writer) (pandoc--format-extensions (pandoc--get 'write-extensions)))))
        (output (when output-file (format "--output=%s" output-file)))
        ;; Filters are handled separately, because they sometimes need to be
        ;; passed to `pandoc' before other options.
        (filters (pandoc--format-list-options 'filter (pandoc--get 'filter)))
        (lua-filters (pandoc--format-list-options 'lua-filter (pandoc--get 'lua-filter)))
        (list-options (mapcar (lambda (option)
                                (pandoc--format-list-options option (pandoc--get option)))
                              (remove 'lua-filter
                                      (remove 'filter pandoc--list-options))))
        (alist-options (mapcar (lambda (option)
                                 (pandoc--format-alist-options option (pandoc--get option)))
                               pandoc--alist-options))
        (cli-options (pandoc--format-cli-options)))
    ;; Note: list-options and alist-options are both lists of lists, so we need to flatten them first.
    (delq nil (append (list read write output) filters lua-filters cli-options (apply #'append list-options) (apply #'append alist-options)))))

(defun pandoc--format-extensions (extensions)
  "Create a string of extensions to be added to the Pandoc command line.
EXTENSIONS is an alist of (<extension> . <value>) pairs."
  (mapconcat (lambda (elt)
               (if (cdr elt)
                   (format "%s%s" (cdr elt) (car elt))
                 ""))
             extensions
             ""))

(defun pandoc--format-list-options (option values)
  "Create a list of cli options for OPTION from the values in VALUES."
  (mapcar (lambda (value)
            (format "--%s=%s" option (if (eq (get option 'pandoc-list-type) 'file)
                                         (pandoc--expand-absolute-path value)
                                       value)))
          values))

(defun pandoc--format-alist-options (option alist)
  "Create a list of cli options for OPTION from the key-value pairs in ALIST."
  (mapcar (lambda (kv)
            (let ((key (car kv))
                  (value (cdr kv)))
              ;; if key or value contains a colon, we use the short form
              ;; of the option, because it uses = to separate the two.
              (if (or (string-match-p ":" key)
                      (string-match-p ":" value))
                  ;; the only two alist options are `variable' and
                  ;; `metadata', whose short forms are `V' and `M',
                  ;; respectively, so we can just capitalise their first
                  ;; letters.
                  (format "-%c %s%s" (upcase (aref (symbol-name option) 0))
                          key
                          (if (eq value t)
                              ""
                            (format "=%s" value)))
                (format "--%s=%s%s" option key
                        (if (eq value t)
                            ""
                          (format ":%s" value))))))
          alist))

(defun pandoc--format-cli-options ()
  "Create a list of options in `pandoc--cli-options'."
  (mapcar (lambda (option)
            (let ((value (pandoc--get option)))
              (when (and value
                         (memq option pandoc--filepath-options))
                (setq value (pandoc--expand-absolute-path value)))
              (cond
               ((eq value t)
                (format "--%s" option))
               ((or (numberp value)
                    (stringp value))
                (format "--%s=%s" option value))
               (t nil))))
          pandoc--cli-options))

(defun pandoc-process-directives (output-format)
  "Processes pandoc-mode @@-directives in the current buffer.
OUTPUT-FORMAT is passed unchanged to the functions associated
with the @@-directives."
  (interactive (list (pandoc--get 'writer)))
  (mapc #'funcall pandoc-directives-hook)
  (let ((case-fold-search nil)
        (directives-regexp (concat "\\([\\]?\\)@@" (regexp-opt (mapcar #'car pandoc-directives) t))))
    (goto-char (point-min))
    (while (re-search-forward directives-regexp nil t)
      (if (string= (match-string 1) "\\")
          (delete-region (match-beginning 1) (match-end 1))
        (let ((@@-beg (match-beginning 0))
              (@@-end (match-end 0))
              (directive-fn (cdr (assoc (match-string 2) pandoc-directives))))
          (cond
           ((eq (char-after) ?{) ; if there is an argument.
            ;; note: point is on the left brace, and scan-lists
            ;; returns the position *after* the right brace. we need
            ;; to adjust both values to get the actual argument.
            (let* ((arg-beg (1+ (point)))
                   (arg-end (1- (scan-lists (point) 1 0)))
                   (text (buffer-substring-no-properties arg-beg arg-end)))
              (goto-char @@-beg)
              (delete-region @@-beg (1+ arg-end))
              (insert (funcall directive-fn output-format text)))
            (goto-char @@-beg))
           ;; check if the next character is not a letter or number.
           ;; if it is, we're actually on a different directive.
           ((looking-at "[a-zA-Z0-9]") t)
           ;; otherwise there is no argument.
           (t (goto-char @@-beg)
              (delete-region @@-beg @@-end) ; else there is no argument
              (insert (funcall directive-fn output-format))
              (goto-char @@-beg))))))))

(defun pandoc--process-lisp-directive (_ lisp)
  "Process @@lisp directives.
The first argument is the output argument and is ignored.  LISP
is the argument of the @@lisp directive."
  (format "%s" (eval (car (read-from-string lisp)))))

(defun pandoc--process-include-directive (_ include-file)
  "Process @@include directives.
The first argument is the output argument and is ignored.
INCLUDE-FILE is the argument of the @@include directive."
  (with-temp-buffer
    (insert-file-contents include-file)
    (buffer-string)))

;; `pandoc-call-external' sets up a process sentinel that needs to refer to
;; `pandoc-binary' to provide an informative message. We want to allow a
;; buffer-local value of `pandoc-binary', but the process sentinel doesn't
;; have the necessary context. With `lexical-binding' set to t, we could
;; make the sentinel a closure, but this only works for Emacs >= 24.1. An
;; alternative way is to use a global variable, which, however, means that
;; we can only have one pandoc subprocess at a time. Hopefully that won't
;; be a problem.

(defvar pandoc--local-binary "pandoc"
  "Temporary store for the buffer-local value of `pandoc-binary'.")

(defun pandoc--call-external (output-format &optional pdf region)
  "Call pandoc on the current buffer.
This function creates a temporary buffer and sets up the required
local options.  The contents of the current buffer is copied into
the temporary buffer, the @@-directives are processed, after
which pandoc is called.

OUTPUT-FORMAT is the format to use.  If t, the current buffer's
output format is used.  If PDF is non-nil, a pdf file is created.
REGION is a cons cell specifying the beginning and end of the
region to be sent to pandoc.

If the current buffer's \"master file\" option is set, that file
is processed instead.  The output format is taken from the current
buffer, however, unless one is provided specifically.  REGION is
also ignored in this case."
  (let* ((orig-buffer (current-buffer))
         (buffer (if (pandoc--get 'master-file)
                     (find-file-noselect (pandoc--get 'master-file))
                   (current-buffer)))
         (input-file (buffer-file-name buffer))
         output-file
         (display-name (buffer-name)))

    ;; If the buffer is visiting a file, we want to display the file name in
    ;; messages. If the buffer is not visiting a file, we create a file name in
    ;; case we need one, but we display the buffer name in messages.  Then
    ;; create the output file name.
    (if input-file
        (setq display-name (file-name-nondirectory input-file))
      (setq input-file (expand-file-name (concat "./" (pandoc--create-file-name-from-buffer (buffer-name))))))

    ;; If there's a master file, ignore the region.
    (if (pandoc--get 'master-file)
        (setq region nil))

    ;; Keep track of the buffer-local value of `pandoc-binary', if there is one.
    (setq pandoc--local-binary (buffer-local-value 'pandoc-binary buffer))

    ;; We use a temp buffer, so we can process @@-directives without having to
    ;; undo them and set the options independently of the original buffer.
    (with-temp-buffer
      (cond
       ;; If an output format was provided, try and load a defaults file for it.
       ((stringp output-format)
        (unless (and input-file
                     (pandoc--load-settings-for-file (expand-file-name input-file) output-format t))
          ;; If no defaults file was found, unset all options except input and output format.
          (setq pandoc--local-settings (copy-tree pandoc--options))
          (pandoc--set 'writer output-format)
          (pandoc--set 'reader (pandoc--get 'reader buffer))))

       ;; If no output format was provided, we use BUFFER's options, except the
       ;; output format, which we take from ORIG-BUFFER.  We also set the local
       ;; variable `output-format' to this format, so that the value of
       ;; `pandoc--latest-run' is set correctly beloww.
       ((eq output-format t)
        (setq pandoc--local-settings (buffer-local-value 'pandoc--local-settings buffer))
        (pandoc--set 'writer (setq output-format (pandoc--get 'writer orig-buffer)))))

      ;; Set the name of the output file.
      (setq output-file (pandoc--compose-output-file-name pdf input-file))

      ;; Copy any local `pandoc/' variables from `orig-buffer' or
      ;; `buffer' (the values in `orig-buffer' take precedence):
      (dolist (option (pandoc--get-file-local-options (list orig-buffer buffer)))
        (set (make-local-variable (car option)) (cdr option)))
      (let ((option-list (pandoc--format-all-options output-file pdf)))
        (insert-buffer-substring-no-properties buffer (car region) (cdr region))
        (insert "\n") ; Insert a new line. If Pandoc does not encounter a newline on a single line, it will hang forever.
        (message "Running %s on %s" (file-name-nondirectory pandoc--local-binary) display-name)
        (pandoc-process-directives (pandoc--get 'writer))
        (with-current-buffer (get-buffer-create pandoc--output-buffer-name)
          (erase-buffer))
        (pandoc--log 'log "%s\n%s" (make-string 50 ?=) (current-time-string))
        (pandoc--log 'log "Calling %s with:\n\n%s %s" (file-name-nondirectory pandoc--local-binary) pandoc--local-binary (mapconcat #'identity option-list " "))

        (let ((coding-system-for-read 'utf-8)
              (coding-system-for-write 'utf-8)
              (log-success (lambda (file binary)
                             (pandoc--log 'message "%s: %s finished successfully"
                                          (file-name-nondirectory file)
                                          (file-name-nondirectory binary))
                             (with-current-buffer orig-buffer
                               (setq pandoc--latest-run (cons output-format output-file)))))
              (log-failure (lambda (file binary)
                             (pandoc--log 'message "%s: Error in %s process"
                                          (file-name-nondirectory file)
                                          (file-name-nondirectory binary))
                             (with-current-buffer orig-buffer
                               (setq pandoc--latest-run 'error)))))

          (cond
           (pandoc-use-async
            (let* ((process-connection-type pandoc-process-connection-type)
                   (process (apply #'start-process "pandoc-process" (get-buffer-create pandoc--output-buffer-name) pandoc--local-binary option-list)))
              (set-process-sentinel process (lambda (_ e)
                                              (cond
                                               ((string-equal e "finished\n")
                                                (funcall log-success display-name pandoc--local-binary)
                                                (run-hooks 'pandoc-async-success-hook))
                                               (t (funcall log-failure display-name pandoc--local-binary)
                                                  (display-buffer pandoc--output-buffer-name)))))
              (process-send-region process (point-min) (point-max))
              (process-send-eof process)))
           ((not pandoc-use-async)
            (if (= 0 (apply #'call-process-region (point-min) (point-max) pandoc--local-binary nil (get-buffer-create pandoc--output-buffer-name) t option-list))
                (funcall log-success display-name pandoc--local-binary)
              (funcall log-failure display-name pandoc--local-binary)
              (display-buffer pandoc--output-buffer-name)))))))))

(defun pandoc-run-pandoc (&optional prefix)
  "Run pandoc on the current document.
If called with a PREFIX argument, the user is asked for an output
format.  Otherwise, the output format currently set in the buffer
is used.

If the region is active, pandoc is run on the region instead of
the buffer."
  (interactive "P")
  (pandoc--call-external (if prefix
                             (completing-read "Output format to use: "
                                              (pandoc--extract-formats 'output)
                                              nil t)
                           t)
                         nil
                         (if (use-region-p)
                             (cons (region-beginning) (region-end)))))

(defvar-local pandoc--output-format-for-pdf nil
  "Output format used to for pdf conversion.
Set the first time the user converts to pdf.  Unset when the
user changes output format.")

(defun pandoc-convert-to-pdf (&optional prefix)
  "Convert the current document to pdf.
If the output format of the current buffer can be used to create
a pdf (latex, context, or html5), the buffer's options are used.
If not, the user is asked to supply a format.  If a defaults file
for the user-supplied format exists, the settings from this file
are used for conversion.  If no such defaults file exists, only
the input and output format are set, all other options are unset.
This user-supplied output format is persistent: the next pdf
conversion uses the same format.

If called with a PREFIX argument \\[universal-argument], always ask the user for a
pdf-able format.

Note that if the user changes the output format for the buffer,
the format for pdf conversion is unset.

If the region is active, pandoc is run on the region instead of
the buffer (except when a master file is set, in which case
pandoc is always run on the master file)."
  ;; TODO When the region is active, it might be nice to run pandoc on the
  ;; region but use the master file's settings.
  (interactive "P")
  (let ((ask (and (listp prefix) (eq (car prefix) 4))))
    (cond
     ((and (not ask)
           (member (pandoc--get 'writer) pandoc--pdf-able-formats))
      (setq pandoc--output-format-for-pdf t)) ; Use buffer's output format and settings.
     ((or ask
          (not pandoc--output-format-for-pdf))
      (setq pandoc--output-format-for-pdf (completing-read "Specify output format for pdf creation: " pandoc--pdf-able-formats nil t nil nil (car pandoc--pdf-able-formats))))))
  (pandoc--call-external pandoc--output-format-for-pdf t (when (use-region-p) (cons (region-beginning) (region-end)))))

(defun pandoc-set-default-format ()
  "Set the current output format as default.
This is done by creating a symbolic link to the relevant defaults
files.  (Therefore, this function is not available on Windows.)"
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "This option is not available on MS Windows")
    (let ((current-defaults-file
           (file-name-nondirectory (pandoc--create-defaults-filename 'local (buffer-file-name)
                                                                     (pandoc--get 'writer))))
          (current-project-file
           (file-name-nondirectory (pandoc--create-defaults-filename 'project (buffer-file-name)
                                                                     (pandoc--get 'writer)))))
      (when (not (file-exists-p current-defaults-file))
        (pandoc--save-settings 'local (pandoc--get 'writer)))
      (make-symbolic-link current-defaults-file
                          (pandoc--create-defaults-filename 'local (buffer-file-name) "default") t)
      (when (file-exists-p current-project-file)
        (make-symbolic-link current-project-file
                            (pandoc--create-defaults-filename 'project (buffer-file-name) "default") t))
      (message "`%s' set as default output format." (pandoc--get 'writer)))))

(defun pandoc-save-settings ()
  "Save the settings of the current buffer.
This function just calls pandoc--save-settings with the
appropriate output format."
  (interactive)
  (pandoc--save-settings 'local (pandoc--get 'writer)))

(defun pandoc-save-project-settings ()
  "Save the current settings as a project file."
  (interactive)
  (pandoc--save-settings 'project (pandoc--get 'writer)))

(defun pandoc-save-global-settings ()
  "Save the current settings to a global settings file."
  (interactive)
  (unless (file-directory-p pandoc-data-dir)
    (make-directory pandoc-data-dir))
  (pandoc--save-settings 'global (pandoc--get 'writer)))

(defun pandoc--save-settings (type format &optional no-confirm)
  "Save the settings of the current buffer.
TYPE must be a quoted symbol and specifies the type of settings
file.  It can be `local', `project', or `global'.  FORMAT is the
output format for which the settings are to be saved.  If
NO-CONFIRM is non-nil, any existing settings file is overwritten
without asking."
  (let* ((filename (buffer-file-name))
         (settings pandoc--local-settings)
         (defaults-file (if (eq type 'global)
                            (pandoc--create-global-defaults-filename format)
                          (pandoc--create-defaults-filename type filename format))))
    (if (and (not no-confirm)
             (file-exists-p defaults-file)
             (not (y-or-n-p (format "%s defaults file `%s' already exists.  Overwrite? "
                                    (capitalize (symbol-name type))
                                    (file-name-nondirectory defaults-file)))))
        (message "%s defaults file not written." (capitalize (symbol-name type)))
      (with-temp-buffer
        (let ((print-length nil)
              (print-level nil)
              (print-circle nil))
          (insert ";; -*- mode: lisp-data -*-\n\n"
                  (format ";; pandoc-mode %s settings file%s\n"
                          type
                          (if (eq type 'local)
                              (concat " for " (file-name-nondirectory filename))
                            ""))
                  (format ";; saved on %s\n\n" (format-time-string "%Y.%m.%d %H:%M")))
          (pp settings (current-buffer)))
        (let ((make-backup-files nil))
          (write-region (point-min) (point-max) defaults-file))
        (message "%s settings file written to `%s'." (capitalize (symbol-name type)) (file-name-nondirectory defaults-file)))
      (setq pandoc--settings-modified-flag nil))))

(defun pandoc-revert-settings ()
  "Revert settings for the current buffer.
The settings file is reread from disk, so that any changes made
to the settings that have not been saved are reverted."
  (interactive)
  (let ((format (pandoc--get 'writer)))
    (setq pandoc--local-settings (copy-tree pandoc--options))
    (pandoc--load-settings-profile format 'no-confirm)))

(defun pandoc-load-default-settings ()
  "Load the default settings of the file in the current buffer.
This function is for use in `pandoc-mode-hook'."
  (pandoc--load-settings-profile "default"))

(defun pandoc--load-settings-profile (format &optional no-confirm)
  "Load the options for FORMAT from the corresponding settings file.
If NO-CONFIRM is t, no confirmation is asked if the current
settings have not been saved."
  (pandoc--load-settings-for-file (when (buffer-file-name)
                                    (expand-file-name (buffer-file-name)))
                                  format
                                  no-confirm))

(defun pandoc--load-settings-for-file (file format &optional no-confirm)
  "Load the settings file of FILE for FORMAT.
Search for a local, a project and a global settings file, in that
order, and load the first one that exists and is readable.

If NO-CONFIRM is t, no confirmation is asked if the current
settings have not been saved.  FILE must be an absolute path
name.  If FILE is nil, a global settings file is read, if any.
The settings are stored in the current buffer's
`pandoc--local-settings'.  Return nil if no settings or project
file is found for FILE, otherwise non-nil."
  (when (and (not no-confirm)
             pandoc--settings-modified-flag
             (y-or-n-p (format "Current settings for format \"%s\" modified.  Save first? " (pandoc--get 'writer))))
    (pandoc--save-settings 'local (pandoc--get 'writer) t))
  (let (settings)
    ;; first try to read local settings
    (when file
      (setq settings (cons 'local (pandoc--read-settings-from-file (pandoc--create-defaults-filename 'local file format)))))
    ;; if it fails, try project settings
    (when (and file (not (cdr settings)))
      (setq settings (cons 'project (pandoc--read-settings-from-file (pandoc--create-defaults-filename 'project file format)))))
    ;; if that fails too, or if there is no file, try reading global settings
    (unless (cdr settings)
      (setq settings (cons 'global (pandoc--read-settings-from-file (pandoc--create-global-defaults-filename format)))))
    ;; now set them
    (when (cdr settings)
      (setq pandoc--local-settings (cdr settings))
      (message "%s settings file loaded for format \"%s\"." (capitalize (symbol-name (car settings))) format))))

(defun pandoc--read-settings-from-file (file)
  "Read the settings in FILE and return them.
If FILE does not exist or cannot be read, return nil."
  (if (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (looking-at "#") ; We're probably dealing with an old settings file.
            (pandoc--read-old-settings-from-buffer)
          (let ((flist (when (search-forward "(" nil t)
                         (forward-char -1)
                         (read (current-buffer)))))
            (if (listp flist)
                flist))))))

(defun pandoc--read-old-settings-from-buffer ()
  "Read old-style settings from the current buffer.
`pandoc--settings-modified-flag' is set, so that the user will be
asked to save the settings on exit.  Return an alist with the
options and their values."
  (goto-char (point-min))
  (let (options)                        ; we collect the options in a list
    (while (re-search-forward "^\\([a-z-]*\\)::\\(.*?\\)$" nil t)
      (let ((option (intern (match-string 1)))
            (value (match-string 2)))
        ;; If the option is a variable or extension, we read its name and
        ;; value and add them to the alist as a dotted list.
        (push (if (memq option '(variable read-extensions write-extensions))
                  (progn
                    (string-match "^\\(.*?\\):\\(.*?\\)$" value)
                    (cons option (cons (match-string 1 value)
                                       (if (eq option 'variable)
                                           (match-string 2 value)
                                         (intern (match-string 2 value))))))
                (cons option (cond
                              ((string-match "^[0-9]$" value) (string-to-number value))
                              ((string= "t" value) t)
                              ((string= "nil" value) nil)
                              (t value))))
              options)))
    ;; `options' isn't in the proper format for pandoc--local-settings yet:
    ;; there may be multiple variables and extensions in it. Since we're in
    ;; a temp buffer, we can simply use pandoc--set to set all options and
    ;; then return the local value of `pandoc--local-settings'.
    (setq pandoc--local-settings (copy-tree pandoc--options))
    (mapc (lambda (option)
            (pandoc--set (car option) (cdr option)))
          options)
    pandoc--local-settings))

(defun pandoc-view-output (&optional arg)
  "Display the output file.
Try to display the output file generated in the most recent call
to Pandoc.  If no output file was produced or Pandoc has not been
called yet, try to open the output file as defined by the current
settings.  If the latest Pandoc run produced an error, display an
error message, unless ARG is non-nil, in which case try to open
the output file defined by the current settings.  If no output
file exists, display the *Pandoc output* buffer."
  (interactive "P")
  (when (and (eq pandoc--latest-run 'error) (not arg))
    (error "No output file created on most recent call to `pandoc'"))
  (let ((format (or (car pandoc--latest-run)
                    (pandoc--get 'writer)))
        (file (or (cdr pandoc--latest-run)
                  (pandoc--compose-output-file-name arg))))
    (if file
        (if (file-readable-p file)
            (let ((handler (if (cl-equalp (file-name-extension file) "pdf")
                               pandoc-pdf-viewer
                             (cadr (assoc-string format pandoc-viewers)))))
              (cond
               ((stringp handler)
                (start-process "pandoc-viewer" pandoc--viewer-buffer-name handler file))
               ((eq handler 'emacs)
                (let ((buffer (find-file-noselect file)))
                  (if buffer
                      (display-buffer buffer)
                    (error "Could not open %s" file))))
               ((functionp handler)
                (funcall handler file))
               (t (error "No viewer defined for output format `%s'" format))))
          (error "`%s' is not readable" file))
      (pandoc-view-output-buffer))))

(defun pandoc-view-output-buffer ()
  "Displays the *Pandoc output* buffer."
  (interactive)
  (display-buffer pandoc--output-buffer-name))

(defun pandoc-view-settings ()
  "Displays the settings file in a *Help* buffer."
  (interactive)
  ;; remove all options that do not have a value.
  (let* ((settings (pandoc--current-settings))
         (buffers (list (current-buffer)
                        (if (pandoc--get 'master-file)
                            (find-file-noselect (pandoc--get 'master-file)))))
         (file-locals (pandoc--get-file-local-options buffers)))
    (with-help-window " *Pandoc Help*"
      (let ((print-length nil)
            (print-level nil)
            (print-circle nil))
        (princ "Current settings:\n\n")
        (pp settings)
        (when file-locals
          (princ "\n\nFile-local settings:\n\n")
          (pp file-locals))))))

(defun pandoc-view-log ()
  "Display the log buffer in a temporary window."
  (interactive)
  (display-buffer (get-buffer-create pandoc--log-buffer-name)))

(defun pandoc-insert-@ ()
  "Insert a new labeled (@) list marker at point."
  (interactive)
  (let ((label (pandoc--@-counter-inc)))
    (insert (format "(@%s)" label))))

(defun pandoc--collect-@-definitions ()
  "Collect (@)-definitions and return them as a list."
  (save-excursion
    (goto-char (point-min))
    (let (definitions)
      (while (re-search-forward "^[[:space:]]*\\((@.*?).*\\)$" nil t)
        (push (match-string-no-properties 1) definitions))
      (nreverse definitions))))

(defun pandoc-select-@ ()
  "Show a list of (@)-definitions and allow the user to choose one."
  (interactive)
  (let ((definitions (pandoc--collect-@-definitions)))
    (setq pandoc--window-config (current-window-configuration))
    (setq pandoc--pre-select-buffer (current-buffer))
    (setq pandoc--@-buffer (get-buffer-create " *Pandoc select*"))
    (set-buffer pandoc--@-buffer)
    (pandoc-@-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (mapc (lambda (definition)
              (insert (concat " " definition "\n\n")))
            definitions)
      (goto-char (point-min))
      (setq pandoc--@-overlay (make-overlay (point-min) (line-end-position)))
      (overlay-put pandoc--@-overlay 'face 'highlight))
    (select-window (display-buffer pandoc--@-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to set specific options. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pandoc--read-completion-function (formats)
  "Create a completion function for `pandoc-set-read-from-category' for FORMATS.
Return a function that can be used in `completing-read' as the
COLLECTION function, using TABLE as the completion table."
  (lambda (str pred flag)
    (if (eq flag 'metadata)
        `(metadata (category . pandoc-format)
                   (annotation-function . ,(lambda (format)
                                             (propertize (concat (make-string (- 30 (length format)) ?\s)
                                                                 (nth 1 (assoc format formats)))
                                                         'face 'italic))))
      (complete-with-action flag formats str pred))))

(defun pandoc-set-read-from-category (category)
  "Set the input format.
CATEGORY is a string naming a category of formats as listed in
`pandoc--formats'.  Only formats from CATEGORY are offered as completion
candidates."
  (let* ((formats (drop 3 (assoc category pandoc--formats)))
         (format (completing-read "Input format: " (pandoc--read-completion-function formats))))
    (pandoc--set 'reader format)
    (message "Input format set to `%s'" format)))

(defun pandoc-set-read (format)
  "Set the input format to FORMAT."
  (interactive (list (completing-read "Set input format to: "
                                      (pandoc--extract-formats 'input)
                                      nil t)))
  (pandoc--set 'reader format)
  (message "Input format set to `%s'" format))

(defun pandoc-set-write (format)
  "Set the output format to FORMAT.
If a settings and/or project file exists for FORMAT, they are
loaded.  If none exists, all options are unset (except the input
format)."
  (interactive (list (completing-read "Set output format to: "
                                      (pandoc--extract-formats 'output)
                                      nil t)))
  (when (and pandoc--settings-modified-flag
             (y-or-n-p (format "Current settings for output format \"%s\" changed.  Save? " (pandoc--get 'writer))))
    (pandoc--save-settings 'local (pandoc--get 'writer) t))
  (unless (pandoc--load-settings-profile format t)
    (setq pandoc--local-settings (copy-tree pandoc--options))
    (pandoc--set 'writer format)
    (pandoc--set 'reader (cdr (assq major-mode pandoc-major-modes))))
  (setq pandoc--settings-modified-flag nil)
  (setq pandoc--output-format-for-pdf nil)
  (message "Output format set to `%s'" format))

(defun pandoc-set-output (prefix)
  "Set the output file.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]', the output file is unset.  If called
with any other prefix argument, the output file is created on the
basis of the input file and the output format."
  (interactive "P")
  (pandoc--set 'output
               (cond
                ((eq prefix '-) nil)
                ((null prefix) (file-name-nondirectory (read-file-name "Output file: ")))
                (t t))))

(defun pandoc-set-data-dir (prefix)
  "Set the option `Data Directory'.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]'), the data directory is set to NIL, which
means use $HOME/.pandoc."
  (interactive "P")
  (pandoc--set 'data-dir
               (if (eq prefix '-)
                   nil
                 (read-directory-name "Data directory: " nil nil t))))

(defun pandoc-set-defaults (prefix)
  "Set the defaults file.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]'), the defaults file is set to nil."
  (interactive "P")
  (pandoc--set 'defaults (cond
                          ((eq prefix '-) nil)
                          (t (pandoc--read-file-name "Defaults file: " default-directory (not prefix))))))

(defun pandoc-set-output-dir (prefix)
  "Set the option `Output Directory'.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]'), the output directory is set to NIL,
which means use the directory of the input file."
  (interactive "P")
  (pandoc--set 'output-dir
               (if (eq prefix '-)
                   nil
                 (read-directory-name "Output directory: " nil nil t))))

(defun pandoc-set-extract-media (prefix)
  "Set the option `Extract media'.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]'), no media files are extracted."
  (interactive "P")
  (pandoc--set 'extract-media
               (if (eq prefix '-)
                   nil
                 (read-directory-name "Extract media files to directory: " nil nil t))))

(defun pandoc-set-file-scope (prefix)
  "Set the option `File scope'.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]'), document scope is used."
  (interactive "P")
  (pandoc--set 'file-scope (if (eq prefix '-) nil t)))

(defun pandoc-set-sandbox (prefix)
  "Set the option `Sandbox'.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]'), no sandbox is used."
  (interactive "P")
  (pandoc--set 'sandbox (if (eq prefix '-) nil t)))

(defun pandoc-set-master-file (prefix)
  "Set the master file.
If called with the PREFIX argument `\\[universal-argument] -' (or
`\\[negative-argument]'), the master file is set to nil, which
means the current file is the master file."
  (interactive "P")
  (pandoc--set 'master-file (cond
                             ((eq prefix '-) nil)
                             (t (pandoc--read-file-name "Master file: " default-directory (not prefix))))))

(defun pandoc-set-this-file-as-master ()
  "Set the current file as master file.
This option creates a Project settings file in the current
directory to ensure that all files use the current file as master
file."
  (interactive)
  (pandoc--set 'master-file (buffer-file-name))
  (pandoc--save-settings 'project (pandoc--get 'writer)))

(defun pandoc-set-html-math-method (prefix method)
  "Set the method for rendering mathematics in HTML to METHOD.
This function is meant to be called from an interactive function to do
the actual work.  PREFIX is the raw prefix argument from the calling
function.  If PREFIX is non-nil, ask for a URL and add it to the
option's value.  METHOD is a string naming a math rendering method as
defined in `pandoc--html-math-methods'.  METHOD can also be nil, in
which case `html-math-method' is unset, or it can be t, in which case
the method is kept as is, but the user is asked to provide a URL."
  (let* ((method (if (eq method t)
                     (cdr (assq 'method (pandoc--get 'html-math-method)))
                   method))
         (url (if (and prefix
                       (cdr (assoc method pandoc--html-math-methods)))
                  (read-string "URL: "))))
    ;; This is a hack.  Normally, calling `pandoc--set' on a list option
    ;; would *add* the item to the list.  But we only want the list to have
    ;; at most these two items, so we clear the option first.
    (pandoc--set 'html-math-method nil)
    (if method (pandoc--set 'html-math-method `(method . ,method)))
    (if url (pandoc--set 'html-math-method `(url . ,url)))))

(defun pandoc-set-verbosity (prefix)
  "Set the option `verbosity'.
If PREFIX is \\[negative-argument], unset the option, otherwise ask the user.  Currently
allowed values are \"INFO\" and \"ERROR\"."
  (interactive "P")
  (pandoc--set 'verbosity
               (if (eq prefix '-)
                   nil
                 (completing-read "Verbosity: " '("INFO" "ERROR") nil t))))

(easy-menu-define pandoc-mode-menu pandoc-mode-map "Pandoc menu."
  `("Pandoc"
    ["Run Pandoc" pandoc-run-pandoc :active t]
    ["Create PDF" pandoc-convert-to-pdf :active t]
    ["View Output File" pandoc-view-output :active t]
    ["View Output Buffer" pandoc-view-output-buffer :active t]
    ["View Log Buffer" pandoc-view-log :active t]
    ("Example Lists"
     ["Insert New Example" pandoc-insert-@ :active t]
     ["Select And Insert Example Label" pandoc-select-@ :active t])
    ("Settings Files"
     ["Save File Settings" pandoc-save-settings :active t]
     ["Save Project File" pandoc-save-project-settings :active t]
     ["Save Global Settings File" pandoc-save-global-settings :active t]
     ["Revert Settings" pandoc-revert-settings :active t]
     ["Set As Default Format" pandoc-set-default-format :active (not (eq system-type 'windows-nt))])
    "--"
    ["View Current Settings" pandoc-view-settings :active t]

    ,(append (cons "Input Format"
                   (mapcar (lambda (option)
                             (vector (car option)
                                     `(pandoc--set 'reader ,(cdr option))
                                     :active t
                                     :style 'radio
                                     :selected `(string= (pandoc--get 'reader)
                                                         ,(cdr option))))
                           pandoc--input-formats-menu))
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc--get 'reader)))
                           (mapcar (lambda (ext)
                                     (vector (car ext)
                                             `(lambda () (interactive) (pandoc-toggle-extension ,(car ext) 'reader))
                                             :active t
                                             :style 'toggle
                                             :selected `(pandoc--extension-active-p ,(car ext) 'reader)))
                                   pandoc--extensions))))

    ,(append (cons "Output Format"
                   (mapcar (lambda (option)
                             (vector (cadr option)
                                     `(pandoc-set-write ,(car option))
                                     :active t
                                     :style 'radio
                                     :selected `(string= (pandoc--get 'writer)
                                                         ,(car option))))
                           (pandoc--extract-formats 'output)))
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc--get 'writer)))
                           (mapcar (lambda (ext)
                                     (vector (car ext)
                                             `(lambda () (interactive) (pandoc-toggle-extension ,(car ext) 'writer))
                                             :active t
                                             :style 'toggle
                                             :selected `(pandoc--extension-active-p ,(car ext) 'writer)))
                                   pandoc--extensions))))

    ("Files"
     ("Output File"
      ["Output To Stdout" (pandoc--set 'output nil) :active t
       :style radio :selected (null (pandoc--get 'output))]
      ["Create Output Filename" (pandoc--set 'output t) :active t
       :style radio :selected (eq (pandoc--get 'output) t)]
      ["Set Output File..." pandoc-set-output :active t
       :style radio :selected (stringp (pandoc--get 'output))])
     ("Output Directory"
      ["Use Input Directory" (pandoc--set 'output-dir nil) :active t
       :style radio :selected (null (pandoc--get 'output-dir))]
      ["Set Output Directory" pandoc-set-output-dir :active t
       :style radio :selected (pandoc--get 'output-dir)])
     ("Data Directory"
      ["Use Default Data Directory" (pandoc--set 'data-dir nil) :active t
       :style radio :selected (null (pandoc--get 'data-dir))]
      ["Set Data Directory" pandoc-set-data-dir :active t
       :style radio :selected (pandoc--get 'data-dir)])
     ("Extract Media"
      ["Do Not Extract Media Files" (pandoc--set 'extract-media nil) :active t
       :style radio :selected (null (pandoc--get 'extract-media))]
      ["Extract Media Files" pandoc-set-extract-media :active t
       :style radio :selected (pandoc--get 'extract-media)])
     ("Scope"
      ["Document Scope" (pandoc--set 'file-scope nil) :active t
       :style radio :selected (null (pandoc--get 'file-scope))]
      ["File Scope" pandoc-set-file-scope :active t
       :style radio :selected (pandoc--get 'file-scope)])
     ("Master File"
      ["No Master File" (pandoc-set-master-file '-) :active t :style radio :selected (null (pandoc--get 'master-file))]
      ["Use This File As Master File" pandoc-set-this-file-as-master :active t :style radio :selected (equal (pandoc--get 'master-file) (buffer-file-name))]
      ["Set Master File" pandoc-set-master-file :active t :style radio :selected (and (pandoc--get 'master-file) (not (equal (pandoc--get 'master-file) (buffer-file-name))))]))

    ("Reader Options"
     ,@pandoc--reader-menu-list)
    ("General Writer Options"
     ,@pandoc--writer-menu-list)
    ("Options For Specific Writers"
     ,@pandoc--specific-menu-list
     "--"
     ("HTML-Based Formats"
      ,@pandoc--html-menu-list)
     ("EPUB"
      ,@pandoc--epub-menu-list))
    ("Citations"
     ,@pandoc--citations-menu-list)
    ("Math Rendering"
     ["KaTeX" (lambda () (interactive) (pandoc-set-html-math-method nil "katex"))
      :active t :style 'radio :selected `(= "katex" (cdr (assq 'method (pandoc--get 'html-math-method))))]
     ["WebTeX" (lambda () (interactive) (pandoc-set-html-math-method nil "webtex"))
      :active t :style 'radio :selected `(= "katex" (cdr (assq 'method (pandoc--get 'html-math-method))))]
     ["gladTeX" (lambda () (interactive) (pandoc-set-html-math-method nil "gladTeX"))
      :active t :style 'radio :selected `(= "katex" (cdr (assq 'method (pandoc--get 'html-math-method))))]
     ["MathJax" (lambda () (interactive) (pandoc-set-html-math-method nil "mathjax"))
      :active t :style 'radio :selected `(= "katex" (cdr (assq 'method (pandoc--get 'html-math-method))))]
     ["MathML" (lambda () (interactive) (pandoc-set-html-math-method nil "mathml"))
      :active t :style 'radio :selected `(= "katex" (cdr (assq 'method (pandoc--get 'html-math-method))))]
     ["None" (lambda () (interactive) (pandoc-set-html-math-method nil nil))
      :active t :style 'radio :selected `(= "katex" (cdr (assq 'method (pandoc--get 'html-math-method))))]
     ["Set URL" (lambda () (interactive) (pandoc-set-html-math-method t (cdr (assq 'method (pandoc--get 'html-math-method)))))
      :active t :selected (cdr (assoc (cdr (assq 'method (pandoc--get 'html-math-method))) pandoc--html-math-methods))])
    ("Obsolete options"
     ,@pandoc--obsolete-menu-list)
    ("Verbosity"
     ["High (show all messages)" (pandoc--set 'verbosity "INFO")
      :active t :style 'radio :selected (equal (pandoc--get 'verbosity ) "INFO")]
     ["Medium (show warnings)" (pandoc--set 'verbosity nil)
      :active t :style 'radio :selected (not (pandoc--get 'verbosity ))]
     ["Low (show only error messages)" (pandoc--set 'verbosity "ERROR")
      :active t :style 'radio :selected (equal (pandoc--get 'verbosity ) "ERROR")])))

;; ("Options"
;;  ,@pandoc--options-menu)
;; ("Switches"
;;  ;; put the binary options into the menu
;;  ,@(mapcar (lambda (option)
;;              (vector (car option) `(pandoc--toggle (quote ,(cdr option)))
;;                      :active t
;;                      :style 'toggle
;;                      :selected `(pandoc--get (quote ,(cdr option)))))
;;            pandoc--switches))

;;;; Transients

(transient-define-prefix pandoc-main-transient ()
  "Pandoc-mode main menu."
  ["Pandoc\n"
   ("I" pandoc-input-formats-transient
    :description (lambda ()
                   (format "Input format   [%s%s]" (pandoc--get 'reader) (pandoc--format-extensions (pandoc--get 'read-extensions)))))
   ("O" pandoc-output-formats-transient
    :description (lambda ()
                   (format "Output format  [%s%s]" (pandoc--get 'writer) (pandoc--format-extensions (pandoc--get 'write-extensions)))))]
  [["Actions"
    ("r" "Run Pandoc"            pandoc-run-pandoc)
    ("p" "Convert to PDF"        pandoc-convert-to-pdf)
    ("v" "View output file"      pandoc-view-output)
    ("e" "Example lists"         pandoc-@-transient)]
   ["Settings"
    ("o" "Options"               pandoc-options-transient)
    ("s" "Settings files"        pandoc-settings-transient)
    ("S" "View current settings" pandoc-view-settings)]
   ["Buffers"
    ("B" "Output buffer"         pandoc-view-output-buffer)
    ("L" "Log buffer"            pandoc-view-log)]]

  [("q" "Quit" transient-quit-all)])

;;; Generate the main input & output format transients.
(transient-define-prefix pandoc-input-formats-transient ()
  "Pandoc-mode main input formats menu."
  [:class transient-column
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-input-formats-transient
             (vconcat (list "Input formats")
                      (mapcar (lambda (format)
                                (let ((submenu (nth 0 format))
                                      (description (nth 1 format))
                                      (key (nth 2 format)))
                                  (list key description (intern (format "pandoc-%s-input-formats-transient" submenu)))))
                              (mapcar (lambda (elt)
                                        (take 3 elt))
                                      pandoc--formats))
                      (list " "         ; empty line
                            '("X" "Extensions" pandoc-read-exts-transient)
                            " "
                            '("b" "Back" transient-quit-one)
                            '("q" "Quit" transient-quit-all)))))])

(transient-define-prefix pandoc-output-formats-transient ()
  "Pandoc-mode main output formats menu."
  [:class transient-column
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-output-formats-transient
             (vconcat (list "Output formats")
                      (mapcar (lambda (format)
                                (let ((submenu (nth 0 format))
                                      (description (nth 1 format))
                                      (key (nth 2 format)))
                                  (list key description (intern (format "pandoc-%s-output-formats-transient" submenu)))))
                              (mapcar (lambda (elt)
                                        (take 3 elt))
                                      pandoc--formats))
                      (list " "         ; empty line
                            '("X" "Extensions" pandoc-write-exts-transient)
                            " "
                            '("b" "Back" transient-quit-one)
                            '("q" "Quit" transient-quit-all)))))])

;;; Generate the specific input & output format transients.
(mapc (lambda (group)
        (let* ((category (car group))
               (group-description (nth 1 group))
               (transient-name (intern (format "pandoc-%s-input-formats-transient" category))))
          (eval `(transient-define-prefix ,transient-name ()
                   ,(format "Pandoc-mode %s input formats menu." category)
                   [:class transient-column
                           :pad-keys t
                           :setup-children
                           (lambda (_)
                             (transient-parse-suffixes
                              (quote ,transient-name)
                              (vconcat (list ,group-description)
                                       (mapcar ,(lambda (format-spec)
                                                  (let ((format-name (nth 0 format-spec))
                                                        (description (nth 1 format-spec))
                                                        (key (nth 2 format-spec)))
                                                    (list key description (lambda ()
                                                                            (interactive)
                                                                            (pandoc-set-read format-name))
                                                          :transient t)))
                                               (seq-filter (lambda (elt)
                                                             (not (eq (nth 3 elt) 'output)))
                                                           (drop 3 (quote ,group))))
                                       (list " " ; empty line
                                             '("b" "Back" transient-quit-one)
                                             '("q" "Quit" transient-quit-all)))))]))))
      pandoc--formats)

(mapc (lambda (group)
        (let* ((category (car group))
               (group-description (nth 1 group))
               (transient-name (intern (format "pandoc-%s-output-formats-transient" category))))
          (eval `(transient-define-prefix ,transient-name ()
                   ,(format "Pandoc-mode %s output formats menu." category)
                   [:class transient-column
                           :pad-keys t
                           :setup-children
                           (lambda (_)
                             (transient-parse-suffixes
                              (quote ,transient-name)
                              (vconcat (list ,group-description)
                                       (mapcar ,(lambda (format-spec)
                                                  (let ((format-name (nth 0 format-spec))
                                                        (description (nth 1 format-spec))
                                                        (key (nth 2 format-spec)))
                                                    (list key description (lambda ()
                                                                            (interactive)
                                                                            (pandoc-set-write format-name))
                                                          :transient t)))
                                               (seq-filter (lambda (elt)
                                                             (not (eq (nth 3 elt) 'input)))
                                                           (drop 3 (quote ,group))))
                                       (list " " ; empty line
                                             '("b" "Back" transient-quit-one)
                                             '("q" "Quit" transient-quit-all)))))]))))
      pandoc--formats)

(transient-define-prefix pandoc-settings-transient ()
  "Transient for settings files."
  ["Settings files"
   ("s" "Save file settings"            pandoc-save-settings)
   ("p" "Save project settings"         pandoc-save-project-settings)
   ("g" "Save global settings"          pandoc-save-global-settings)
   ("d" "Set current format as default" pandoc-set-default-format)
   ("r" "Revert settings"               pandoc-revert-settings)
   " "
   ("b" "Back"                          transient-quit-one)
   ("q" "Quit"                          transient-quit-all)])

(transient-define-prefix pandoc-@-transient ()
  "Transient for example lists."
  ["Example lists"
   ("i" "Insert new example" pandoc-insert-@)
   ("s" "Select and insert example reference" pandoc-select-@)
   " "
   ("b" "Back" transient-quit-one)
   ("q" "Quit" transient-quit-all)])

(transient-define-prefix pandoc-read-exts-transient ()
  "Pandoc-mode reader extensions menu."
  [:class transient-columns
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-read-exts-transient
             (let ((num 0))
               (append (mapcar
                        (lambda (partition)
                          (vconcat
                           (mapcar (lambda (elt)
                                     (let ((extension (car elt)))
                                       (list (format "%02d" (cl-incf num))
                                             `(lambda ()
                                                (interactive)
                                                (pandoc-toggle-extension ,extension 'reader))
                                             :description (lambda ()
                                                            (format " %s %s"
                                                                    (pandoc--extension-active-marker extension 'reader)
                                                                    extension))
                                             :transient t)))
                                   partition)))
                        (seq-partition pandoc--extensions 26))
                       (list [("b" "Back" transient-quit-one)
                              ("q" "Quit" transient-quit-all)])))))])

(transient-define-prefix pandoc-write-exts-transient ()
  "Pandoc-mode writer extensions menu."
  [:class transient-columns
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-write-exts-transient
             (let ((num 0))
               (append (mapcar
                        (lambda (partition)
                          (vconcat
                           (mapcar (lambda (elt)
                                     (let ((extension (car elt)))
                                       (list (format "%02d" (cl-incf num))
                                             `(lambda ()
                                                (interactive)
                                                (pandoc-toggle-extension ,extension 'writer))
                                             :description (lambda ()
                                                            (format " %s %s"
                                                                    (pandoc--extension-active-marker extension 'writer)
                                                                    extension))
                                             :transient t)))
                                   partition)))
                        (seq-partition pandoc--extensions 26))
                       (list [("b" "Back" transient-quit-one)
                              ("q" "Quit" transient-quit-all)])))))])

(transient-define-prefix pandoc-options-transient ()
  "Pandoc-mode options menu."
  ["Options menu"
   ("f" "Files"                        pandoc-file-transient)
   ("r" "Reader options"               pandoc-reader-options-transient)
   ("w" "General writer options"       pandoc-writer-options-transient)
   ("s" "Options for specific writers" pandoc-specific-options-transient)
   ("c" "Citations"                    pandoc-citations-transient)
   ("m" "Math rendering"               pandoc-math-transient)
   ("O" "Obsolete options"             pandoc-obsolete-options-transient)
   ("V" "Verbosity"                    pandoc-set-verbosity)
   " "
   ("b" "Back"                         transient-quit-one)
   ("q" "Quit"                         transient-quit-all)])

(transient-define-prefix pandoc-file-transient ()
  "Pandoc-mode file menu."
  ["File menu"
   ("o" pandoc-set-output
    :description (lambda ()
                   (format "%-27s[%s]" "Output file" (pandoc--pp-option 'output))))
   ("O" pandoc-set-output-dir
    :description (lambda ()
                   (format "%-27s[%s]" "Output directory" (pandoc--pp-option 'output-dir))))
   ("d" pandoc-set-defaults
    :description (lambda ()
                   (format "%-27s[%s]" "Data directory" (pandoc--pp-option 'data-dir))))
   ("D" pandoc-set-data-dir
    :description (lambda ()
                   (format "%-27s[%s]" "Defaults file" (pandoc--pp-option 'defaults))))
   ("e" pandoc-set-extract-media
    :description (lambda ()
                   (format "%-27s[%s]" "Extract media files" (pandoc--pp-option 'extract-media))))
   ("f" pandoc-set-file-scope
    :description (lambda ()
                   (format "%-27s[%s]" "File Scope" (pandoc--pp-option 'file-scope))))
   ("m" pandoc-set-master-file
    :description (lambda ()
                   (format "%-27s[%s]" "Master file" (pandoc--pp-option 'master-file))))
   ("M" "Use current file as master file" pandoc-set-this-file-as-master)
   " "
   ("b" "Back" transient-quit-one)
   ("q" "Quit" transient-quit-all)])

(transient-define-prefix pandoc-reader-options-transient ()
  "Pandoc-mode reader options menu."
  [:class transient-columns
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-reader-options-transient
             (list (vconcat (list "Reader options")
                            pandoc--reader-transient-list
                            '(" "
                              ("b" "Back" transient-quit-one)
                              ("q" "Quit" transient-quit-all))))))])

(transient-define-prefix pandoc-writer-options-transient ()
  "Pandoc-mode general writer options menu."
  [:class transient-columns
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-writer-options-transient
             (list (vconcat  (list "General writer options")
                             pandoc--writer-transient-list
                             '(" "
                               ("b" "Back" transient-quit-one)
                               ("q" "Quit" transient-quit-all))))))])

(transient-define-prefix pandoc-specific-options-transient ()
  "Pandoc-mode specific writer options menu."
  [:class transient-columns
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-specific-options-transient
             (list (vconcat  (list "Specific writer options")
                             pandoc--specific-transient-list
                             '(" "
                               ("H" "HTML-based writers" pandoc-html-options-transient)
                               ("E" "EPUB" pandoc-epub-options-transient)
                               " "
                               ("b" "Back" transient-quit-one)
                               ("q" "Quit" transient-quit-all))))))])

(transient-define-prefix pandoc-html-options-transient ()
  "Pandoc-mode HTML options menu."
  [:class transient-columns
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-html-options-transient
             (list (vconcat  (list "HTML-based writer options")
                             pandoc--html-transient-list
                             '(" "
                               ("b" "Back" transient-quit-one)
                               ("q" "Quit" transient-quit-all))))))])

(transient-define-prefix pandoc-obsolete-options-transient ()
  "Pandoc-mode obsolete options menu."
  [:class transient-columns
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-obsolete-options-transient
             (list (vconcat  (list "Obsolete options")
                             pandoc--obsolete-transient-list
                             '(" "
                               ("b" "Back" transient-quit-one)
                               ("q" "Quit" transient-quit-all))))))])

(transient-define-prefix pandoc-epub-options-transient ()
  "Pandoc-mode EPUB options menu."
  [:class transient-columns
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-epub-options-transient
             (list (vconcat  (list "TeX-based writer options")
                             pandoc--epub-transient-list
                             '(" "
                               ("b" "Back" transient-quit-one)
                               ("q" "Quit" transient-quit-all))))))])

(transient-define-prefix pandoc-citations-transient ()
  "Pandoc-mode citations menu."
  [:class transient-columns
          :pad-keys t
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-citations-transient
             (list (vconcat  (list "Citations menu")
                             pandoc--citations-transient-list
                             '(" "
                               ("b" "Back" transient-quit-one)
                               ("q" "Quit" transient-quit-all))))))])

(transient-define-prefix pandoc-math-transient ()
  "Pandoc-mode math rendering menu."
  [:description (lambda ()
                  (let* ((value (pandoc--get 'html-math-method))
                         (method (cdr (assq 'method value)))
                         (url (cdr (assq 'url value))))
                    (format "Math HTML method: %s%s"
                            (or method "none")
                            (if url (format " <%s>" url) ""))))
                ("k" "KaTeX" (lambda (prefix)
                               (interactive "P")
                               (pandoc-set-html-math-method prefix "katex"))
                 :transient t)
                ("w" "WebTeX" (lambda (prefix)
                                (interactive "P")
                                (pandoc-set-html-math-method prefix "webtex"))
                 :transient t)
                ("g" "gladTeX" (lambda (prefix)
                                 (interactive "P")
                                 (pandoc-set-html-math-method prefix "gladtex"))
                 :transient t)
                ("J" "MathJax" (lambda (prefix)
                                 (interactive "P")
                                 (pandoc-set-html-math-method prefix "mathjax"))
                 :transient t)
                ("m" "MathML" (lambda (prefix)
                                (interactive "P")
                                (pandoc-set-html-math-method prefix "mathml"))
                 :transient t)
                ("n" "None" (lambda ()
                              (interactive)
                              (pandoc--set 'html-math-method nil))
                 :transient t)
                " "
                ("b" "Back" transient-quit-one)
                ("q" "Quit" transient-quit-all)])

;;; Faces:
;;; Regexp based on github.com/vim-pandoc/vim-pandoc-syntax.
;;; Overall structure modeled after face handling in markdown-mode.el:
;;; http://jblevins.org/git/markdown-mode.git

(defvar pandoc-citation-key-face 'pandoc-citation-key-face
  "Face name to use for citations.")

(defvar pandoc-citation-marker-face 'pandoc-citation-marker-face
  "Face name to use for '@' citation identifier.")

(defvar pandoc-citation-extra-face 'pandoc-citation-extra-face
  "Face name to use for page numbers and other notation.")

(defvar pandoc-citation-brackets-face 'pandoc-citation-brackets-face
  "Face name to use for page numbers and other notation.")

(defvar pandoc-strikethrough-text-face 'pandoc-strikethrough-text-face
  "Face name to use for strikethrough text.")

(defvar pandoc-strikethrough-tilde-face 'pandoc-strikethrough-tilde-face
  "Face name to use for strikethrough delimiters.")

(defvar pandoc-directive-@@-face 'pandoc-directive-@@-face
  "Face name to use for '@@' in @@directives.")

(defvar pandoc-directive-type-face 'pandoc-directive-type-face
  "Face name to use for name of @@directives.")

(defvar pandoc-directive-braces-face 'pandoc-directive-braces-face
  "Face name to use for braces in @@directives.")

(defvar pandoc-directive-contents-face 'pandoc-directive-contents-face
  "Face name to use for contents of @@directives.")

(defface pandoc-citation-key-face
  '((t (:inherit font-lock-function-name-face)))
  "Base face for the key of Pandoc citations."
  :group 'pandoc)

(defface pandoc-strikethrough-text-face
  '((t (:strike-through t)))
  "Base face for Pandoc strikethrough text."
  :group 'pandoc)

(defface pandoc-strikethrough-tilde-face
  '((t (:inherit font-lock-warning-face)))
  "Base face for Pandoc strikethrough delimiters."
  :group 'pandoc)

(defface pandoc-directive-@@-face
  '((t (:inherit font-lock-type-face)))
  "Base face for pandoc-mode @@directive syntax."
  :group 'pandoc)

(defface pandoc-directive-type-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Base face for pandoc-mode @@directive type."
  :group 'pandoc)

(defface pandoc-directive-braces-face
  '((t (:inherit font-lock-variable-name-face)))
  "Base face for pandoc-mode @@directive braces."
  :group 'pandoc)

(defface pandoc-directive-contents-face
  '((t (:inherit font-lock-constant-face)))
  "Base face for pandoc-mode @@directive type."
  :group 'pandoc)

(defconst pandoc-regex-citation-key
  "[^[:alnum:]]\\(-?@\\([[:alnum:]_][[:alnum:]_:.#$%&+?<>~/-]*\\)\\)"
  "Regular expression for a citation key.")

(defconst pandoc-regex-strikethrough
  "\\(~\\{2\\}\\)\\([^~].*?\\)\\(~\\{2\\}\\)"
  "Regular expression for pandoc markdown's strikethrough syntax.")

(defconst pandoc-regex-@@-directive
  "\\(@@\\)\\(include\\|lisp\\)\\({\\)\\(.*?\\)\\(}\\)"
  "Regular expression for pandoc-mode's @@directives.")

(defvar pandoc-faces-keywords
  (list
   (cons pandoc-regex-@@-directive
   	 '((1 pandoc-directive-@@-face)
	   (2 pandoc-directive-type-face)
	   (3 pandoc-directive-braces-face)
   	   (4 pandoc-directive-contents-face)
	   (5 pandoc-directive-braces-face)))
   (cons pandoc-regex-citation-key
   	 '((1 pandoc-citation-key-face t)))
   (cons pandoc-regex-strikethrough
   	 '((1 pandoc-strikethrough-tilde-face)
   	   (2 pandoc-strikethrough-text-face )
   	   (3 pandoc-strikethrough-tilde-face))))
  "Keywords for pandoc faces.")

(defun pandoc-faces-load ()
  "Load pandoc-faces."
  (font-lock-add-keywords nil pandoc-faces-keywords t)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (with-no-warnings
      (font-lock-fontify-buffer))))

(defun pandoc-faces-unload ()
  "Unload pandoc-faces."
  (font-lock-remove-keywords nil pandoc-faces-keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (with-no-warnings
      (font-lock-fontify-buffer))))

;;; Citation jumping:
;;; Jump to citation in a bibliography file.

(defun pandoc-jump-to-reference ()
  "Display the BibTeX reference for the citation key at point.
Extract the key at point and pass it to the function in
`pandoc-citation-jump-function', together with a list of the
current buffer's BibTeX files."
  (interactive)
  (let ((biblist (pandoc--get 'bibliography)))
    (if biblist
        (if (thing-at-point-looking-at pandoc-regex-citation-key)
            (funcall pandoc-citation-jump-function (match-string-no-properties 2) biblist)
          (error "No citation at point"))
      (error "No bibliography selected"))))

(defun pandoc-goto-citation-reference (key biblist)
  "Open the BibTeX file containing the entry for KEY.
BIBLIST is a list of BibTeX files in which to search for KEY.
The first file in which KEY is found is opened in a new
window (using `find-file-other-window').

This function is the default value of `pandoc-citation-jump-function'."
  (let* ((key-regexp (concat "@[a-zA-Z]*[[:space:]]*[{(][[:space:]]*" key))
         (bibfile (cl-loop for file in biblist
                           if (with-temp-buffer
                                (insert-file-contents file)
                                (re-search-forward key-regexp nil t))
                           return file)))
    (if (not bibfile)
        (error "Key '%s' not found" key)
      (find-file-other-window bibfile)
      (goto-char (point-min))
      (re-search-forward key-regexp nil t)
      (beginning-of-line))))

(defun pandoc-open-in-ebib (key biblist)
  "Open BibTeX item KEY in Ebib.
BIBLIST is a list of BibTeX files in which to search for KEY.

This function is for use in `pandoc-citation-jump-function'."
  (let ((bibfile (cl-loop for file in biblist
                          if (with-temp-buffer
                               (insert-file-contents file)
                               (re-search-forward (concat "@[a-zA-Z]*[[:space:]]*[{(][[:space:]]*" key) nil t))
                          return file)))
    (if bibfile
        (ebib bibfile key)
      (error "Key '%s' not found" key))))

(defun pandoc-show-citation-as-help (key biblist)
  "Show the BibTeX item KEY in a *Help* buffer.
BIBLIST is a list of BibTeX files in which to search for KEY.

This function is for use in `pandoc-citation-jump-function'."
  (let ((entry (cl-loop for file in biblist
                        thereis (with-temp-buffer
                                  (insert-file-contents file)
                                  (when (re-search-forward (concat "@[a-zA-Z]*[[:space:]]*\\([{(]\\)[[:space:]]*" key) nil t)
                                    (beginning-of-line)
                                    (let ((beg (point)))
                                      (goto-char (match-beginning 1))
                                      (forward-list)
                                      (buffer-substring beg (point))))))))
    (if entry
        (with-help-window (help-buffer)
          (princ entry))
      (error "Key `%s' not found" key))))

(provide 'pandoc-mode)

;;; pandoc-mode.el ends here
