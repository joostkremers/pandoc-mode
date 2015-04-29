;;; pandoc-mode.el --- Minor mode for interacting with Pandoc

;; Copyright (c) 2009-2015 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 31 Oct 2009
;; Version: 2.12
;; Keywords: text, pandoc
;; Package-Requires: ((hydra "0.10.0") (dash "2.10.0"))

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
(require 'hydra)
(require 'dash)
(require 'pandoc-mode-utils)

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
    (move-overlay pandoc--@-overlay (point) (point-at-eol))))

(defun pandoc-prev-@ ()
  "Highlight previous (@)-definition."
  (interactive)
  (if (= (point) (point-min))
      (beep)
    (forward-line -2)
    (move-overlay pandoc--@-overlay (point) (point-at-eol))))

(defun pandoc-goto-first-@ ()
  "Highlight the first (@)-definition."
  (interactive)
  (goto-char (point-min))
  (move-overlay pandoc--@-overlay (point) (point-at-eol)))

(defun pandoc-goto-last-@ ()
  "Highlight the last (@)-definition."
  (interactive)
  (goto-char (point-max))
  (forward-line -2)
  (move-overlay pandoc--@-overlay (point) (point-at-eol)))

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
    (define-key map "\C-c/" #'pandoc-main-hydra/body)
    map)
  "Keymap for pandoc-mode")

;;;###autoload
(define-minor-mode pandoc-mode
  "Minor mode for interacting with Pandoc."
  :init-value nil :lighter (:eval (concat " Pandoc/" (pandoc--get 'write))) :global nil
  (cond
   (pandoc-mode    ; pandoc-mode is turned on
    (setq pandoc--local-settings (copy-tree pandoc--options))
    (pandoc--set 'read (cdr (assq major-mode pandoc-major-modes)))
    (setq pandoc--settings-modified-flag nil)
    (or (buffer-live-p pandoc--output-buffer)
        (setq pandoc--output-buffer (get-buffer-create " *Pandoc output*")))
    (pandoc-faces-load))
   ((not pandoc-mode)    ; pandoc-mode is turned off
    (setq pandoc--local-settings nil
          pandoc--settings-modified-flag nil)
    (pandoc-faces-unload))))

;;;###autoload
(defun conditionally-turn-on-pandoc ()
  "Turn on pandoc-mode if a pandoc settings file exists.
This is for use in major mode hooks."
  (when (and (buffer-file-name)
             (file-exists-p (pandoc--create-settings-filename 'local (buffer-file-name) "default")))
    (pandoc-mode 1)))

(defun pandoc-toggle-extension (extension rw)
  "Toggle the value of EXTENSION.
RW is either `read' or `write', indicating whether the extension
should be toggled for the input or the output format."
  (interactive (list (completing-read "Extension: " pandoc--extensions nil t)
                     (intern (completing-read "Read/write: " '("read" "write") nil t))))
  (let* ((current-value (pandoc--get-extension extension rw))
         (new-value (cond
                     ((memq current-value '(+ -)) ; if the value is set explicitly
                      nil)  ; we can simply return it to the default
                     ((pandoc--extension-in-format-p extension (pandoc--get rw) rw) ; if the extension is part of the current format
                      '-)  ; we explicitly unset it
                     (t '+)))) ; otherwise we explicitly set it
    (pandoc--set-extension extension rw new-value)))

(defun pandoc-toggle-read-extension-by-number (n)
  "Toggle a `read' extension.
N is the index of the extension in `pandoc--extensions'."
  (interactive "P")
  (let* ((ext (caar (nthcdr (1- n) pandoc--extensions))))
    (pandoc-toggle-extension ext 'read)))

(defun pandoc-toggle-write-extension-by-number (n)
  "Toggle a `write' extension.
N is the index of the extension in `pandoc--extensions'."
  (interactive "P")
  (let* ((ext (caar (nthcdr (1- n) pandoc--extensions))))
    (pandoc-toggle-extension ext 'write)))

(defun pandoc--create-settings-filename (type filename output-format)
  "Create a settings filename.
TYPE is the type of settings file, either 'local or 'project.
The return value is an absolute filename."
  (setq filename (expand-file-name filename))
  (cond
   ((eq type 'local)
    (concat (file-name-directory filename) "." (file-name-nondirectory filename) "." output-format ".pandoc"))
   ((eq type 'project)
    (concat (file-name-directory filename) "Project." output-format ".pandoc"))))

(defun pandoc--create-global-settings-filename (format)
  "Create a global settings filename.
FORMAT is the output format to use."
  (concat (file-name-as-directory pandoc-data-dir) format ".pandoc"))

(defun pandoc--format-all-options (input-file &optional pdf)
  "Create a list of strings with pandoc options for the current buffer.
INPUT-FILE is the name of the input file. If PDF is non-nil, an
output file is always set, derived either from the input file or
from the output file set for the \"latex\" output profile, and
gets the suffix `.pdf'. If the output format is \"odt\", \"epub\"
or \"docx\" but no output file is specified, one will be created,
since pandoc does not support output to stdout for those two
formats."
  (let ((read (format "--read=%s%s%s" (pandoc--get 'read) (if (pandoc--get 'read-lhs) "+lhs" "")
                      (pandoc--format-extensions (pandoc--get 'read-extensions))))
        (write (if pdf
                   nil
                 (format "--write=%s%s%s" (pandoc--get 'write) (if (pandoc--get 'write-lhs) "+lhs" "")
                         (pandoc--format-extensions (pandoc--get 'write-extensions)))))
        (output (pandoc--format-output-option input-file pdf))
        (list-options (mapcar (lambda (option)
                                (pandoc--format-list-options option (pandoc--get option)))
                              pandoc--list-options))
        (alist-options (mapcar (lambda (option)
                                 (pandoc--format-alist-options option (pandoc--get option)))
                               pandoc--alist-options))
        (cli-options (pandoc--format-cli-options)))
    ;; Note: list-options and alist-options are both lists of lists, so we need to flatten them first.
    (delq nil (append (list read write output) cli-options (apply #'append list-options) (apply #'append alist-options)))))

(defun pandoc--format-extensions (extensions)
  "Create a string of extensions to be added to the Pandoc command line."
  (mapconcat (lambda (elt)
               (if (cdr elt)
                   (format "%s%s" (cdr elt) (car elt))
                 ""))
             extensions
             ""))

(defun pandoc--format-output-option (input-file pdf)
  "Create the output option for calling Pandoc.
Return a string that can be added to the call to Pandoc."
  (cond
   ((or (eq (pandoc--get 'output) t) ; if the user set the output file to T
        (and (null (pandoc--get 'output)) ; or if the user set no output file but either
             (or pdf                    ; (i) we're converting to pdf, or
                 (member (pandoc--get 'write) ; (ii) the output format is odt, epub or docx
                         '("odt" "epub" "docx")))))
    (format "--output=%s/%s%s"          ; we create an output file name.
            (expand-file-name (or (pandoc--get 'output-dir)
                                  (file-name-directory input-file)))
            (file-name-sans-extension (file-name-nondirectory input-file))
            (if pdf
                ".pdf"
              (cadr (assoc (pandoc--get 'write) pandoc-output-format-extensions)))))
   ((stringp (pandoc--get 'output)) ; if the user set an output file,
    (format "--output=%s/%s"      ; we combine it with the output directory
            (expand-file-name (or (pandoc--get 'output-dir)
                                  (file-name-directory input-file)))
            (if pdf                  ; and check if we're converting to pdf
                (concat (file-name-sans-extension (pandoc--get 'output)) ".pdf")
              (pandoc--get 'output))))
   (t nil)))

(defun pandoc--format-list-options (option values)
  "Create a list of cli options for OPTION from the values in VALUES."
  (mapcar (lambda (value)
            (format "--%s=%s" option value))
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
              (when (and value (memq option pandoc--filepath-options))
                (setq value (expand-file-name value)))
              (cond
               ((eq value t) (format "--%s" option))
               ((or (numberp value)
                    (stringp value)) (format "--%s=%s" option value))
               (t nil))))
          pandoc--cli-options))

(defun pandoc-process-directives (output-format)
  "Processes pandoc-mode @@-directives in the current buffer.
OUTPUT-FORMAT is passed unchanged to the functions associated
with the @@-directives."
  (interactive (list (pandoc--get 'write)))
  (mapc #'funcall pandoc-directives-hook)
  (let ((case-fold-search nil))
    (mapc (lambda (directive)
            (goto-char (point-min))
            (while (re-search-forward (concat "\\([\\]?\\)@@" (car directive)) nil t)
              (if (string= (match-string 1) "\\")
                  (delete-region (match-beginning 1) (match-end 1))
                (let ((@@-beg (match-beginning 0))
                      (@@-end (match-end 0)))
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
                      (insert (funcall (cdr directive) output-format text)))
                    (goto-char @@-beg))
                   ;; check if the next character is not a letter or number.
                   ;; if it is, we're actually on a different directive.
                   ((looking-at "[a-zA-Z0-9]") t)
                   ;; otherwise there is no argument.
                   (t (goto-char @@-beg)
                      (delete-region @@-beg @@-end) ; else there is no argument
                      (insert (funcall (cdr directive) output-format))
                      (goto-char @@-beg)))))))
          pandoc-directives)))

(defun pandoc--process-lisp-directive (output-format lisp)
  "Process @@lisp directives."
  (format "%s" (eval (car (read-from-string lisp)))))

(defun pandoc--process-include-directive (output-format include-file)
  "Process @@include directives."
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
local options. The contents of the current buffer is copied into
the temporary buffer, the @@-directives are processed, after
which pandoc is called.

OUTPUT-FORMAT is the format to use. If nil, the current buffer's
output format is used. If PDF is non-nil, a pdf file is created.
REGION is a cons cell specifying the beginning and end of the
region to be sent to pandoc.

If the current buffer's \"master file\" option is set, that file
is processed instead. The output format is taken from the current
buffer, however, unless one is provided specifically. REGION is
also ignored in this case."
  (let* ((orig-buffer (current-buffer))
         (buffer (if (pandoc--get 'master-file)
                     (find-file-noselect (pandoc--get 'master-file))
                   (current-buffer)))
         (filename (buffer-file-name buffer)))
    ;; if there's a master file, ignore the region
    (if (pandoc--get 'master-file)
        (setq region nil))
    ;; keep track of the buffer-local value of `pandoc-binary', if there is one
    (setq pandoc--local-binary (buffer-local-value 'pandoc-binary buffer))
    (with-current-buffer buffer
      ;; we use with-current-buffer so that we can work on the master file
      ;; if there is one. we then change to a temp buffer, so we can
      ;; process @@-directives without having to undo them and set the
      ;; options independently of the original buffer.
      (with-temp-buffer
        (cond
         ;; if an output format was provided, try and load a settings file for it
         (output-format
          (unless (and filename
                       (pandoc--load-settings-for-file (expand-file-name filename) output-format t))
            ;; if no settings file was found, unset all options except input and output format
            (setq pandoc--local-settings (copy-tree pandoc--options))
            (pandoc--set 'write output-format)
            (pandoc--set 'read (pandoc--get 'read buffer))))
         ;; if no output format was provided, we use BUFFER's options,
         ;; except the output format, which we take from ORIG-BUFFER:
         (t (setq pandoc--local-settings (buffer-local-value 'pandoc--local-settings buffer))
            (pandoc--set 'write (pandoc--get 'write orig-buffer))))
        ;; copy any local `pandoc/' variables from `orig-buffer' or
        ;; `buffer' (the values in `orig-buffer' take precedence):
        (mapc (lambda (option)
                (let ((var (intern (concat "pandoc/" (symbol-name (car option))))))
                  (if (local-variable-p var orig-buffer)
                      (set (make-local-variable var) (buffer-local-value var orig-buffer))
                    (if (local-variable-p var buffer)
                        (set (make-local-variable var) (buffer-local-value var buffer))))))
              pandoc--options)
        (let ((option-list (pandoc--format-all-options filename pdf)))
          (insert-buffer-substring-no-properties buffer (car region) (cdr region))
          (message "Running %s..." (file-name-nondirectory pandoc--local-binary))
          (pandoc-process-directives (pandoc--get 'write))
          (with-pandoc-output-buffer
            (erase-buffer)
            (insert (format "Running `%s %s'\n\n" pandoc--local-binary (mapconcat #'identity option-list " "))))
          (let ((coding-system-for-read 'utf-8)
                (coding-system-for-write 'utf-8)
                (process (apply #'start-process "pandoc-process" pandoc--output-buffer pandoc--local-binary option-list)))
            (set-process-sentinel process (lambda (p e)
                                            (if (string-equal e "finished\n")
                                                (message "Running %s... Finished." (file-name-nondirectory pandoc--local-binary))
                                              (message "Error in %s process." (file-name-nondirectory pandoc--local-binary))
                                              (display-buffer pandoc--output-buffer))))
            (process-send-region process (point-min) (point-max))
            (process-send-eof process)))))))

(defun pandoc-run-pandoc (prefix)
  "Run pandoc on the current document.
If called with a prefix argument, the user is asked for an output
format. Otherwise, the output format currently set in the buffer
is used.

If the region is active, pandoc is run on the region instead of
the buffer."
  (interactive "P")
  (pandoc--call-external (if prefix
                             (completing-read "Output format to use: " pandoc--output-formats nil t)
                           nil)
                         nil
                         (if (use-region-p)
                             (cons (region-beginning) (region-end)))))

(defun pandoc-convert-to-pdf (prefix)
  "Convert the current document to pdf.
If the output format of the current buffer is set to \"latex\",
the buffer's options are used. If called with a prefix argument,
or if the current buffer's output format is not \"latex\", a
LaTeX settings file is searched for and loaded when found. If no
such settings file is found, all options are unset except for the
input and output formats.

If the region is active, pandoc is run on the region instead of
the buffer."
  (interactive "P")
  (pandoc--call-external (if (or prefix (not (string= (pandoc--get 'write) "latex")))
                             "latex"
                           nil)
                         t
                         (if (use-region-p)
                             (cons (region-beginning) (region-end)))))

(defun pandoc-set-default-format ()
  "Sets the current output format as default.
This is done by creating a symbolic link to the relevant settings
files. (Therefore, this function is not available on Windows.)"
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "This option is not available on MS Windows")
    (let ((current-settings-file
           (file-name-nondirectory (pandoc--create-settings-filename 'local (buffer-file-name)
                                                                     (pandoc--get 'write))))
          (current-project-file
           (file-name-nondirectory (pandoc--create-settings-filename 'project (buffer-file-name)
                                                                     (pandoc--get 'write)))))
      (when (not (file-exists-p current-settings-file))
        (pandoc--save-settings 'local (pandoc--get 'write)))
      (make-symbolic-link current-settings-file
                          (pandoc--create-settings-filename 'local (buffer-file-name) "default") t)
      (when (file-exists-p current-project-file)
        (make-symbolic-link current-project-file
                            (pandoc--create-settings-filename 'project (buffer-file-name) "default") t))
      (message "`%s' set as default output format." (pandoc--get 'write)))))

(defun pandoc-save-settings-file ()
  "Save the settings of the current buffer.
This function just calls pandoc--save-settings with the
appropriate output format."
  (interactive)
  (pandoc--save-settings 'local (pandoc--get 'write)))

(defun pandoc-save-project-file ()
  "Save the current settings as a project file."
  (interactive)
  (pandoc--save-settings 'project (pandoc--get 'write)))

(defun pandoc-save-global-settings-file ()
  "Save the current settings to a global settings file."
  (interactive)
  (pandoc--save-settings 'global (pandoc--get 'write)))

(defun pandoc--save-settings (type format &optional no-confirm)
  "Save the settings of the current buffer for FORMAT.
TYPE must be a quoted symbol and specifies the type of settings
file. If its value is 'local, a normal settings file is
created for the current file. If TYPE's value is 'project, a
project settings file is written. If optional argument NO-CONFIRM
is non-nil, any existing settings file is overwritten without
asking."
  (let* ((filename (buffer-file-name))
         (settings pandoc--local-settings)
         (settings-file (if (eq type 'global)
                            (pandoc--create-global-settings-filename format)
                          (pandoc--create-settings-filename type filename format))))
    (if (and (not no-confirm)
             (file-exists-p settings-file)
             (not (y-or-n-p (format "%s file `%s' already exists. Overwrite? "
                                    (capitalize (symbol-name type))
                                    (file-name-nondirectory settings-file)))))
        (message "%s file not written." (capitalize (symbol-name type)))
      (with-temp-buffer
        (let ((print-length nil)
              (print-level nil)
              (print-circle nil))
          (insert ";; -*- mode: emacs-lisp -*-\n\n"
                  (format ";; pandoc-mode %s settings file%s\n"
                          type
                          (if (eq type 'local)
                              (concat " for " (file-name-nondirectory filename))
                            ""))
                  (format ";; saved on %s\n\n" (format-time-string "%Y.%m.%d %H:%M")))
          (pp settings (current-buffer)))
        (let ((make-backup-files nil))
          (write-region (point-min) (point-max) settings-file))
        (message "%s settings file written to `%s'." (capitalize (symbol-name type)) (file-name-nondirectory settings-file)))
      (setq pandoc--settings-modified-flag nil))))

(defun pandoc-revert-settings ()
  "Revert settings for the current buffer.
The settings file is reread from disk, so that any changes made
to the settings that have not been saved are reverted."
  (interactive)
  (let ((format (pandoc--get 'write)))
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
  "Load the settings for FILE.
Load FILE's own settings file if it exists, otherwise check for a
project file and load that. If no project file exist, check if a
global settings file exist.

If NO-CONFIRM is t, no confirmation is asked if the current
settings have not been saved. FILE must be an absolute path name.
If FILE is nil, a global settings file is read, if any. The
settings are stored in the current buffer's
`pandoc--local-settings'. Return nil if no settings or project
file is found for FILE, otherwise non-nil."
  (when (and (not no-confirm)
             pandoc--settings-modified-flag
             (y-or-n-p (format "Current settings for format \"%s\" modified. Save first? " (pandoc--get 'write))))
    (pandoc--save-settings 'local (pandoc--get 'write) t))
  (let (settings)
    ;; first try to read local settings
    (when file
      (setq settings (cons 'local (pandoc--read-settings-from-file (pandoc--create-settings-filename 'local file format)))))
    ;; if it fails, try project settings
    (when (and file (not (cdr settings)))
      (setq settings (cons 'project (pandoc--read-settings-from-file (pandoc--create-settings-filename 'project file format)))))
    ;; if that fails too, or if there is no file, try reading global settings
    (unless (cdr settings)
      (setq settings (cons 'global (pandoc--read-settings-from-file (pandoc--create-global-settings-filename format)))))
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
asked to save the settings on exit. Return an alist with the
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

(defun pandoc-view-output ()
  "Displays the *Pandoc output* buffer."
  (interactive)
  (display-buffer pandoc--output-buffer))

(defun pandoc-view-settings ()
  "Displays the settings file in the *Pandoc output* buffer."
  (interactive)
  ;; remove all options that do not have a value.
  (let* ((remove-defaults (lambda (alist)
                            (delq nil (mapcar (lambda (option)
                                                (if (cdr option)
                                                    option))
                                              alist))))
         (settings (copy-tree pandoc--local-settings))
         (read-extensions (assq 'read-extensions settings))
         (write-extensions (assq 'write-extensions settings)))
    (when read-extensions
      (setcdr read-extensions (funcall remove-defaults (cdr read-extensions))))
    (when write-extensions
      (setcdr write-extensions (funcall remove-defaults (cdr write-extensions))))
    (setq settings (funcall remove-defaults settings))
    (with-current-buffer pandoc--output-buffer
      (let ((print-length nil)
            (print-level nil)
            (print-circle nil))
        (erase-buffer)
        (pp settings (current-buffer))))
    (display-buffer pandoc--output-buffer)))

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
      (setq pandoc--@-overlay (make-overlay (point-min) (point-at-eol)))
      (overlay-put pandoc--@-overlay 'face 'highlight))
    (select-window (display-buffer pandoc--@-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to set specific options. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pandoc-set-write (format)
  "Sets the output format to FORMAT.
If a settings and/or project file exists for FORMAT, they are
loaded. If none exists, all options are unset (except the input
format)."
  (interactive (list (completing-read "Set output format to: " pandoc--output-formats nil t)))
  (when (and pandoc--settings-modified-flag
             (y-or-n-p (format "Current settings for output format \"%s\" changed. Save? " (pandoc--get 'write))))
    (pandoc--save-settings 'local (pandoc--get 'write) t))
  (unless (pandoc--load-settings-profile format t)
    (setq pandoc--local-settings (copy-tree pandoc--options))
    (pandoc--set 'write format)
    (pandoc--set 'read (cdr (assq major-mode pandoc-major-modes))))
  (setq pandoc--settings-modified-flag nil)
  (message "Output format set to `%s'" format))

(defun pandoc-set-read (format)
  "Set the input format to FORMAT."
  (interactive (list (completing-read "Set input format to: " pandoc--input-formats nil t)))
  (pandoc--set 'read format)
  (message "Input format set to `%s'" format))

(defun pandoc-set-output (prefix)
  "Set the output file.
If called with the prefix argument C-u - (or M--), the output
file is unset. If called with any other prefix argument, the
output file is created on the basis of the input file and the
output format."
  (interactive "P")
  (pandoc--set 'output
               (cond
                ((eq prefix '-) nil)
                ((null prefix) (file-name-nondirectory (read-file-name "Output file: ")))
                (t t))))

(defun pandoc-set-data-dir (prefix)
  "Set the option `Data Directory'.
If called with the prefix argument C-u - (or M--), the data
directory is set to NIL, which means use $HOME/.pandoc."
  (interactive "P")
  (pandoc--set 'data-dir
               (if (eq prefix '-)
                   nil
                 (read-directory-name "Data directory: " nil nil t))))

(defun pandoc-set-output-dir (prefix)
  "Set the option `Output Directory'.
If called with the prefix argument C-u - (or M--), the output
directory is set to NIL, which means use the directory of the
input file."
  (interactive "P")
  (pandoc--set 'output-dir
               (if (eq prefix '-)
                   nil
                 (read-directory-name "Output directory: " nil nil t))))

(defun pandoc-set-extract-media (prefix)
  "Set the option `Extract media'.
If called with the prefix argument C-u - (or M--), no media files
are extracted."
  (interactive "P")
  (pandoc--set 'extract-media
               (if (eq prefix '-)
                   nil
                 (read-directory-name "Extract media files to directory: " nil nil t))))

(defun pandoc-set-master-file (prefix)
  "Set the master file.
If called with the prefix argumen C-u - (or M--), the master file
is set to nil, which means the current file is the master file."
  (interactive "P")
  (pandoc--set 'master-file
               (if (eq prefix '-)
                   nil
                 (read-file-name "Master file: "))))

(defun pandoc-set-this-file-as-master ()
  "Set the current file as master file.
This option creates a Project settings file in the current
directory to ensure that all files use the current file as master
file."
  (interactive)
  (pandoc--set 'master-file (buffer-file-name))
  (pandoc--save-settings 'project (pandoc--get 'write)))

(defun pandoc-toggle-interactive (prefix option)
  "Toggle one of pandoc's binary options.
If called with the prefix argument C-u - (or M--), the options is
unset. If called with any other prefix argument, the option is
set. Without any prefix argument, the option is toggled."
  (interactive "P")
  (let* ((completion-ignore-case t)
         (option (cdr (assoc (completing-read (format "%s option: " (cond
                                                                     ((eq prefix '-) "Unset")
                                                                     ((null prefix) "Toggle")
                                                                     (t "Set")))
                                              pandoc--switches nil t) pandoc--switches))))
    (pandoc--set option (cond
                         ((eq prefix '-) nil)
                         ((null prefix) (not (pandoc--get option)))
                         (t t)))
    (message "Option `%s' %s." (car (rassq option pandoc--switches)) (if (pandoc--get option)
                                                                         "set"
                                                                       "unset"))))

(easy-menu-define pandoc-mode-menu pandoc-mode-map "Pandoc menu"
  `("Pandoc"
    ["Run Pandoc" pandoc-run-pandoc :active t]
    ["Create PDF" pandoc-convert-to-pdf :active t]
    ["View Output Buffer" pandoc-view-output :active t]
    ("Settings Files"
     ["Save File Settings" pandoc-save-settings-file :active t]
     ["Save Project File" pandoc-save-project-file :active t]
     ["Save Global Settings File" pandoc-save-global-settings-file :active t]
     ["Revert Settings" pandoc-revert-settings :active t]
     ["Set As Default Format" pandoc-set-default-format :active (not (eq system-type 'windows-nt))])
    ("Example Lists"
     ["Insert New Example" pandoc-insert-@ :active t]
     ["Select And Insert Example Label" pandoc-select-@ :active t])
    "--"
    ["View Current Settings" pandoc-view-settings :active t]

    ,(append (cons "Input Format"
                   (mapcar (lambda (option)
                             (vector (car option)
                                     `(pandoc--set 'read ,(cdr option))
                                     :active t
                                     :style 'radio
                                     :selected `(string= (pandoc--get 'read)
                                                         ,(cdr option))))
                           pandoc--input-formats-menu))
             (list ["Literal Haskell" (pandoc--toggle 'read-lhs)
                    :active (member (pandoc--get 'read) '("markdown" "rst" "latex"))
                    :style toggle :selected (pandoc--get 'read-lhs)])
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc--get 'read)))
                           (mapcar (lambda (ext)
                                     (vector (car ext)
                                             `(pandoc-toggle-extension ,(car ext) 'read)
                                             :active t
                                             :style 'toggle
                                             :selected `(pandoc--extension-active-p ,(car ext) 'read)))
                                   pandoc--extensions))))

    ,(append (cons "Output Format"
                   (mapcar (lambda (option)
                             (vector (cadr option)
                                     `(pandoc-set-write ,(car option))
                                     :active t
                                     :style 'radio
                                     :selected `(string= (pandoc--get 'write)
                                                         ,(car option))))
                           pandoc--output-formats))
             (list ["Literal Haskell" (pandoc--toggle 'write-lhs)
                    :active (member (pandoc--get 'write)
                                    '("markdown" "rst" "latex" "beamer" "html" "html5"))
                    :style toggle :selected (pandoc--get 'write-lhs)])
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc--get 'write)))
                           (mapcar (lambda (ext)
                                     (vector (car ext)
                                             `(pandoc-toggle-extension ,(car ext) 'write)
                                             :active t
                                             :style 'toggle
                                             :selected `(pandoc--extension-active-p ,(car ext) 'write)))
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
     ,@pandoc--files-menu
     ("Extract Media"
      ["Do Not Extract Media Files" (pandoc--set 'extract-media nil) :active t
       :style radio :selected (null (pandoc--get 'extract-media))]
      ["Extract Media Files" pandoc-set-extract-media :active t
       :style radio :selected (pandoc--get 'extract-media)])
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
     ("TeX-Based Formats"
      ,@pandoc--tex-menu-list)
     ("EPUB"
      ,@pandoc--epub-menu-list))
    ("Citations"
     ,@pandoc--citations-menu-list)
    ("Math Rendering"
     ,@pandoc--math-menu-list)))

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

(easy-menu-add pandoc-mode-menu pandoc-mode-map)

;; hydras

(defhydra pandoc-main-hydra (:foreign-keys warn :exit t :hint nil)
  "
Main menu

_r_: Run Pandoc               _I_: Input format
_p_: Convert to pdf           _O_: Output format
_V_: View output buffer       _s_: Settings files
_S_: View current settings    _e_: Example lists
_o_: Options

"
  ("r" pandoc-run-pandoc)
  ("p" pandoc-convert-to-pdf)
  ("V" pandoc-view-output)
  ("S" pandoc-view-settings)
  ("I" pandoc-input-format-hydra/body)
  ("O" pandoc-output-format-hydra/body)
  ("s" pandoc-settings-file-hydra/body)
  ("e" pandoc-@-hydra/body)
  ("o" pandoc-options-hydra/body)
  ("q" nil "Quit"))

(define-pandoc-hydra pandoc-input-format-hydra (:foreign-keys warn :exit t :hint nil)
  (concat "Input format: %s(pandoc--get 'read)\n\n"
          (pandoc--tabulate-input-formats)
          "\n"
          (make-string 50 ?-)
          "\n"
          "_X_: Extensions\n\n")
  (--map (list (caddr it) (list 'pandoc-set-read (car it)))
         pandoc--input-formats)
  ("X" pandoc-read-exts-hydra/body)
  ("q" nil "Quit")
  ("b" pandoc-main-hydra/body "Back"))

(define-pandoc-hydra pandoc-output-format-hydra (:foreign-keys warn :hint nil)
  (concat "Output format: %s(pandoc--get 'write)\n\n"
          (pandoc--tabulate-output-formats)
          "\n"
          (make-string 50 ?-)
          "\n"
          "_X_: Extensions\n\n")
  (--map (list (caddr it) (list 'pandoc-set-write (car it)))
         pandoc--output-formats)
  ("X" pandoc-write-exts-hydra/body :exit t)
  ("q" nil "Quit")
  ("b" pandoc-main-hydra/body "Back" :exit t))

(defhydra pandoc-settings-file-hydra (:foreign-keys warn :hint nil)
  "
Settings files

_s_: Save file settings
_p_: Save project file
_g_: Save global settings file
_d_: Set current format as default
_r_: Revert settings

"
  ("s" pandoc-save-settings-file)
  ("p" pandoc-save-project-file)
  ("g" pandoc-save-global-settings-file)
  ("d" pandoc-set-default-format)
  ("r" pandoc-revert-settings)
  ("q" nil "Quit")
  ("b" pandoc-main-hydra/body "Back" :exit t))

(defhydra pandoc-@-hydra (:foreign-keys warn :exit t :hint nil)
  "
Example lists

_i_: Insert new example
_s_: Select and insert example label

"
  ("i" pandoc-insert-@)
  ("s" pandoc-select-@)
  ("q" nil "Quit")
  ("b" pandoc-main-hydra/body "Back"))

(defun pandoc--extension-active-marker (extension rw)
  "Return a marker indicating whether EXTENSION is active."
  (if (pandoc--extension-active-p extension rw)
      pandoc-extension-active-marker
    pandoc-extension-inactive-marker))

(defhydra pandoc-read-exts-hydra (:foreign-keys warn :hint nil)
  (concat "\n" (pandoc--tabulate-extensions 'read) "\n\n<number> [_t_]: Toggle extension, [_q_]: Quit, [_b_]: Back")
  ("t" pandoc-toggle-read-extension-by-number)
  ("q" nil)
  ("b" pandoc-input-format-hydra/body :exit t))

(defhydra pandoc-write-exts-hydra (:foreign-keys warn :hint nil)
  (concat "\n" (pandoc--tabulate-extensions 'write) "\n\n<number> [_t_]: Toggle extension, [_q_]: Quit, [_b_]: Back")
  ("t" pandoc-toggle-write-extension-by-number)
  ("q" nil)
  ("b" pandoc-output-format-hydra/body :exit t))

(defhydra pandoc-options-hydra (:foreign-keys warn :exit t :hint nil)
  "
Options menu

_f_: Files
_r_: Reader options
_w_: General writer options
_s_: Options for specific writers
_c_: Citations
_m_: Math rendering

"
  ("f" pandoc-file-hydra/body)
  ("r" pandoc-reader-options-hydra/body)
  ("w" pandoc-writer-options-hydra/body)
  ("s" pandoc-specific-options-hydra/body)
  ("c" pandoc-citations-hydra/body)
  ("m" pandoc-math-hydra/body)
  ("q" nil "Quit")
  ("b" pandoc-main-hydra/body "Back"))

(defhydra pandoc-file-hydra (:foreign-keys warn :hint nil)
  "
File options

_o_: Output file           [%s(pandoc--pp-option 'output)]
_O_: Output directory      [%s(pandoc--pp-option 'output-dir)]
_d_: Data directory        [%s(pandoc--pp-option 'data-dir)]
_e_: Extract media files   [%s(pandoc--pp-option 'extract-media)]
_m_: Master file           [%s(pandoc--pp-option 'master-file)]
_M_: Use current file as master file

"
  ("o" pandoc-set-output)
  ("O" pandoc-set-output-dir)
  ("d" pandoc-set-data-dir)
  ("e" pandoc-set-extract-media)
  ("m" pandoc-set-master-file)
  ("M" pandoc-set-this-file-as-master)
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-reader-options-hydra (:foreign-keys warn :hint nil)
  (concat "Reader options"
          "\n\n"
          (mapconcat #'car pandoc--reader-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--reader-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-writer-options-hydra (:foreign-keys warn :hint nil)
  (concat "General writer options"
          "\n\n"
          (mapconcat #'car pandoc--writer-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--writer-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-specific-options-hydra (:foreign-keys warn :hint nil)
  (concat "Specific writer options"
          "\n\n"
          (mapconcat #'car pandoc--specific-hydra-list "\n")
          "\n"
          (make-string 50 ?-)
          "\n"
          "_H_: HTML-based writers\n"
          "_T_: TeX-based writers\n"
          "_E_: Epub"
          "\n\n")
  (mapcar #'cdr pandoc--specific-hydra-list)
  ("H" pandoc-html-options-hydra/body :exit t)
  ("T" pandoc-tex-options-hydra/body :exit t)
  ("E" pandoc-epub-options-hydra/body :exit t)
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-html-options-hydra (:foreign-keys warn :hint nil)
  (concat "HTML-based writer options"
          "\n\n"
          (mapconcat #'car pandoc--html-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--html-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-specific-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-tex-options-hydra (:foreign-keys warn :hint nil)
  (concat "TeX-based writer options"
          "\n\n"
          (mapconcat #'car pandoc--tex-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--tex-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-specific-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-epub-options-hydra (:foreign-keys warn :hint nil)
  (concat "Epub writer options"
          "\n\n"
          (mapconcat #'car pandoc--epub-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--epub-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-specific-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-citations-hydra (:foreign-keys warn :hint nil)
  (concat "Citations menu"
          "\n\n"
          (mapconcat #'car pandoc--citations-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--citations-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t))

(define-pandoc-hydra pandoc-math-hydra (:foreign-keys warn :hint nil)
  (concat "Math rendering"
          "\n\n"
          (mapconcat #'car pandoc--math-hydra-list "\n")
          "\n\n")
  (mapcar #'cdr pandoc--math-hydra-list)
  ("q" nil "Quit")
  ("b" pandoc-options-hydra/body "Back" :exit t))


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


(defface pandoc-citation-key-face
  '((t (:inherit font-lock-function-name-face)))
  "Base face for pandoc citations."
  :group 'pandoc)

(defface pandoc-citation-marker-face
  '((t (:inherit font-lock-variable-name-face)))
  "Base face for pandoc citation marker '@'."
  :group 'pandoc)

(defface pandoc-citation-extra-face
  '((t (:inherit font-lock-variable-name-face)))
  "Base face for page numbers and other pandoc citation notation."
  :group 'pandoc)

(defface pandoc-citation-brackets-face
  '((t (:inherit font-lock-function-name-face)))
  "Base face for pandoc citation brackets."
  :group 'pandoc)

(defface pandoc-strikethrough-text-face
  '((t (:strike-through t)))
  "Base face for pandoc strikethrough text."
  :group 'pandoc)

(defface pandoc-strikethrough-tilde-face
  '((t (:inherit font-lock-warning-face)))
  "Base face for pandoc strikethrough delimiters."
  :group 'pandoc)

(defconst pandoc-regex-parenthetical-citation-single
  "\\(\\[\\)\\(-?@\\)\\([-a-zA-Z0-9_+:]*\\)\\(\\]\\)"
  "Regular expression for parenthetical citations with only one key.")

(defconst pandoc-regex-parenthetical-citation-multiple
  "\\(\\[\\)\\(-?@\\)\\([-a-zA-Z0-9_+:]*\\)\\(.*?\\)\\(\\]\\)"
  "Regular expression for parenthetical citations with page numbers or multiple keys.")

(defconst pandoc-regex-in-text-citation
  "\\[\\{0\\}\\(-?@\\)\\([-a-zA-Z0-9_+:]*\\)\\s-\\(\\[\\)\\(.*?\\)\\(\\]\\)"
  "Regular expression for stand-alone citation with anchor.")

(defconst pandoc-regex-in-text-citation-2
  "\\<\\(-?@\\)\\([-a-zA-Z0-9_+:]*\\)"
  "Regular expression for stand-alone citation with no anchor.")

(defconst pandoc-regex-strikethrough
  "\\(~\\{2\\}\\)\\([^~].*?\\)\\(~\\{2\\}\\)"
  "Regular expression for pandoc markdown's strikethrough syntax.")

(defvar pandoc-faces-keywords
  (list
   (cons pandoc-regex-parenthetical-citation-single
   	 '((1 pandoc-citation-brackets-face t)
   	   (2 pandoc-citation-marker-face)
   	   (3 pandoc-citation-key-face)
   	   (4 pandoc-citation-brackets-face t)))
   (cons pandoc-regex-parenthetical-citation-multiple
         '((1 pandoc-citation-brackets-face t)
           (2 pandoc-citation-marker-face)
           (3 pandoc-citation-key-face)
           (4 pandoc-citation-extra-face t)
           (5 pandoc-citation-brackets-face t)))
   (cons pandoc-regex-in-text-citation
	 '((1 pandoc-citation-marker-face)
	   (2 pandoc-citation-key-face)
	   (3 pandoc-citation-brackets-face)
	   (4 pandoc-citation-extra-face)
	   (5 pandoc-citation-brackets-face)))
   (cons pandoc-regex-in-text-citation-2
	 '((1 pandoc-citation-marker-face t)
	   (2 pandoc-citation-key-face t)))
   (cons pandoc-regex-strikethrough
   	 '((1 pandoc-strikethrough-tilde-face)
   	   (2 pandoc-strikethrough-text-face )
   	   (3 pandoc-strikethrough-tilde-face))))
  "Keywords for pandoc faces.")

(defun pandoc-faces-load ()
  "Load pandoc-faces."
  (font-lock-add-keywords nil pandoc-faces-keywords)
  (font-lock-fontify-buffer))

(defun pandoc-faces-unload ()
  "Unload pandoc-faces."
  (font-lock-remove-keywords nil pandoc-faces-keywords)
  (font-lock-fontify-buffer))

(provide 'pandoc-mode)

;;; pandoc-mode.el ends here
