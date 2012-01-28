;;; pandoc-mode.el --- Interact with Pandoc
;;
;; Copyright (c) 2009-2011 Joost Kremers
;; All rights reserved.
;;
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

;; Provide a mode for interacting with Pandoc.

;;; Code:

(require 'easymenu)

(defmacro nor (&rest args)
  "Return T if none of its arguments are true."
  `(not (or ,@args)))

(defun nonempty (string)
  "Return STRING, unless it is \"\", in which case return NIL."
  (when (not (string= string ""))
    string))

(defgroup pandoc nil "Minor mode for interacting with pandoc." :group 'Wp)

(defcustom pandoc-binary "/usr/bin/pandoc"
  "*The full path of the pandoc binary."
  :group 'pandoc
  :type 'file)

(defcustom pandoc-markdown2pdf-script "/usr/bin/markdown2pdf"
  "*The full path of the markdown2pdf script."
  :group 'pandoc
  :type 'file)

(defcustom pandoc-directives '(("include" . pandoc-process-include-directive)
			       ("lisp" . pandoc-process-lisp-directive))
  "*List of directives to be processed before pandoc is called.
The directive must be given without `@@'; the function should
return a string that will replace the directive and its
argument (if any).

The directives are processed in the order in which they appear in
this list. If a directive produces output that contains another
directive, the new directive will only be processed if it is of
the same type (i.e., an @@include directive loading a text that
also contains @@include directives) or if it is lower on the
list, not if it appears higher on the list."
  :group 'pandoc
  :type '(alist :key-type (string :tag "Directive") :value-type function))

(defcustom pandoc-directives-hook nil
  "*List of functions to call before the directives are processed."
  :group 'pandoc
  :type '(repeat function))

(defvar pandoc-major-modes
  '((haskell-mode . "native")
    (text-mode . "markdown")
    (markdown-mode . "markdown")
    (rst-mode . "rst")
    (html-mode . "html")
    (latex-mode . "latex")))

(defvar pandoc-input-formats
  '("native"
    "markdown"
    "rst"
    "html"
    "latex"
    "textile"
    "json")
  "List of pandoc input formats.")

(defvar pandoc-output-formats
  '(("native" . ".hs")
    ("plain" . ".txt")
    ("markdown" . ".text")
    ("rst" . ".rst")
    ("html" . ".html")
    ("latex" . ".tex")
    ("context" . ".tex")
    ("man" . "")
    ("mediawiki" . ".mw")
    ("texinfo" . ".texi")
    ("docbook" . ".xml")
    ("epub" . ".epub")
    ("opendocument" . ".odf")
    ("odt" . ".odt")
    ("s5" . ".html")
    ("slidy" . ".html")
    ("rtf" . ".rtf")
    ("textile" . ".textile")
    ("org" . ".org")
    ("json" . ".json"))
  "List of pandoc output formats plus file extensions.")

(defvar pandoc-switches
  '(standalone
    preserve-tabs
    tab-stop
    strict
    normalize
    reference-links
    parse-raw
    smart
    html5
    latexmathml
    mathml
    mimetex
    webtex
    jsmath
    mathjax
    gladtex
    incremental
    offline
    xetex
    chapters
    number-sections
    listings
    section-divs
    no-wrap
    columns
    email-obfuscation
    id-prefix
    indented-code-classes
    table-of-contents
    base-header-level
    template
    variable
    css
    include-in-header
    include-before-body
    include-after-body
    title-prefix
    reference-odt
    epub-stylesheet
    epub-cover-image
    epub-metadata
    print-default-template
    bibliography
    csl
    natbib
    biblatex
    data-dir)
  "List of switches accepted by the pandoc binary. Switches that
  need special treatment (--read, --write and --output) are not in this list.")

(defvar pandoc-markdown2pdf-switches
  '(preserve-tabs
    tab-stop
    strict
    parse-raw
    xetex
    number-sections
    listings
    table-of-contents
    template
    variable
    include-in-header
    include-before-body
    include-after-body
    bibliography
    csl
    data-dir)
  "List of switches accepted by the markdown2pdf binary. Switches
  that need special treatment (--read and --write) are not in
  this list.")

(defvar pandoc-filepath-switches
  '(data-dir
    include-in-header
    include-before-body
    include-after-body
    custom-header
    template
    epub-stylesheet
    epub-cover-image
    epub-metadata
    reference-odt
    bibliography ; TODO
    csl) ; TODO
  "List of switches that have a file path as value, which are
  expanded before they are sent to pandoc. For relative paths,
  the file's working directory is used as base directory.")

(defvar pandoc-binary-switches
  '(("Standalone" . standalone)
    ("Preserve Tabs" . preserve-tabs)
    ("Strict" . strict)
    ("Normalize Document" . normalize)
    ("Reference Links" . reference-links)
    ("Parse Raw" . parse-raw)
    ("Smart" . smart)
    ("HTML5" . html5)
    ("gladTeX" . gladtex)
    ("Incremental" . incremental)
    ("Offline" . offline)
    ("XeTeX" . xetex)
    ("Top-level headers are chapters" . chapters)
    ("Number Sections" . number-sections)
    ("Use LaTeX listings Package" . listings)
    ("Wrap Sections in <div> Tags" . section-divs)
    ("No Wrap" . no-wrap)
    ("Table of Contents" . table-of-contents)
    ("Use NatBib" . natbib)
    ("Use BibLaTeX" . biblatex)
    ("Use only ASCII in HTML" . ascii)
    ;; ("Sanitize HTML" . sanitize-html) ; removed?
    ))

(defvar pandoc-options
  '((read)                         ; see pandoc-input-formats
    (read-lhs)                     ; input is literal Haskell
    (write . "native")             ; see pandoc-output-formats
    (write-lhs)                    ; output is literal Haskell

    ;; if the value of an option is NIL, it is *not* passed to pandoc. filepaths
    ;; are expanded before beiing passed to pandoc, filenames are not. TODO

    ;; possible value: a filename, NIL or T
    (output)
    (epub-stylesheet)

    ;; possible value: a directory path or NIL
    (data-dir)
    (output-dir) ; this is not actually a pandoc option

    ;; possible value: a filename or NIL
    (css)

    ;; possible value: filepath or NIL
    (include-in-header)
    (include-before-body)
    (include-after-body)
    (custom-header)
    (template)
    (reference-odt)
    (epub-cover-image)
    (epub-metadata)
    (bibliography)
    (csl)

    ;; possibe values: an integer or NIL
    (columns)
    (tab-stop)
    (base-header-level)

    ;; possible values: a string, NIL or T
    (latexmathml)
    (mathml)
    (mimetex)
    (webtex)
    (jsmath)

    ;; possible values: a string or NIL
    (mathjax)
    (title-prefix)
    (id-prefix)
    (indented-code-classes)

    ;; possible values: an alist or NIL
    (variable)

    ;; possible values: "none", "javascript" or "references"
    (email-obfuscation . "none")

    ;; possible values: NIL or T (binary switches)
    (standalone)
    (preserve-tabs)
    (strict)
    (normalize)
    (reference-links)
    (parse-raw)
    (smart)
    (html5)
    (gladtex)
    (incremental)
    (offline)
    (xetex)
    (chapters)
    (number-sections)
    (listings)
    (section-divs)
    (no-wrap)
    (table-of-contents)
    (natbib)
    (biblatex)
    (ascii)
    ;; (sanitize-html) ; removed?
    )
  "Pandoc option alist.")

;; Regarding storing of options: both per-file options and project are stored in
;; buffer-local variables. Note that pandoc-local-options stores *all* options,
;; the per-file ones and the project ones. The reason for this is that the
;; per-file (a.k.a. local) options should be able to override the project
;; options. We cannot say that if a local option has the value NIL, we check the
;; project option and use its value if it has one, because the NIL value of the
;; local option may actually be the overriding value.

(defvar pandoc-local-options nil "A buffer-local variable holding a file's pandoc options.")
(make-variable-buffer-local 'pandoc-local-options)

(defvar pandoc-project-options nil "A buffer-local variable holding a file's project options.")
(make-variable-buffer-local 'pandoc-project-options)

(defvar pandoc-settings-modified-flag nil "T if the current settings were modified and not saved.")
(make-variable-buffer-local 'pandoc-settings-modified-flag)

(defvar pandoc-output-buffer (get-buffer-create " *Pandoc output*"))

(defvar pandoc-@-counter 0 "Counter for (@)-lists.")
(make-variable-buffer-local 'pandoc-@-counter)

(defvar pandoc-window-config nil
  "Stores the window configuration before calling pandoc-select-@.")

(defvar pandoc-pre-select-buffer nil
  "Buffer from which pandoc-@-select is called.")

(defvar pandoc-@-buffer nil
  "Buffer for selecting an (@)-element.")

(defvar pandoc-@-overlay nil
  "Overlay for pandoc-@-buffer.")

(defun pandoc-@-counter-inc ()
  "Increment pandoc-@-counter and return the new value."
  (when (= pandoc-@-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(@\\([0-9]+?\\))" (point-max) t)
        (let ((label (string-to-number (match-string 1))))
          (when (> label pandoc-@-counter)
            (setq pandoc-@-counter label))))))
  (incf pandoc-@-counter))

(defvar pandoc-@-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'pandoc-quit-@-select)
    (define-key map "j" 'pandoc-next-@)
    (define-key map "n" 'pandoc-next-@)
    (define-key map [down] 'pandoc-next-@)
    (define-key map "k" 'pandoc-prev-@)
    (define-key map "p" 'pandoc-prev-@)
    (define-key map [up] 'pandoc-prev-@)
    (define-key map [return] 'pandoc-select-current-@)
    (define-key map [home] 'pandoc-goto-first-@)
    (define-key map [prior] 'pandoc-goto-first-@)
    (define-key map [end] 'pandoc-goto-last-@)
    (define-key map [next] 'pandoc-goto-first-@)
    map)
  "Keymap for pandoc-@-mode.")

(defun pandoc-quit-@-select ()
  "Leave pandoc-@-select-buffer without selecting an (@)-label."
  (interactive)
  (remove-overlays)
  (set-window-configuration pandoc-window-config)
  (switch-to-buffer pandoc-pre-select-buffer))

(defun pandoc-next-@ ()
  "Highlight next (@)-definition."
  (interactive)
  (if (= (count-lines (point) (point-max)) 2)
      (beep)
    (forward-line 2)
    (move-overlay pandoc-@-overlay (point) (point-at-eol))))

(defun pandoc-prev-@ ()
  "Highlight previous (@)-definition."
  (interactive)
  (if (= (point) (point-min))
      (beep)
    (forward-line -2)
    (move-overlay pandoc-@-overlay (point) (point-at-eol))))

(defun pandoc-goto-first-@ ()
  "Highlight the first (@)-definition."
  (interactive)
  (goto-char (point-min))
  (move-overlay pandoc-@-overlay (point) (point-at-eol)))

(defun pandoc-goto-last-@ ()
  "Highlight the last (@)-definition."
  (interactive)
  (goto-char (point-max))
  (forward-line -2)
  (move-overlay pandoc-@-overlay (point) (point-at-eol)))

(defun pandoc-select-current-@ ()
  "Leave pandoc-@-select-buffer and insert selected (@)-label at point."
  (interactive)
  (looking-at " \\((@.*?)\\)")
  (let ((label (match-string 1)))
    (remove-overlays)
    (set-window-configuration pandoc-window-config)
    (switch-to-buffer pandoc-pre-select-buffer)
    (insert label)))

(define-derived-mode pandoc-@-mode
  fundamental-mode "Pandoc-select"
  "Major mode for the Pandoc-select buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defvar pandoc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c/r" 'pandoc-run-pandoc)
    (define-key map "\C-c/p" 'pandoc-run-markdown2pdf)
    (define-key map "\C-c/s" 'pandoc-save-settings-file)
    (define-key map "\C-c/Ps" 'pandoc-save-project-file)
    (define-key map "\C-c/Pu" 'pandoc-undo-file-settings)
    (define-key map "\C-c/w" 'pandoc-set-write)
    (define-key map "\C-c/v" 'pandoc-set-template-variable)
    (define-key map "\C-c/V" 'pandoc-view-output)
    (define-key map "\C-c/S" 'pandoc-view-settings)
    (define-key map "\C-c/c" 'pandoc-insert-@)
    (define-key map "\C-c/C" 'pandoc-select-@)
    map)
  "Keymap for pandoc-mode.")

(define-minor-mode pandoc-mode
  "Minor mode for interacting with Pandoc."
  :init-value nil :lighter (:eval (concat " Pandoc/" (pandoc-get 'write))) :global nil
  (setq pandoc-local-options (copy-alist pandoc-options))
  (pandoc-set 'read (cdr (assq major-mode pandoc-major-modes)))
  (setq pandoc-settings-modified-flag nil))

(defun turn-on-pandoc ()
  "Unconditionally turn on pandoc-mode."
  (interactive)
  (pandoc-mode 1))

(defun turn-off-pandoc ()
  "Unconditionally turn off pandoc-mode"
  (interactive)
  (pandoc-mode -1))

(defun conditionally-turn-on-pandoc ()
  "Turn on pandoc-mode if a pandoc settings file exists.
This is for use in major mode hooks."
  (when (file-exists-p (pandoc-create-settings-filename 'settings (buffer-file-name) "default"))
    (turn-on-pandoc)))

(defun pandoc-set (option value)
  "Sets the local value of OPTION to VALUE.
If OPTION is 'variable, VALUE should be a cons of the
form (variable-name . value), which is then added to the
variables already stored, or just (variable-name), in which case
the named variable is deleted from the list."
  (when (assq option pandoc-local-options) ; check if the option is licit
    (let ((new-value
	   (if (eq option 'variable)
	       ;; new variables are added to the list; existing variables are
	       ;; overwritten or deleted.
	       (append (assq-delete-all (car value) (pandoc-get 'variable))
		       (if (cdr value)
			   (list value)
			 nil))
	     ;; all other options simply override the existing value.
	     value)))
      (setcdr (assq option pandoc-local-options) new-value))
    (setq pandoc-settings-modified-flag t)))

(defun pandoc-set* (option value)
  "Sets the project value of OPTION to VALUE.
If OPTION is 'variable, VALUE should be a cons of the
form (variable-name . value), which is then added to the
variables already stored, or just (variable-name), in which case
the named variable is deleted from the list."
  (when (assq option pandoc-project-options) ; check if the option is licit
    (let ((new-value
	   (if (eq option 'variable)
	       ;; new variables are added to the list; existing variables are
	       ;; overwritten or deleted.
	       (append (assq-delete-all (car value) (pandoc-get* 'variable))
		       (if (cdr value)
			   (list value)
			 nil))
	     ;; all other options simply override the existing value.
	     value)))
      (setcdr (assq option pandoc-project-options) new-value))
    (setq pandoc-settings-modified-flag t)))

(defun pandoc-get (option &optional buffer)
  "Returns the local value of OPTION."
  (cdr (assq option (if buffer
			(buffer-local-value 'pandoc-local-options buffer)
		      pandoc-local-options))))

(defun pandoc-get* (option)
  "Returns the project value of OPTION."
  (cdr (assq option pandoc-project-options)))

(defun pandoc-toggle (option)
  "Toggles the local value of an on/off option."
  (pandoc-set option (not (pandoc-get option))))

(defun pandoc-toggle* (option)
  "Toggles the project value of an on/off option."
  (pandoc-set* option (not (pandoc-get* option))))

(defun pandoc-create-settings-filename (type filename output-format)
  "Create a settings filename.
TYPE is the type of settings file, either 'settings or 'project.
FILENAME should be an absolute filename, the return value is an
absolute filename as well."
  (cond
   ((eq type 'settings)
    (concat (file-name-directory filename) "." (file-name-nondirectory filename) "." output-format ".pandoc"))
   ((eq type 'project)
    (concat (file-name-directory filename) "Project." output-format ".pandoc"))))

(defun pandoc-create-command-option-list (input-file &optional pdf)
  "Create a list of strings with pandoc switches for the current buffer.
INPUT-FILE is the name of the input file. If PDF is non-nil, an
output file is always set, derived either from the input file or
from the output file set for the \"latex\" output profile, and
gets the suffix `.pdf'. If the output format is \"odt\" or
\"epub\" but no output file is specified, one will be created,
since pandoc does not support output to stdout for those two
formats."
  (let ((read (format "--read=%s%s" (pandoc-get 'read) (if (pandoc-get 'read-lhs) "+lhs" "")))
	(write (if pdf
		   nil
		 (format "--write=%s%s" (pandoc-get 'write) (if (pandoc-get 'write-lhs) "+lhs" ""))))
	(output (cond
		 ((or (eq (pandoc-get 'output) t)                     ; if the user set the output file to T
		      (and (null (pandoc-get 'output))                ; or if the user set no output file but either
			   (or pdf                                    ; (i) we're running markdown2pdf, or
			       (string= (pandoc-get 'write) "odt")    ; (ii) the output format is odt or epub
			       (string= (pandoc-get 'write) "epub"))))
		  (format "--output=%s/%s%s"                          ; we create an output file name.
			  (expand-file-name (or (pandoc-get 'output-dir)
						(file-name-directory input-file)))
			  (file-name-sans-extension (file-name-nondirectory input-file))
			  (if pdf
			      ".pdf"
			    (cdr (assoc (pandoc-get 'write) pandoc-output-formats)))))
		 ((stringp (pandoc-get 'output))                      ; if the user set an output file,
		  (format "--output=%s/%s"                            ; we combine it with the output directory
			  (expand-file-name (or (pandoc-get 'output-dir)
						(file-name-directory input-file)))
			  (if pdf                                     ; and check if we're running markdown2pdf
			      (concat (file-name-sans-extension (pandoc-get 'output)) ".pdf")
			    (pandoc-get 'output))))
		 (t nil)))
	(variables (mapcar #'(lambda (variable)
			       (format "--variable=%s:%s" (car variable) (cdr variable)))
			   (pandoc-get 'variable)))
	(other-options (mapcar #'(lambda (switch)
				   (let ((value (pandoc-get switch)))
				     (when (and value (memq switch pandoc-filepath-switches))
				       (setq value (expand-file-name value)))
				     (cond
				      ((eq value t) (format "--%s" switch))
				      ((stringp value) (format "--%s=%s" switch value))
				      (t nil))))
			       (if pdf
				   pandoc-markdown2pdf-switches
				 pandoc-switches))))
    (delq nil (append (list read write output) variables other-options))))

(defun pandoc-process-directives (output-format)
  "Processes pandoc-mode @@-directives in the current buffer.
OUTPUT-FORMAT is passed unchanged to the functions associated
with the @@-directives."
  (interactive (list (pandoc-get 'write)))
  (mapc #'funcall pandoc-directives-hook)
  (let ((case-fold-search nil))
    (mapc #'(lambda (directive)
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

(defun pandoc-process-lisp-directive (output-format lisp)
  "Process @@lisp directives."
  (format "%s" (eval (car (read-from-string lisp)))))

(defun pandoc-process-include-directive (output-format include-file)
  "Process @@include directives."
  (with-temp-buffer
    (insert-file-contents include-file)
    (buffer-string)))

(defun pandoc-call-external (buffer output-format &optional pdf)
  "Call pandoc on the current document.
This function creates a temporary buffer and sets up the required
local options. BUFFER is the buffer whose contents must be sent
to pandoc. Its contents is copied into the temporary buffer, the
@@-directives are processed, after which pandoc is called.

OUTPUT-FORMAT is the format to use. If nil, BUFFER's output
format is used.

If PDF is non-nil, markdown2pdf is called instead of pandoc."
  (let ((filename (buffer-file-name buffer))
	(command (if pdf pandoc-markdown2pdf-script pandoc-binary)))
    (with-temp-buffer ; we do this in a temp buffer so we can process
		      ; @@-directives without having to undo them and set the
		      ; options independently of the original buffer.
      (if (and output-format ; if an output format was provided (and the buffer is visiting a file)
	       filename)     ; we want to use settings for that format or no settings at all.
	  (unless (pandoc-load-settings-for-file (expand-file-name filename) output-format t)
	    ;; if we do not find a settings file, we unset all options:
	    (setq pandoc-local-options (copy-alist pandoc-options)
		  pandoc-project-options (copy-alist pandoc-options))
	    ;; and specify only the input and output formats:
	    (pandoc-set 'write output-format)
	    (pandoc-set 'read (pandoc-get 'read buffer)))
	;; if no output format was provided, we use the buffer's options:
	(setq pandoc-local-options (buffer-local-value 'pandoc-local-options buffer))
	(setq pandoc-project-options (buffer-local-value 'pandoc-project-options buffer)))
      (let ((option-list (pandoc-create-command-option-list filename pdf)))
	(insert-buffer-substring-no-properties buffer)
	(message "Running %s..." (file-name-nondirectory command))
	(pandoc-process-directives (pandoc-get 'write))
	(with-current-buffer pandoc-output-buffer
	  (erase-buffer)
	  (insert (format "Running `%s %s'\n\n" (file-name-nondirectory command) (mapconcat #'identity option-list " "))))
	(if (= 0 (apply 'call-process-region (point-min) (point-max) command nil pandoc-output-buffer t option-list))
	    (message "Running %s... Finished." (file-name-nondirectory command))
	  (message "Error in %s process." (file-name-nondirectory command))
	  (display-buffer pandoc-output-buffer))))))

(defun pandoc-run-pandoc (prefix)
  "Run pandoc on the current document.
If called with a prefix argument, the user is asked for an output
format. Otherwise, the output format currently set in the buffer
is used."
  (interactive "P")
  (pandoc-call-external (current-buffer)
			(if prefix
			    (completing-read "Output format to use: " pandoc-output-formats nil t)
			  nil)))

(defun pandoc-run-markdown2pdf (prefix)
  "Run markdown2pdf on the current document.
If the output format of the current buffer is set to \"latex\",
the buffer's options are used. If called with a prefix argument,
or if the current buffer's output format is not \"latex\", a
LaTeX settings file is searched for and loaded when found. If no
such settings file is found, all options are unset except for the
input and output formats."
  (interactive "P")
  (pandoc-call-external (current-buffer)
			(if (or prefix
				(not (string= (pandoc-get 'write) "latex")))
			    "latex"
			  nil)
			t))

(defun pandoc-set-default-format ()
  "Sets the current output format as default.
This is done by creating a symbolic link to the relevant settings
files. (Therefore, this function is not available on Windows.)"
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "This option is not available on MS Windows")
    (let ((current-settings-file (file-name-nondirectory (pandoc-create-settings-filename 'settings (buffer-file-name) (pandoc-get 'write))))
	  (current-project-file (file-name-nondirectory (pandoc-create-settings-filename 'project (buffer-file-name) (pandoc-get 'write)))))
      (when (not (file-exists-p current-settings-file))
	(pandoc-save-settings 'settings (pandoc-get 'write)))
      (make-symbolic-link current-settings-file (pandoc-create-settings-filename 'settings (buffer-file-name) "default") t)
      (when (file-exists-p current-project-file)
	  (make-symbolic-link current-project-file (pandoc-create-settings-filename 'project (buffer-file-name) "default") t))
      (message "`%s' set as default output format." (pandoc-get 'write)))))

(defun pandoc-save-settings-file ()
  "Save the settings of the current buffer.
This function just calls pandoc-save-settings with the
appropriate output format."
  (interactive)
  (pandoc-save-settings 'settings (pandoc-get 'write)))

(defun pandoc-save-project-file ()
  "Save the current settings as a project file.
In order to achieve this, the current local settings are copied
to the project settings."
  (interactive)
  (setq pandoc-project-options (copy-alist pandoc-local-options))
  (pandoc-save-settings 'project (pandoc-get 'write)))

(defun pandoc-save-settings (type format &optional no-confirm)
  "Save the settings of the current buffer for FORMAT.
TYPE must be a quoted symbol and specifies the type of settings
file. If its value is 'settings, a normal settings file is
created for the current file. If TYPE's value is 'project, a
project settings file is written. If optional argument NO-CONFIRM
is non-nil, any existing settings file is overwritten without
asking."
  (let ((settings-file (pandoc-create-settings-filename type (buffer-file-name) format))
	(filename (buffer-file-name))
	;; If TYPE is 'settings, we only need the options in
	;; pandoc-local-options that differ from pandoc-project-options. Note
	;; that we convert all values to strings, so that options that are nil
	;; in pandoc-local-options but non-nil in pandoc-project-options are
	;; also saved below.
	(options (cond ((eq type 'settings) (delq nil (mapcar #'(lambda (option)
								  (when (not (equal (pandoc-get option)
										    (pandoc-get* option)))
								    (if (eq option 'variable)
									(cons option (or (pandoc-get option)
											 "nil"))
								      (cons option (format "%s" (pandoc-get option))))))
							      (mapcar #'car pandoc-options))))
		       ((eq type 'project) pandoc-project-options))))
    (if (and (not no-confirm)
	     (file-exists-p settings-file)
	     (not (y-or-n-p (format "%s file `%s' already exists. Overwrite? "
				    (capitalize (symbol-name type))
				    (file-name-nondirectory settings-file)))))
	(message "%s file not written." (capitalize (symbol-name type)))
      (with-temp-buffer
	(insert (format "# pandoc-mode %s file for %s #\n"
			type
			(file-name-nondirectory filename))
		(format "# saved on %s #\n\n" (format-time-string "%Y.%m.%d %H:%M")))
	(pandoc-insert-options options)
	(let ((make-backup-files nil))
	  (write-region (point-min) (point-max) settings-file))
	(message "%s file written to `%s'." (capitalize (symbol-name type)) (file-name-nondirectory settings-file)))
      (setq pandoc-settings-modified-flag nil))))

(defun pandoc-insert-options (options)
  "Insert OPTIONS in the current buffer.
Options are written out in the format <option>::<value>."
  (mapc #'(lambda (option)
	    (when (cdr option)
	      (cond
	       ((eq (car option) 'variable)
		(mapc #'(lambda (variable)
			  (insert (format "variable::%s:%s\n" (car variable) (cdr variable))))
		      (cdr option)))
	       (t (insert (format "%s::%s\n" (car option) (cdr option)))))))
	options))

(defun pandoc-undo-file-settings ()
  "Undo all settings specific to the current file.
Project settings associated with the current file are kept."
  (interactive)
  (setq pandoc-local-options (copy-alist pandoc-project-options))
  (message "Local file settings undone for current session. Save local settings to make persistent."))

(defun pandoc-load-default-settings ()
  "Load the default settings of the file in the current buffer.
This function is for use in pandoc-mode-hook."
  (pandoc-load-settings-profile "default"))

(defun pandoc-load-settings-profile (format &optional no-confirm)
  "Load the options for FORMAT from the corresponding settings file.
If NO-CONFIRM is t, no confirmation is asked if the current
settings have not been saved."
  (when (buffer-file-name)
    (pandoc-load-settings-for-file (expand-file-name (buffer-file-name)) format no-confirm)))

(defun pandoc-load-settings-for-file (file format &optional no-confirm)
  "Load the options for FILE.
Both the file's own settings file and the directory's project
file are read, if they exist. If NO-CONFIRM is t, no confirmation
is asked if the current settings have not been saved. FILE must
be an absolute path name. Returns NIL if no settings or project
file is found for FILE, otherwise non-NIL."
  (when (and (not no-confirm)
	     pandoc-settings-modified-flag
	     (y-or-n-p (format "Current settings for format \"%s\" modified. Save first? " (pandoc-get 'write))))
    (pandoc-save-settings (pandoc-get 'write) t))
  ;; We read the options from the settings files but do not store them in the
  ;; pandoc-{local|project}-options variables. Rather, we set them one by one
  ;; with pandoc-set(*), so we can make sure per-file settings override the
  ;; project settings.
  (let ((project-settings (pandoc-read-settings-from-file (pandoc-create-settings-filename 'project file format)))
	(local-settings (pandoc-read-settings-from-file (pandoc-create-settings-filename 'settings file format))))
    (unless (nor project-settings local-settings)
      (setq pandoc-project-options (copy-alist pandoc-options)
	    pandoc-local-options (copy-alist pandoc-options))
      (mapc #'(lambda (option)
		(pandoc-set (car option) (cdr option))
		(pandoc-set* (car option) (cdr option)))
	    project-settings)
      ;; the local settings are processed second, so that they can override the
      ;; project settings.
      (mapc #'(lambda (option)
		(pandoc-set (car option) (cdr option)))
	    local-settings)
      (setq pandoc-settings-modified-flag nil)
      (message "Settings loaded for format \"%s\"." format))))

(defun pandoc-read-settings-from-file (settings-file)
  "Read the options in SETTINGS-FILE.
Returns an alist with the options and their values."
  (when (file-readable-p settings-file)
    (with-temp-buffer
      (insert-file-contents settings-file)
      (goto-char (point-min))
      (let (options) ; alist holding the options we read
	(while (re-search-forward "^\\([a-z-]*\\)::\\(.*?\\)$" nil t)
	  (let ((option (intern (match-string 1)))
		(value (match-string 2)))
	    ;; If the option is a variable, we read its name and value and add
	    ;; them to the alist as a dotted list. note that there may be more
	    ;; than one variable-value pair in OPTIONS.
	    (add-to-list 'options (if (eq option 'variable)
				      (progn
					(string-match "^\\(.*?\\):\\(.*?\\)$" value)
					(cons 'variable (cons (intern (match-string 1 value)) (match-string 2 value))))
				    (cons option (cond
						  ((string-match "^[0-9]$" value) (string-to-number value))
						  ((string= "t" value) t)
						  ((string= "nil" value) nil)
						  (t value)))))))
	options))))

(defun pandoc-view-output ()
  "Displays the *Pandoc output* buffer."
  (interactive)
  (display-buffer pandoc-output-buffer))

(defun pandoc-view-settings ()
  "Displays the settings file in the *Pandoc output* buffer."
  (interactive)
  (let ((options pandoc-local-options))
    (set-buffer pandoc-output-buffer)
    (erase-buffer)
    (pandoc-insert-options options))
  (display-buffer pandoc-output-buffer))

(defun pandoc-insert-@ ()
  "Insert a new labeled (@) list marker at point."
  (interactive)
  (let ((label (pandoc-@-counter-inc)))
    (insert (format "(@%s)" label))))	

(defun pandoc-collect-@-definitions ()
  "Collect (@)-definitions and return them as a list."
  (save-excursion
    (goto-char (point-min))
    (let (definitions)
    (while (re-search-forward "^[[:space:]]*\\((@.*?).*\\)$" nil t)
      (add-to-list 'definitions (match-string-no-properties 1) t))
    definitions)))

(defun pandoc-select-@ ()
  "Show a list of (@)-definitions and allow the user to choose one."
  (interactive)
  (let ((definitions (pandoc-collect-@-definitions)))
    (setq pandoc-window-config (current-window-configuration))
    (setq pandoc-pre-select-buffer (current-buffer))
    (setq pandoc-@-buffer (get-buffer-create " *Pandoc select*"))
    (set-buffer pandoc-@-buffer)
    (pandoc-@-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (mapc #'(lambda (definition)
		(insert (concat " " definition "\n\n")))
	    definitions)
      (goto-char (point-min))
      (setq pandoc-@-overlay (make-overlay (point-min) (point-at-eol)))
      (overlay-put pandoc-@-overlay 'face 'highlight))
    (select-window (display-buffer pandoc-@-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to set specific options. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pandoc-set-write (format)
  "Sets the output format to FORMAT.
If a settings and/or project file exists for FORMAT, they are
loaded. If none exists, all options are unset (except the input
format)."
  (interactive (list (completing-read "Set output format to: " pandoc-output-formats nil t)))
  (when (and pandoc-settings-modified-flag
	     (y-or-n-p (format "Current settings for output format \"%s\" changed. Save? " (pandoc-get 'write))))
    (pandoc-save-settings (pandoc-get 'write) t))
  (unless (pandoc-load-settings-profile format t)
    (setq pandoc-local-options (copy-alist pandoc-options))
    (pandoc-set 'write format)
    (pandoc-set 'read (cdr (assq major-mode pandoc-major-modes)))))

(defun pandoc-set-output (prefix)
  "Set the output file.
If called with the prefix argument C-u - (or M--), the output
file is unset. If called with any other prefix argument, the
output file is created on the basis of the input file and the
output format."
  (interactive "P")
  (pandoc-set 'output
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (file-name-nondirectory (read-file-name "Output file: ")))
	       (t t))))

(defun pandoc-set-epub-stylesheet (prefix)
  "Set the EPUB style sheet.
If called with the prefix argument C-u - (or M--), the EPUB style
sheet is unset. If called with any other prefix argument,
Pandoc looks for the file epub.css in the user data directory."
  (interactive "P")
  (pandoc-set 'epub-stylesheet
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-file-name "EPUB style sheet: "))
	       (t t))))

(defun pandoc-set-data-dir (prefix)
  "Set the option `Data Directory'.
If called with the prefix argument C-u - (or M--), the data
directory is set to NIL, which means use $HOME/.pandoc."
  (interactive "P")
  (pandoc-set 'data-dir
	      (if (eq prefix '-)
		  nil
		(read-directory-name "Data directory: " nil nil t))))

(defun pandoc-set-output-dir (prefix)
  "Set the option `Output Directory'.
If called with the prefix argument C-u - (or M--), the output
directory is set to NIL, which means use the directory of the
input file."
  (interactive "P")
  (pandoc-set 'output-dir
	      (if (eq prefix '-)
		  nil
		(read-directory-name "Output directory: " nil nil t))))

(defun pandoc-set-css (prefix)
  "Set the CSS style sheet.
If called with the prefix argument C-u - (or M--), the CSS style
sheet file is unset."
  (interactive "P")
  (pandoc-set 'css
	      (if (eq prefix '-)
		  nil
		(file-name-nondirectory (read-file-name "Select CSS style sheet: ")))))

(defun pandoc-set-include-in-header (prefix)
  "Set the file to be included in the header.
If called with the prefix argument C-u - (or M--), the include
header file is unset."
  (interactive "P")
  (pandoc-set 'include-in-header
	      (if (eq prefix '-)
		  nil
		(read-file-name "File to include in the header: "))))

(defun pandoc-set-include-before-body (prefix)
  "Set the file to be included before the body.
If called with the prefix argument C-u - (or M--), the include
before body file is unset."
  (interactive "P")
  (pandoc-set 'include-before-body
	      (if (eq prefix '-)
		  nil
		(read-file-name "File to include before the body: "))))

(defun pandoc-set-include-after-body (prefix)
  "Set the file to be included after the body.
If called with the prefix argument C-u - (or M--), the include
after body file is unset."
  (interactive "P")
  (pandoc-set 'include-after-body
	      (if (eq prefix '-)
		  nil
		(read-file-name "File to include after the body: "))))

(defun pandoc-set-custom-header (prefix)
  "Select the custom header file.
If called with the prefix argument C-u - (or M--), the custom
header file is unset."
  (interactive "P")
  (pandoc-set 'custom-header
	      (if (eq prefix '-)
		  nil
		(read-file-name "Select the custom header file: "))))

(defun pandoc-set-template (prefix)
  "Set the template file.
If called with the prefix argument C-u - (or M--), the template
file is unset."
  (interactive "P")
  (pandoc-set 'template
	      (if (eq prefix '-)
		  nil
		(read-file-name "Template file: "))))

(defun pandoc-set-reference-odt (prefix)
  "Set the reference ODT file.
If called with the prefix argument C-u - (or M--), the reference
ODT file is unset."
  (interactive "P")
  (pandoc-set 'reference-odt
	      (if (eq prefix '-)
		  nil
		(read-file-name "Reference ODT file: "))))

(defun pandoc-set-epub-metadata (prefix)
  "Set the EPUB metadata file.
If called with the prefix argument C-u - (or M--), the EPUB
metadata file is unset."
  (interactive "P")
  (pandoc-set 'epub-metadata
	      (if (eq prefix '-)
		  nil
		(read-file-name "EPUB metadata file: "))))

(defun pandoc-set-epub-cover-image (prefix)
  "Set the EPUB cover image file.
If called with the prefix argument C-u - (or M--), the EPUB
cover image file is unset."
  (interactive "P")
  (pandoc-set 'epub-cover-image
	      (if (eq prefix '-)
		  nil
		(read-file-name "EPUB cover image: "))))

(defun pandoc-set-bibliography (prefix)
  "Set the bibliography file.
If called with the prefix argument C-u - (or M--), the
bibliography file is unset."
  (interactive "P")
  (pandoc-set 'bibliography
	      (if (eq prefix '-)
		  nil
		(read-file-name "Bibliography file: "))))

(defun pandoc-set-csl (prefix)
  "Set the CSL file for the bibliography.
If called with the prefix argument C-u - (or M--), the CSL file
is unset."
  (interactive "P")
  (pandoc-set 'csl
	      (if (eq prefix '-)
		  nil
		(read-file-name "CSL file: "))))

(defun pandoc-set-columns (prefix)
  "Set the column width.
If called with the prefix argument C-u - (or M--), the column
width is unset."
  (interactive "P")
  (pandoc-set 'columns
	      (if (eq prefix '-)
		  nil
		(string-to-number (read-string "Column width: ")))))

(defun pandoc-set-tab-stop (prefix)
  "Set the tab stop size.
If called with the prefix argument C-u - (or M--), the tab stop
width is unset (so that its default value of 4 is used by pandoc)."
  (interactive "P")
  (pandoc-set 'tab-stop
	      (if (eq prefix '-)
		  nil
		(string-to-number (read-string "Tab stop: ")))))

(defun pandoc-set-base-header-level (prefix)
  "Set the base header level.
If called with the prefix argument C-u - (or M--), the base
header level is unset (so that its default value of 1 is used by
pandoc)."
  (interactive "P")
  (pandoc-set 'base-header-level
	      (if (eq prefix '-)
		  nil
		(string-to-number (read-string "Base header level: ")))))

(defun pandoc-set-latexmathml (prefix)
  "Use LaTeXMathML to display embedded TeX in HTML output.
If called with the prefix argument C-u - (or M--), LaTeXMathML is
not used. If called with any other prefix argument, the
LaTeXMathML script will be added to the HTML header."
  (interactive "P")
  (pandoc-set 'latexmathml
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-string "LaTeXMathML URL: "))
	       (t t))))

(defun pandoc-set-mathml (prefix)
  "Use MathML to display embedded TeX in HTML output.
If called with the prefix argument C-u - (or M--), MathML is not
used. If called with any other prefix argument, the MathML script
will be added to the HTML header."
  (interactive "P")
  (pandoc-set 'mathml
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-string "MathML URL: "))
	       (t t))))

(defun pandoc-set-mimetex (prefix)
  "Render TeX math using the MimeTeX CGI script.
If called with the prefix argument C-u - (or M--), MimeTeX is not
used. If called with any other prefix argument, the MimeTeX CGI
script will assumed to be in /cgi-bin/mimetex.cgi."
  (interactive "P")
  (pandoc-set 'mimetex
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-string "MimeTeX CGI script: "))
	       (t t))))

(defun pandoc-set-webtex (prefix)
  "Render TeX math using an external script.
If called with the prefix argument C-u - (or M--), WebTeX is not
used. If called with any other prefix argument, the Google Chart
API will be used."
  (interactive "P")
  (pandoc-set 'webtex
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-string "WebTeX URL: "))
	       (t t))))

(defun pandoc-set-jsmath (prefix)
  "Use jsMath to render TeX math in HTML.
If called with the prefix argument C-u - (or M--), jsMath is not
used. If called with any other prefix argument, the user must
provide a URL in the HTML template."
  (interactive "P")
  (pandoc-set 'webtex
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-string "jsMath URL: "))
	       (t t))))

(defun pandoc-set-mathjax (prefix)
  "Use MathJax to display embedded TeX math in HTML.
If called with the prefix argument C-u - (or M--), MathJax is not
used."
  (interactive "P")
  (pandoc-set 'mathjax
	      (if (eq prefix '-)
		  nil
		(read-string "MathJax URL: "))))

(defun pandoc-set-title-prefix (prefix)
  "Set title prefix.
If called with the prefix argument C-u - (or M--), the title
prefix is unset."
  (interactive "P")
  (pandoc-set 'title-prefix
	      (if (eq prefix '-)
		  nil
		(read-string "Title prefix: "))))

(defun pandoc-set-id-prefix (prefix)
  "Set the id prefix.
If called with the prefix argument C-u - (or M--), the id
prefix is unset."
  (interactive "P")
  (pandoc-set 'id-prefix
	      (if (eq prefix '-)
		  nil
		(read-string "ID prefix: "))))

(defun pandoc-set-indented-code-classes (prefix)
  "Set the option `Indented Code Classes'.
If called with the prefix argument C-u - (or M--), the indented
code classes option is unset."
  (interactive "P")
  (pandoc-set 'indented-code-classes
	      (if (eq prefix '-)
		  nil
		(read-string "Indented Code Classes: "))))

(defun pandoc-set-template-variable (prefix)
  "Add/change/remove a template variable.
The user is asked for a variable name. If the function is called
with a prefix value, this variable is removed from the list of
variables, otherwise the user is asked for a value."
  (interactive "P")
  (let ((var (nonempty (completing-read "Variable name: " (pandoc-get 'variable)))))
    (when var
      (setq var (intern var))
      (let ((value (if (eq prefix '-)
		       nil
		     (read-string "Value: " nil nil (cdr (assq var (pandoc-get 'variable)))))))
	(pandoc-set 'variable (cons var value))
	(message "Template variable %s %s." var (if value
						    (format "added with value `%s'" value)
						  "removed"))))))

(defun pandoc-set-email-obfuscation (prefix)
  "Set the option `Email Obfuscation'.
If called with prefix argument C-u - (or M--), Email Obfuscation
is unset."
  (interactive "P")
  (pandoc-set 'email-obfuscation
	      (if (eq prefix '-)
		  "none"
		(let ((value (completing-read "Set email obfuscation: " '("none" "javascript" "references") nil t)))
		  (if (member value '("" "none"))
		      "none"
		    value))))
  (message "Email obfuscation: %s." (or (pandoc-get 'email-obfuscation)
					"unset")))

(defun pandoc-toggle-interactive (prefix)
  "Toggle one of pandoc's binary options.
If called with the prefix argument C-u - (or M--), the options is
unset. If called with any other prefix argument, the option is
set. Without any prefix argument, the option is toggled."
  (interactive "P")
  (let ((completion-ignore-case t))
    (let ((option (cdr (assoc (completing-read (format "%s option: " (cond
								      ((eq prefix '-) "Unset")
								      ((null prefix) "Toggle")
								      (t "Set")))
					       pandoc-binary-switches nil t) pandoc-binary-switches))))
      (pandoc-set option (cond
			  ((eq prefix '-) nil)
			  ((null prefix) (not (pandoc-get option)))
			  (t t)))
      (message "Option `%s' %s." (car (rassq option pandoc-binary-switches)) (if (pandoc-get option)
										 "set"
									       "unset")))))

(easy-menu-define pandoc-mode-menu pandoc-mode-map "Pandoc menu"
  `("Pandoc"
    ["Run Pandoc" pandoc-run-pandoc :active t]
    ["Create PDF" pandoc-run-markdown2pdf
     :active (string= (pandoc-get 'read) "markdown")]
    ["View Output Buffer" pandoc-view-output :active t]
    ["Save File Settings" pandoc-save-settings-file :active t]
    ["Set As Default Format" pandoc-set-default-format :active t]
    ("Project"
     ["Save Project File" pandoc-save-project-file :active t]
     ["Undo File Settings" pandoc-undo-file-settings :active t])
    ("Example Lists"
     ["Insert New Example" pandoc-insert-@ :active t]
     ["Select And Insert Example Label" pandoc-select-@ :active t])
    "--"
    ["View Current Settings" pandoc-view-settings :active t]
    ,(append (cons "Input Format"
		   (mapcar #'(lambda (option)
			       (vector (car option)
				       `(pandoc-set 'read ,(cdr option))
				       :active t
				       :style 'radio
				       :selected `(string= (pandoc-get 'read)
							   ,(cdr option))))
			   '(("Native Haskell" . "native")
			     ("Markdown" . "markdown")
			     ("reStructuredText" . "rst")
			     ("HTML" . "html")
			     ("LaTeX" . "latex")
			     ("Textile" . "textile")
			     ("JSON" . "json"))))
	     (list ["Literal Haskell" (pandoc-toggle 'read-lhs)
		    :active (member (pandoc-get 'read) '("markdown" "rst" "latex"))
		    :style toggle :selected (pandoc-get 'read-lhs)]))

    ,(append (cons "Output Format"
		   (mapcar #'(lambda (option)
			       (vector (car option)
				       `(pandoc-set-write ,(cdr option))
				       :active t
				       :style 'radio
				       :selected `(string= (pandoc-get 'write)
							   ,(cdr option))))
			   '(("Native Haskell" . "native")
			     ("Markdown" . "markdown")
			     ("reStructuredText" . "rst")
			     ("HTML" . "html")
			     ("LaTeX" . "latex")
			     ("ConTeXt" . "context")
			     ("Man Page" . "man")
			     ("MediaWiki" . "mediawiki")
			     ("TeXinfo" . "texinfo")
			     ("DocBook XML" . "docbook")
			     ("EPUB E-Book" . "epub")
			     ("OpenDocument XML" . "opendocument")
			     ("OpenOffice Text Document" . "odt")
			     ("S5 HTML/JS Slide Show" . "s5")
			     ("Slidy Slide Show" . "slidy")
			     ("Rich Text Format" . "rtf")
			     ("Textile" . "textile")
			     ("Org-mode" . "org")
			     ("JSON" . "json"))))
	     (list ["Literal Haskell" (pandoc-toggle 'write-lhs)
		    :active (member (pandoc-get 'write)
				    '("markdown" "rst" "latex" "html"))
		    :style toggle :selected (pandoc-get 'write-lhs)]))

    ("Files"
     ("Output File"
      ["Output To Stdout" (pandoc-set 'output nil) :active t
       :style radio :selected (null (pandoc-get 'output))]
      ["Create Output Filename" (pandoc-set 'output t) :active t
       :style radio :selected (eq (pandoc-get 'output) t)]
      ["Set Output File..." pandoc-set-output :active t
      :style radio :selected (stringp (pandoc-get 'output))])
     ("Output Directory"
      ["Use Input Directory" (pandoc-set 'output-dir nil) :actine t
       :style radio :selected (null (pandoc-get 'output-dir))]
      ["Set Output Directory" pandoc-set-output-dir :active t
       :style radio :selected (pandoc-get 'output-dir)])
     ("Data Directory"
      ["Use Default Data Directory" (pandoc-set 'data-dir nil) :actine t
       :style radio :selected (null (pandoc-get 'data-dir))]
      ["Set Data Directory" pandoc-set-data-dir :active t
       :style radio :selected (pandoc-get 'data-dir)])
     ("Template File"
      ["No Template File" (pandoc-set 'template nil) :active t
       :style radio :selected (null (pandoc-get 'template))]
      ["Set Template File..." pandoc-set-template :active t
      :style radio :selected (pandoc-get 'template)])
     ("Reference ODT File"
      ["No Reference ODT File" (pandoc-set 'reference-odt nil) :active t
       :style radio :selected (null (pandoc-get 'reference-odt))]
      ["Set Reference ODT File..." pandoc-set-reference-odt :active t
      :style radio :selected (pandoc-get 'reference-odt)])
     ("CSS Style Sheet"
      ["No CSS Style Sheet" (pandoc-set 'css nil) :active t
       :style radio :selected (null (pandoc-get 'css))]
      ["Set CSS Style Sheet..." pandoc-set-css :active t
      :style radio :selected (pandoc-get 'css)])
     ("EPUB Style Sheet"
      ["No EPUB Style Sheet" (pandoc-set 'epub-stylesheet nil) :active t
       :style radio :selected (null (pandoc-get 'epub-stylesheet))]
      ["Include standard EPUB Style Sheet" (pandoc-set 'epub-stylesheet t) :active t
       :style radio :selected (eq (pandoc-get 'epub-stylesheet) t)]
      ["Set EPUB Style Sheet..." pandoc-set-epub-stylesheet :active t
      :style radio :selected (stringp (pandoc-get 'epub-stylesheet))])
     ("EPUB Cover Image"
      ["No EPUB Cover Image" (pandoc-set 'epub-cover-image nil) :active t
       :style radio :selected (null (pandoc-get 'epub-cover-image))]
      ["Set EPUB Cover Image..." pandoc-set-epub-cover-image :active t
      :style radio :selected (pandoc-get 'epub-cover-image)])
     ("EPUB Metadata File"
      ["No EPUB Metadata" (pandoc-set 'epub-metadata nil) :active t
       :style radio :selected (null (pandoc-get 'epub-metadata))]
      ["Set EPUB Metadata File..." pandoc-set-epub-metadata :active t
      :style radio :selected (pandoc-get 'epub-metadata)])
     ("Bibliography File"
      ["No Bibliography" (pandoc-set 'bibliography nil) :active t
       :style radio :selected (null (pandoc-get 'bibliography))]
      ["Set Bibliography File..." pandoc-set-bibliography :active t
      :style radio :selected (pandoc-get 'bibliography)])
     ("CSL File"
      ["No CSL" (pandoc-set 'csl nil) :active t
       :style radio :selected (null (pandoc-get 'csl))]
      ["Set CSL File..." pandoc-set-csl :active t
      :style radio :selected (pandoc-get 'csl)])
     ("Include In Header"
      ["Nothing Included In Header" (pandoc-set 'include-in-header nil) :active t
       :style radio :selected (null (pandoc-get 'include-in-header))]
      ["Include In Header..." pandoc-set-include-in-header :active t
      :style radio :selected (pandoc-get 'include-in-header)])
     ("Include Before Body"
      ["Nothing Included Before Body" (pandoc-set 'include-before-body nil) :active t
       :style radio :selected (null (pandoc-get 'include-before-body))]
      ["Include Before Body..." pandoc-set-include-before-body :active t
      :style radio :selected (pandoc-get 'include-before-body)])
     ("Include After Body"
      ["Nothing Included After Body" (pandoc-set 'include-after-body nil) :active t
       :style radio :selected (null (pandoc-get 'include-after-body))]
      ["Include After Body..." pandoc-set-include-after-body :active t
      :style radio :selected (pandoc-get 'include-after-body)])
     ("Custom Header"
      ["No Custom Header" (pandoc-set 'custom-header nil) :active t
       :style radio :selected (null (pandoc-get 'custom-header))]
      ["Set Custom Header File..." pandoc-set-custom-header :active t
      :style radio :selected (pandoc-get 'custom-header)]))

    ("Options"
     ("Title Prefix"
      ["No Title Prefix" (pandoc-set 'title-prefix nil) :active t
       :style radio :selected (null (pandoc-get 'title-prefix))]
      ["Set Title Prefix..." pandoc-set-title-prefix :active t
      :style radio :selected (pandoc-get 'title-prefix)])
     ("ID Prefix"
      ["No ID Prefix" (pandoc-set 'id-prefix nil) :active t
       :style radio :selected (null (pandoc-get 'id-prefix))]
      ["Set ID Prefix..." pandoc-set-id-prefix :active t
      :style radio :selected (pandoc-get 'id-prefix)])
     ("Indented Code Classes"
      ["No Indented Code Classes" (pandoc-set 'indented-code-classes nil) :active t
       :style radio :selected (null (pandoc-get 'indented-code-classes))]
      ["Set Indented Code Classes..." pandoc-set-indented-code-classes :active t
      :style radio :selected (pandoc-get 'indented-code-classes)])
     ("Tab Stops"
      ["Default Tab Stops" (pandoc-set 'tab-stop nil) :active t
       :style radio :selected (null (pandoc-get 'tab-stop))]
      ["Set Tab Stop Width..." pandoc-set-tab-stop :active t
      :style radio :selected (pandoc-get 'tab-stop)])
     ("Line Width"
      ["Default Line Width" (pandoc-set 'columns nil) :active t
       :style radio :selected (null (pandoc-get 'columns))]
      ["Set Line Width..." pandoc-set-columns :active t
      :style radio :selected (pandoc-get 'columns)])
     ("Base Header Level"
      ["Default Base Header Level" (pandoc-set 'base-header-level nil) :active t
       :style radio :selected (null (pandoc-get 'base-header-level))]
      ["Set Base Header Level..." pandoc-set-base-header-level :active t
      :style radio :selected (pandoc-get 'base-header-level)])
     ("LaTeXMathML"
      ["No LaTeXMathML" (pandoc-set 'latexmathml nil) :active t
       :style radio :selected (null (pandoc-get 'latexmathml))]
      ["Add Script To HTML Header" (pandoc-set 'latexmathml t) :active t
       :style radio :selected (eq (pandoc-get 'latexmathml) t)]
      ["Set LaTeXMathML URL..." pandoc-set-latexmathml :active t
      :style radio :selected (stringp (pandoc-get 'latexmathml))])
     ("MathML"
      ["No MathML" (pandoc-set 'mathml nil) :active t
       :style radio :selected (null (pandoc-get 'mathml))]
      ["Add Script To HTML Header" (pandoc-set 'mathml t) :active t
       :style radio :selected (eq (pandoc-get 'mathml) t)]
      ["Set MathML URL..." pandoc-set-mathml :active t
      :style radio :selected (stringp (pandoc-get 'mathml))])
     ("jsMath"
      ["No jsMath" (pandoc-set 'jsmath) :active t
       :style radio :selected (null (pandoc-get 'jsmath))]
      ["Provide Link In HTML Template" (pandoc-set 'jsmath t) :active t
       :style radio :selected (eq (pandoc-get 'jsmath) t)]
      ["Set jsMath URL..." pandoc-set-jsmath :active t
      :style radio :selected (pandoc-get 'jsmath)])
     ("MathJax"
      ["No MathJax" (pandoc-set 'mathjax) :active t
       :style radio :selected (null (pandoc-get 'mathjax))]
      ["Set MathJax URL..." pandoc-set-mathjax :active t
      :style radio :selected (pandoc-get 'mathjax)])
     ("MimeTeX"
      ["No MimeTeX" (pandoc-set 'mimetex nil) :active t
       :style radio :selected (null (pandoc-get 'mimetex))]
      ["Default Script Location" (pandoc-set 'mimetex t) :active t
       :style radio :selected (eq (pandoc-get 'mimetex) t)]
      ["Set MimeTeX GCI Script..." pandoc-set-mimetex :active t
      :style radio :selected (stringp (pandoc-get 'mimetex))])
     ("WebTeX"
      ["No WebTeX" (pandoc-set 'webtex nil) :active t
       :style radio :selected (null (pandoc-get 'webtex))]
      ["Use Google Chart API" (pandoc-set 'webtex t) :active t
       :style radio :selected (eq (pandoc-get 'webtex) t)]
      ["Set WebTeX URL..." pandoc-set-webtex :active t
      :style radio :selected (stringp (pandoc-get 'webtex))])
     ("Email Obfuscation"
      ["None" (pandoc-set 'email-obfuscation "none") :active t
       :style radio :selected (string= (pandoc-get 'email-obfuscation) "none")]
      ["Javascript" (pandoc-set 'email-obfuscation "javascript") :active t
       :style radio :selected (string= (pandoc-get 'email-obfuscation) "javascript")]
      ["References" (pandoc-set 'email-obfuscation "references") :active t
       :style radio :selected (string= (pandoc-get 'email-obfuscation) "references")])
     ("Template Variables"
      ["Set/Change Template Variable" pandoc-set-template-variable :active t]
      ["Unset Template Variable" (pandoc-set-template-variable '-) :active t])
     ;; put the binary options into the menu
     ,@(mapcar #'(lambda (option)
		   (vector (car option) `(pandoc-toggle (quote ,(cdr option)))
			   :active t
			   :style 'toggle
			   :selected `(pandoc-get (quote ,(cdr option)))))
	       pandoc-binary-switches))))

(easy-menu-add pandoc-mode-menu pandoc-mode-map)

(provide 'pandoc-mode)

;;; pandoc-mode ends here
