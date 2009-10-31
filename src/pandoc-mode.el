;; pandoc-mode.el
;;
;; Copyright (c) 2009 Joost Kremers
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
;;

(require 'easymenu)

(defgroup pandoc nil "Minor mode for interacting with pandoc." :group 'Wp)

(defcustom pandoc-binary "/usr/bin/pandoc"
  "*The full path of the pandoc binary."
  :group 'pandoc
  :type 'file)

(defcustom pandoc-markdown2pdf-script "/usr/bin/markdown2pdf"
  "*The full path of the markdown2pdf script."
  :group 'pandoc
  :type 'file)

(defcustom pandoc-@@directives '(("include" . pandoc-process-include-directive)
				 ("lisp" . pandoc-process-lisp-directive))
  "*List of directives to be processed before pandoc is called.
The directive must be given without angle brackets, the function
is the function to be called, which should take one argument, the
text between <directive>...</directive>, and should return a
string that will replace the directive and its argument.

The directives are processed in the order in which they appear in
this list."
  :group 'pandoc
  :type '(alist :key-type (string :tag "Directive") :value-type function))

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
    "latex")
  "List of pandoc input formats.")

(defvar pandoc-output-formats
  '(("native" . ".hs")
    ("markdown" . ".text")
    ("rst" . ".rst")
    ("html" . ".html")
    ("latex" . ".tex")
    ("context" . ".tex")
    ("man" . "")
    ("mediawiki" . ".mw")
    ("texinfo" . ".texi")
    ("docbook" . ".xml")
    ("opendocument" . ".odf")
    ("odt" . ".odt")
    ("s5" . ".html")
    ("rtf" . ".rtf"))
  "List of pandoc output formats plus file extensions.")

(defvar pandoc-switches
  '(standalone          tab-stop		  
    preserve-tabs       reference-links		  
    strict              smart			  
    parse-raw           jsmath			  
    latexmathml         mimetex			  
    gladtex             number-sections		  
    incremental         sanitize-html		  
    no-wrap             table-of-contents css	  
    email-obfuscation   include-before-body	  
    include-in-header   custom-header title-prefix
    include-after-body)
  "List of switches accepted by the pandoc binary. Switches that
  need special treatment (--read, --write and --output) are not
  in this list.")

(defvar pandoc-binary-switches
  '(("gladTeX" . gladtex)
    ("Incremental" . incremental)
    ("No Wrap" . no-wrap)
    ("Number Sections" . number-sections)
    ("Parse Raw" . parse-raw)
    ("Preserve Tabs" . preserve-tabs)
    ("Reference Links" .  reference-links)
    ("Sanitize HTML" . sanitize-html)
    ("Smart" . smart)
    ("Standalone" . standalone)
    ("Strict" . strict)
    ("Table of Contents" . table-of-contents)))

(defvar pandoc-options
  '((read)                         ; see pandoc-input-formats
    (read-lhs)                     ; input is literal Haskell
    (write . "native")             ; see pandoc-output-formats
    (write-lhs)                    ; output is literal Haskell
    
    (output)                       ; a string
			           ; NIL means stdout (redirected to a temp buffer)
			           ; T means create output filename on the basis of
                                   ; the input file name and the output format.

    (css)                          ; a file or NIL
    (include-in-header)            ; a file or NIL
    (include-before-body)          ; a file or NIL
    (include-after-body)           ; a file or NIL
    (custom-header)                ; a file or NIL

    (tab-stop)                     ; an integer or NIL
    (title-prefix)                 ; a string or NIL
    (latexmathml)                  ; a string or NIL
    (jsmath)                       ; a string or NIL
    (mimetex)                      ; a string, NIL or T

    (email-obfuscation)            ; nil (="none"), "javascript" or "references"

    (gladtex)                      ; NIL, T
    (incremental)                  ; NIL, T
    (no-wrap)                      ; NIL, T
    (number-sections)              ; NIL, T
    (parse-raw)                    ; NIL, T
    (preserve-tabs)                ; NIL, T
    (reference-links)              ; NIL, T
    (sanitize-html)                ; NIL, T
    (smart)                        ; NIL, T
    (standalone)                   ; NIL, T
    (strict)                       ; NIL, T
    (table-of-contents))           ; NIL, T
  "Pandoc option alist.")

(defvar pandoc-local-options nil "A buffer-local variable holding a file's pandoc options.")
(make-variable-buffer-local 'pandoc-local-options)

(defvar pandoc-settings-modified-flag nil "T if the current settings were modified and not saved.")
(make-variable-buffer-local 'pandoc-settings-modified-flag)

(defvar pandoc-output-buffer (get-buffer-create " *Pandoc output*"))

(defvar pandoc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c/r" 'pandoc-run-pandoc)
    (define-key map "\C-c/p" 'pandoc-run-markdown2pdf)
    (define-key map "\C-c/s" 'pandoc-save-settings-interactive)
    (define-key map "\C-c/l" 'pandoc-load-settings-file)
    (define-key map "\C-c/w" 'pandoc-set-write)
    (define-key map "\C-c/v" 'pandoc-view-output)
    (define-key map "\C-c/V" 'pandoc-view-settings)            
    (define-key map "\C-c/oo" 'pandoc-set-output)
    (define-key map "\C-c/oc" 'pandoc-set-css)
    (define-key map "\C-c/oH" 'pandoc-set-include-in-header)
    (define-key map "\C-c/oB" 'pandoc-set-include-before-body)
    (define-key map "\C-c/oA" 'pandoc-set-include-after-body)
    (define-key map "\C-c/oC" 'pandoc-set-custom-header)
    (define-key map "\C-c/oT" 'pandoc-set-title-prefix)
    (define-key map "\C-c/ot" 'pandoc-set-tab-stop)
    (define-key map "\C-c/om" 'pandoc-set-latexmathml)
    (define-key map "\C-c/oj" 'pandoc-set-jsmath)
    (define-key map "\C-c/oM" 'pandoc-set-mimetex)
    (define-key map "\C-c/oe" 'pandoc-set-email-obfuscation)
    (define-key map "\C-c/t" 'pandoc-toggle-interactive)       
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
  (when (file-exists-p (pandoc-create-settings-filename (buffer-file-name) "default"))
    (turn-on-pandoc)))
  
(defun pandoc-set (option value)
  "Sets pandoc OPTION to VALUE."
  (when (assq option pandoc-local-options)
    (setcdr (assq option pandoc-local-options) value)
    (setq pandoc-settings-modified-flag t)))
  
(defun pandoc-get (option)
  "Returns the value of OPTION."
  (cdr (assq option pandoc-local-options)))

(defun pandoc-toggle (option)
  "Toggles an on/off option."
  (pandoc-set option (not (pandoc-get option))))

(defun pandoc-create-command-option-list (options input-file &optional pdf)
  "Create a list of strings with switches that can be passed to pandoc.
INPUT-FILE is the name of the input file. If PDF is non-nil, the
output file is set to the base name of the input file with the
extension `.pdf', regardless of the setting of `output'."
  (let* ((read (format "--read=%s%s" (pandoc-get 'read) (if (pandoc-get 'read-lhs) "+lhs" "")))
	 (write (if pdf nil
		  (format "--write=%s%s" (pandoc-get 'write) (if (pandoc-get 'write-lhs) "+lhs" ""))))
	 (output (if pdf t (pandoc-get 'output)))
    	 (special-options (list read write
				(cond
				 ((eq output t)
				  (format "--output=%s%s"
					  (file-name-sans-extension input-file)
					  (if pdf
					      ".pdf"
					    (cdr (assoc (pandoc-get 'write) pandoc-output-formats)))))
				 ((stringp output)
				  (format "--output=%s" output))
				 (t nil))))
	 (other-options (mapcar #'(lambda (switch)
				    (let ((value (pandoc-get switch)))
				      (cond
				       ((eq value t) (format "--%s" switch))
				       ((stringp value) (format "--%s=%s" switch value))
				       (t nil))))
				pandoc-switches)))
    (delq nil (append special-options other-options))))

(defun pandoc-process-directives ()
  "Processes pandoc-mode @@directives in the current buffer."
  (interactive)
  (mapc #'(lambda (directive)
	    (goto-char (point-min))
	    (while (re-search-forward (concat "\\([\\]?\\)@@" (car directive)) nil t)
	      (if (string= (match-string 1) "\\")
		  (delete-region (match-beginning 1) (match-end 1))
		(let ((beg-open (match-beginning 0))
		      (end-open (match-end 0)))
		  (search-forward (concat (car directive) "@@"))
		  (let* ((beg-close (match-beginning 0))
			 (end-close (match-end 0))
			 (text (buffer-substring-no-properties end-open beg-close)))
		    (goto-char beg-open)
		    (delete-region beg-open end-close)
		    (insert (funcall (cdr directive) text))
		    (goto-char beg-open))))))
	pandoc-@@directives))

(defun pandoc-process-lisp-directive (lisp)
  "Process @@lisp directives."
  (format "%s" (eval (car (read-from-string lisp)))))
  
(defun pandoc-process-include-directive (include-file)
  "Process @@include directives."
  (with-temp-buffer
    (string-match "^[[:space:]]\\(.*\\)[[:space:]]$" include-file)
    (insert-file-contents (match-string 1 include-file))
    (buffer-string)))

(defun pandoc-run-pandoc ()
  "Run pandoc on the current document."
  (interactive)
  (let ((option-list (pandoc-create-command-option-list pandoc-local-options (buffer-file-name)))
	(buffer (current-buffer)))
    (message "Running pandoc for output format %s..." (pandoc-get 'write))
    (with-current-buffer pandoc-output-buffer
      (erase-buffer)
      (insert (format "Running `pandoc %s'\n\n" (mapconcat #'identity option-list " "))))
    (with-temp-buffer  ; we do this in a temp buffer so we can process @@-directives without having to undo them.
      (setq pandoc-local-options (buffer-local-value 'pandoc-local-options buffer))
      (insert-buffer-substring-no-properties buffer)
      (pandoc-process-directives)
      (if (= 0 (apply 'call-process-region (point-min) (point-max) pandoc-binary nil pandoc-output-buffer t option-list))
	  (message "Running pandoc for output format %s... Finished." (pandoc-get 'write))
	(message "Error in pandoc process. Type `C-c / v' to view output.")
	(display-buffer pandoc-output-buffer)))))

(defun pandoc-run-markdown2pdf (prefix)
  "Run markdown2pdf on the current document."
  (interactive "P")
  (let ((latex-settings-file (pandoc-create-settings-filename (buffer-file-name) "latex"))
	(output-format (pandoc-get 'write))
	(buffer (current-buffer))
	(filename (buffer-file-name)))
    (with-temp-buffer  ; we do this in a temp buffer so we can process @@-directives without having to undo them.
      (if (and (not (string= output-format "latex"))
	       (file-exists-p latex-settings-file)
	       (null prefix))
	  (pandoc-load-settings-file latex-settings-file t)
	(setq pandoc-local-options (buffer-local-value 'pandoc-local-options buffer)))
      (let ((option-list (pandoc-create-command-option-list pandoc-local-options filename t)))
	(insert-buffer-substring-no-properties buffer)
	(pandoc-process-directives)
	(message "Running markdown2pdf...")
	(with-current-buffer pandoc-output-buffer
	  (erase-buffer)
	  (insert (format "Running `markdown2pdf %s'\n\n" (mapconcat #'identity option-list " "))))
	(if (= 0 (apply 'call-process-region (point-min) (point-max) pandoc-markdown2pdf-script nil pandoc-output-buffer t option-list))
	    (message "Running markdown2pdf... Finished.")
	  (message "Error in markdown2pdf process. Type `C-c / v' to view output.")
	  (display-buffer pandoc-output-buffer))))))

(defun pandoc-create-settings-filename (filename output-format)
  "Creates the settings filename associated with FILENAME for OUTPUT-FORMAT.
FILENAME should be an absolute filename, the return value is an
absolute filename as well."
   (concat (file-name-directory filename) "." (file-name-nondirectory filename) "." output-format ".pandoc"))

(defun pandoc-set-default-format ()
  "Sets the current output format as default.
This is done by creating a symbolic link to the relevant settings
file. (Therefore, this function is not available on Windows.)"
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "This option is not available on MS Windows")
    (let ((current-settings-file (file-name-nondirectory (pandoc-create-settings-filename (buffer-file-name) (pandoc-get 'write))))
	  (default-settings-file (pandoc-create-settings-filename (buffer-file-name) "default")))
      (when (not (file-exists-p current-settings-file))
	(pandoc-save-settings (pandoc-get 'write)))
      (make-symbolic-link current-settings-file default-settings-file t)
      (message "`%s' set as default output format." (pandoc-get 'write)))))

(defun pandoc-save-settings-interactive ()
  "Save the settings of the current buffer.
This function just calls pandoc-save-settings with the
appropriate output format."
  (interactive)
  (pandoc-save-settings (pandoc-get 'write)))

(defun pandoc-save-settings (format &optional no-confirm)
  "Save the settings of the current buffer.
The settings are saved to a dot file with the same name as the
file whose settings are being saved, appended with the output
format and the suffix `.pandoc'.

If optional argument NO-CONFIRM is non-nil, any existing settings
file is overwritten without asking."
  (let* ((filename (buffer-file-name))
	 (settings-file (pandoc-create-settings-filename filename format)))
    (if (and (not no-confirm)
	     (file-exists-p settings-file)
	     (not (y-or-n-p (format "Settings file `%s' already exists. Overwrite? " (file-name-nondirectory settings-file)))))
	(message "Settings file not written.")
      (let ((local-options pandoc-local-options)) ; we're doing this in a temp buffer, but we need access to the original buffer's options.
	(with-temp-buffer
	  (insert (format "# pandoc-mode settings file for %s #\n" (file-name-nondirectory filename))
		  (format "# saved on %s #\n\n" (format-time-string "%Y.%m.%d %H:%M")))
	  (mapc #'(lambda (option)
		    (when (cdr option)
		      (insert (format "%s::%s\n" (car option) (cdr option)))))
		local-options)
	  (message settings-file)
	  (let ((make-backup-files nil))
	    (write-region (point-min) (point-max) settings-file))
	  (message "Settings file written to `%s'." (file-name-nondirectory settings-file)))
	(setq pandoc-settings-modified-flag nil)))))

(defun pandoc-load-default-settings ()
  "Load the default settings of the file in the current buffer.
This function is for use in pandoc-mode-hook."
  (pandoc-load-settings-profile "default"))

(defun pandoc-load-settings-profile (format &optional no-confirm)
  "Load the options for FORMAT from the corresponding settings file.
If NO-CONFIRM is t, no confirmation is asked if the current
settings have not been saved."
  (pandoc-load-settings-file (pandoc-create-settings-filename (expand-file-name (buffer-file-name)) format) no-confirm))

(defun pandoc-load-settings-file (settings-file &optional no-confirm)
  "Load the options in SETTINGS-FILE for the current buffer.
If NO-CONFIRM is t, no confirmation is asked if the current
settings have not been saved."
  (interactive "FLoad settings file: ")
  (when (and (not no-confirm)
	     pandoc-settings-modified-flag
	     (y-or-n-p (format "Current settings for format %s modified. Save first? " (pandoc-get 'write))))
    (pandoc-save-settings (pandoc-get 'write) t))
  (let ((full-settings-filename (expand-file-name settings-file)))
    (if (file-exists-p full-settings-filename)
	(progn
	  (setq pandoc-local-options (copy-alist pandoc-options))
	  (mapc #'(lambda (option)
		    (pandoc-set (car option) (cdr option)))
		(pandoc-read-settings-from-file full-settings-filename))
	  (message "Settings file `%s' loaded." (file-name-nondirectory full-settings-filename))
	  (setq pandoc-settings-modified-flag nil))
      (message "Settings file `%s' not found." full-settings-filename))))

(defun pandoc-read-settings-from-file (settings-file)
  "Read the options in SETTINGS-FILE.
Returns an alist with the options and their values."
  (if (file-readable-p settings-file)
      (let ((options nil))
	(with-temp-buffer
	  (insert-file-contents settings-file)
	  (goto-char (point-min))
	  (let (options)
	    (while (re-search-forward "^\\([a-z-]*\\)::\\(.*?\\)$" nil t)
	      (let ((option (match-string 1))
		    (value (match-string 2)))
		(add-to-list 'options (cons (intern option) (cond
							     ((string-match "^[0-9]$" value) (string-to-number value))
							     ((string= "t" value) t)
							     ((string= "nil" value) nil)
							     (t value))))))
	    options)))
    (error "Settings file `%s' not readable" settings-file)))

(defun pandoc-view-output ()
  "Displays the *Pandoc output* buffer."
  (interactive)
  (display-buffer pandoc-output-buffer))

(defun pandoc-view-settings ()
  "Displays the settings file in the *Pandoc output* buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(format (pandoc-get 'write)))
    (set-buffer pandoc-output-buffer)
    (erase-buffer)
    (insert-file-contents (pandoc-create-settings-filename filename format)))
  (display-buffer pandoc-output-buffer))  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to set specific options. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pandoc-set-write (format)
  "Sets the output format to FORMAT.
If a settings file exists for FORMAT, it is loaded. If none
exists, all options are unset (except the input format)."
  (interactive (list (completing-read "Set output format to: " pandoc-output-formats)))
  (when (and pandoc-settings-modified-flag
	     (y-or-n-p (format "Current settings for output format `%s' changed. Save? " (pandoc-get 'write))))
    (pandoc-save-settings (pandoc-get 'write) t))
  (if (file-exists-p (pandoc-create-settings-filename (buffer-file-name) format))
      (pandoc-load-settings-profile format t)
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
	       ((null prefix) (expand-file-name (read-file-name "Output file: ")))
	       (t t))))

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
		(expand-file-name (read-file-name "File to include in the header: ")))))

(defun pandoc-set-include-before-body (prefix)
  "Set the file to be included before the body.
If called with the prefix argument C-u - (or M--), the include
before body file is unset."
  (interactive "P")
  (pandoc-set 'include-before-body
	      (if (eq prefix '-)
		  nil
		(expand-file-name (read-file-name "File to include before the body: ")))))

(defun pandoc-set-include-after-body (prefix)
  "Set the file to be included after the body.
If called with the prefix argument C-u - (or M--), the include
after body file is unset."
  (interactive "P")
  (pandoc-set 'include-after-body
	      (if (eq prefix '-)
		  nil
		(expand-file-name (read-file-name "File to include after the body: ")))))

(defun pandoc-set-custom-header (prefix)
  "Select the custom header file.
If called with the prefix argument C-u - (or M--), the custom
header file is unset."
  (interactive "P")
  (pandoc-set 'custom-header
	      (if (eq prefix '-)
		  nil
		(expand-file-name (read-file-name "Select the custom header file: ")))))

(defun pandoc-set-title-prefix (prefix)
  "Set title prefix.
If called with the prefix argument C-u - (or M--), the title
prefix is unset."
  (interactive "P")
  (pandoc-set 'title-prefix
	      (if (eq prefix '-)
		  nil
		(read-string "Title prefix: "))))

(defun pandoc-set-tab-stop (prefix)
  "Set the tab stop size.
If called with the prefix argument C-u - (or M--), the tab stop
width is set to its default value 4."
  (interactive "P")
  (pandoc-set 'tab-stop
	      (if (eq prefix '-)
		  nil
		(string-to-number (read-string "Tab stop: ")))))

(defun pandoc-set-latexmathml (prefix)
  "Use LaTeXMathML to display embedded TeX in HTML output.
If called with the prefix argument C-u - (or M--), LaTeXMathML is
not used. file is unset. If called with any other prefix
argument, the LaTeXMathML script will be added to the HTML header."
  (interactive "P")
  (pandoc-set 'latexmathml
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-string "LaTeXMathML URL: "))
	       (t t))))

(defun pandoc-set-jsmath (prefix)
  "Use jsMath to display embedded TeX math in HTML output.
If called with the prefix argument C-u - (or M--), no jsMath will be used."
  (interactive "P")
  (pandoc-set 'jsmath
	      (if (eq prefix '-)
		  nil
		(string-to-number (read-string "Tab stop: ")))))

(defun pandoc-set-mimetex (prefix)
  "Render TeX math using the MimeTeX CGI script.
If called with the prefix argument C-u - (or M--), MimeTeX is not
used. file is unset. If called with any other prefix argument,
the MimeTeX CGI script will assumed to be in
/cgi-bin/mimetex.cgi."
  (interactive "P")
  (pandoc-set 'mimetex
	      (cond
	       ((eq prefix '-) nil)
	       ((null prefix) (read-string "MimeTeX CGI script: "))
	       (t t))))

(defun pandoc-set-email-obfuscation (prefix)
  "Set the option `Email Obfuscation'.
If called with prefix argument C-u - (or M--), Email Obfuscation
is unset."
  (interactive "P")
  (pandoc-set 'email-obfuscation
	      (if (eq prefix '-)
		  nil
		(let ((value (completing-read "Set email obfuscation: " '("none" "javascript" "references") nil t)))
		  (if (member value '("" "none"))
		      nil
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
    ["Load Settings File..." pandoc-load-settings-file :active t]
    ["Save File Settings" pandoc-save-settings-interactive :active t]
    ["Set As Default Format" pandoc-set-default-format :active t]
    "--"
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
			     ("LaTeX" . "latex"))))
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
			     ("OpenDocument XML" . "opendocument")
			     ("OpenOffice Text Document" . "odt")
			     ("S5 HTML/JS Slide Show" . "s5")
			     ("Rich Text Format" . "rtf"))))
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
     ("CSS Style Sheet"
      ["No CSS Style Sheet" (pandoc-set 'css nil) :active t
       :style radio :selected (null (pandoc-get 'css))]
      ["Set CSS Style Sheet..." pandoc-set-css :active t
      :style radio :selected (stringp (pandoc-get 'css))])
     ("Include In Header"
      ["Nothing Included In Header" (pandoc-set 'include-in-header nil) :active t
       :style radio :selected (null (pandoc-get 'include-in-header))]
      ["Include In Header..." pandoc-set-include-in-header :active t
      :style radio :selected (stringp (pandoc-get 'include-in-header))])
     ("Include Before Body"
      ["Nothing Included Before Body" (pandoc-set 'include-before-body nil) :active t
       :style radio :selected (null (pandoc-get 'include-before-body))]
      ["Include Before Body..." pandoc-set-include-before-body :active t
      :style radio :selected (stringp (pandoc-get 'include-before-body))])
     ("Include After Body"
      ["Nothing Included After Body" (pandoc-set 'include-after-body nil) :active t
       :style radio :selected (null (pandoc-get 'include-after-body))]
      ["Include After Body..." pandoc-set-include-after-body :active t
      :style radio :selected (stringp (pandoc-get 'include-after-body))])
     ("Custom Header"
      ["No Custom Header" (pandoc-set 'custom-header nil) :active t
       :style radio :selected (null (pandoc-get 'custom-header))]
      ["Set Custom Header File..." pandoc-set-custom-header :active t
      :style radio :selected (stringp (pandoc-get 'custom-header))]))
    
    ("Options"
     ("Title Prefix"
      ["No Title Prefix" (pandoc-set 'title-prefix nil) :active t
       :style radio :selected (null (pandoc-get 'title-prefix))]
      ["Set Title Prefix..." pandoc-set-title-prefix :active t
      :style radio :selected (pandoc-get 'title-prefix)])
     ("Tab Stops"
      ["Default Tab Stops" (pandoc-set 'tab-stop nil) :active t
       :style radio :selected (null (pandoc-get 'tab-stop))]
      ["Set Tab Stop Width..." pandoc-set-tab-stop :active t
      :style radio :selected (pandoc-get 'tab-stop)])
     ("LaTeXMathML"
      ["No LaTeXMathML" (pandoc-set 'latexmathml nil) :active t
       :style radio :selected (null (pandoc-get 'latexmathml))]
      ["Add Script To HTML Header" (pandoc-set 'latexmathml t)  :active t
       :style radio :selected (eq (pandoc-get 'latexmathml) t)]
      ["Set LaTeXMathML URL..." pandoc-set-latexmathml :active t
      :style radio :selected (stringp (pandoc-get 'latexmathml))])
     ("jsMath"
      ["No jsMath" (pandoc-set 'jsmath) :active t
       :style radio :selected (null (pandoc-get 'jsmath))]
      ["Set jsMath URL..." pandoc-set-jsmath :active t
      :style radio :selected (pandoc-get 'jsmath)])
     ("MimeTeX"
      ["No MimeTeX" (pandoc-set 'mimetex nil) :active t
       :style radio :selected (null (pandoc-get 'mimetex))]
      ["Default Script Location" (pandoc-set 'mimetex t)  :active t
       :style radio :selected (eq (pandoc-get 'mimetex) t)]
      ["Set MimeTeX GCI Script..." pandoc-set-mimetex :active t
      :style radio :selected (stringp (pandoc-get 'mimetex))])
     ("Email Obfuscation"
      ["None" (pandoc-set 'email-obfuscation nil) :active t
       :style radio :selected (not (pandoc-get 'email-obfuscation))]
      ["Javascript" (pandoc-set 'email-obfuscation "javascript") :active t
       :style radio :selected (string= (pandoc-get 'email-obfuscation) "javascript")]
      ["References" (pandoc-set 'email-obfuscation "references") :active t
       :style radio :selected (string= (pandoc-get 'email-obfuscation) "references")])
     ,@(mapcar #'(lambda (option)
		   (vector (car option) `(pandoc-toggle (quote ,(cdr option)))
			   :active t
			   :style 'toggle
			   :selected `(pandoc-get (quote ,(cdr option)))))
	       pandoc-binary-switches))))

(easy-menu-add pandoc-mode-menu pandoc-mode-map)

(provide 'pandoc-mode)

;;; pandoc-mode ends here
