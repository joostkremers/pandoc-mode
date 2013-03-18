;;; pandoc-mode.el --- Minor mode for interacting with Pandoc

;; Copyright (c) 2009-2013 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 31 Oct 2009
;; Version: 2.4.0
;; Keywords: text, pandoc

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

(defmacro nor (&rest args)
  "Return T if none of its arguments are true."
  `(not (or ,@args)))

(defun nonempty (string)
  "Return STRING, unless it is \"\", in which case return NIL."
  (when (not (string= string ""))
    string))

(defgroup pandoc nil "Minor mode for interacting with pandoc." :group 'wp)

(defcustom pandoc-binary "/usr/bin/pandoc"
  "*The full path of the pandoc binary."
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

(defcustom pandoc-major-modes
  '((haskell-mode . "native")
    (text-mode . "markdown")
    (markdown-mode . "markdown")
    (mediawiki-mode . "mediawiki")
    (textile-mode . "textile")
    (rst-mode . "rst")
    (html-mode . "html")
    (latex-mode . "latex")
    (json-mode . "json"))
  "*List of major modes and their default pandoc input formats."
  :group 'pandoc
  :type '(repeat (cons (symbol :tag "Major mode") (string :tag "Input format"))))

(defvar pandoc-input-formats-menu
  '(("Native Haskell" . "native")
    ("Markdown" . "markdown")
    ("Markdown (Strict)" . "markdown_strict")
    ("Markdown (PHPExtra)" . "markdown_phpextra")
    ("Markdown (Github)" . "markdown_github")
    ("reStructuredText" . "rst")
    ("HTML" . "html")
    ("LaTeX" . "latex")
    ("Textile" . "textile")
    ("JSON" . "json"))
  "List of items in pandoc-mode's input format menu.")

(defvar pandoc-input-formats
  (mapcar #'cdr pandoc-input-formats-menu)
  "List of pandoc input formats.")

(defvar pandoc-output-formats-menu nil
  "List of items in pandoc-mode's output format menu.")

(defvar pandoc-output-formats-list nil
  "List of Pandoc output formats.")

(defun pandoc-set-output-formats (var value)
  "Set PANDOC-OUTPUT-FORMATS.
The value of this option is the basis for setting
PANDOC-OUTPUT-FORMATS-MENU and PANDOC-OUTPUT-FORMATS-LIST."
  (setq pandoc-output-formats-menu (mapcar #'(lambda (elem)
                                               (cons (cadr (cdr elem)) (car elem)))
                                           value))
  (setq pandoc-output-formats-list (mapcar #'(lambda (elem)
                                               (cons (car elem) (cadr elem)))
                                           value))
  (set-default var value))

(defcustom pandoc-output-formats
  '(("native"            ".hs"      "Native Haskell")
    ("plain"             ".txt"     "Plain Text")
    ("markdown"          ".md"      "Markdown")
    ("markdown_strict"   ".md"      "Markdown (Strict)")
    ("markdown_phpextra" ".md"      "Markdown (PHPExtra)")
    ("markdown_github"   ".md"      "Markdown (Github)")
    ("rst"               ".rst"     "reStructuredText")
    ("html"              ".html"    "HTML")
    ("html5"             ".html"    "HTML5")
    ("latex"             ".tex"     "LaTeX")
    ("beamer"            ".tex"     "Beamer Slide Show")
    ("context"           ".tex"     "ConTeXt")
    ("man"               ""         "Man Page")
    ("mediawiki"         ".mw"      "MediaWiki")
    ("texinfo"           ".texi"    "TeXinfo")
    ("docbook"           ".xml"     "DocBook XML")
    ("epub"              ".epub"    "EPUB E-Book")
    ("epub3"             ".epub"    "EPUB3 E-Book")
    ("fb2"               ".fb2"     "FictionBook2")
    ("opendocument"      ".odf"     "OpenDocument XML")
    ("odt"               ".odt"     "OpenOffice Text Document")
    ("docx"              ".docx"    "MS Word")
    ("s5"                ".html"    "S5 HTML/JS Slide Show")
    ("slidy"             ".html"    "Slidy Slide Show")
    ("slideous"          ".html"    "Slideous Slide Show")
    ("dzslides"          ".html"    "DZSlides Slide Show")
    ("rtf"               ".rtf"     "Rich Text Format")
    ("textile"           ".textile" "Textile")
    ("org"               ".org"     "Org-mode")
    ("json"              ".json"    "JSON")
    ("asciidoc"          ".txt"     "AsciiDoc"))
  "*List of Pandoc output formats and their associated file extensions.
The file extension should include a dot. The description appears
in the menu. Note that it does not make sense to change the names
of the output formats, since Pandoc only recognizes the ones
listed here. It is possible to customize the extensions and the
descriptions, though, and you can remove output formats you don't
use, if you want to unclutter the menu a bit."
  :group 'pandoc
  :type '(repeat :tag "Output Format" (list (string :tag "Format") (string :tag "Extension") (string :tag "Description")))
  :set 'pandoc-set-output-formats)

(defvar pandoc-extensions
  '(("footnotes"                  ("markdown" "markdown_phpextra"))
    ("inline_notes"               ("markdown"))
    ("pandoc_title_block"         ("markdown"))
    ("mmd_title_block"            ())
    ("table_captions"             ("markdown"))
    ("implicit_figures"           ("markdown"))
    ("simple_tables"              ("markdown"))
    ("multiline_tables"           ("markdown"))
    ("grid_tables"                ("markdown"))
    ("pipe_tables"                ("markdown" "markdown_phpextra" "markdown_github"))
    ("citations"                  ("markdown"))
    ("raw_tex"                    ("markdown"))
    ("raw_html"                   ("markdown" "markdown_phpextra" "markdown_github"))
    ("tex_math_dollars"           ("markdown"))
    ("tex_math_single_backslash"  ("markdown_github"))
    ("tex_math_double_backslash"  ())
    ("latex_macros"               ("markdown"))
    ("fenced_code_blocks"         ("markdown" "markdown_phpextra" "markdown_github"))
    ("fenced_code_attributes"     ("markdown" "markdown_github"))
    ("backtick_code_blocks"       ("markdown" "markdown_github"))
    ("inline_code_attributes"     ("markdown"))
    ("markdown_in_html_blocks"    ("markdown"))
    ("markdown_attribute"         ("markdown_phpextra"))
    ("escaped_line_breaks"        ("markdown"))
    ("link_attributes"            ())
    ("autolink_bare_uris"         ("markdown_github"))
    ("fancy_lists"                ("markdown"))
    ("startnum"                   ("markdown"))
    ("definition_lists"           ("markdown" "markdown_phpextra"))
    ("example_lists"              ("markdown"))
    ("all_symbols_escapable"      ("markdown"))
    ("intraword_underscores"      ("markdown" "markdown_phpextra" "markdown_github"))
    ("blank_before_blockquote"    ("markdown"))
    ("blank_before_header"        ("markdown"))
    ("strikeout"                  ("markdown" "markdown_github"))
    ("superscript"                ("markdown"))
    ("subscript"                  ("markdown"))
    ("hard_line_breaks"           ("markdown_github"))
    ("abbreviations"              ("markdown_phpextra"))
    ("auto_identifiers"           ("markdown"))
    ("header_attributes"          ("markdown" "markdown_phpextra"))
    ("mmd_header_identifiers"     ())
    ("implicit_header_references" ("markdown"))
    ("line_blocks"                ("markdown")))
  "List of Markdown extensions supported by Pandoc.")

(defvar pandoc-cli-options
  '(variable
    data-dir
    email-obfuscation
    latex-engine)
  "List of options accepted by the pandoc binary.
A few options are preset, other options are added by the
PANDOC-DEFINE-*-OPTION functions. The options --read, --write
and -output are not in this list, because they need special
treatment.")

(defvar pandoc-filepath-options
  '(data-dir)
  "List of options that have a file path as value.
These file paths are expanded before they are sent to pandoc. For
relative paths, the file's working directory is used as base
directory.

One option is preset, others are added by PANDOC-DEFINE-FILE-OPTION.")

(defvar pandoc-binary-options nil
  "List of binary options.
These are set by PANDOC-DEFINE-BINARY-OPTION.")

(defvar pandoc-options
  `((read)
    (read-lhs)
    (read-extensions ,@(mapcar 'list (sort (mapcar 'car pandoc-extensions) 'string<)))
    (write . "native")
    (write-lhs)
    (write-extensions ,@(mapcar 'list (sort (mapcar 'car pandoc-extensions) 'string<)))
    (output)
    (data-dir)
    (output-dir) ; this is not actually a pandoc option
    (variable)
    (email-obfuscation)
    (latex-engine))
  "Pandoc option alist.
List of options and their default values. For each buffer in
which pandoc-mode is activated, a buffer-local copy of this list
is made that stores the local values of the options. (In fact,
two copies are made, one for the local options and one for the
project options.

The PANDOC-DEFINE-*-OPTION functions add their options to this
list with the default value NIL.")

;; Regarding storing of options: both per-file options and project are stored in
;; buffer-local variables. Note that PANDOC-LOCAL-OPTIONS stores *all* options,
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

(defvar pandoc-options-menu nil
  "Auxiliary variable for creating the options menu.")

(defvar pandoc-files-menu nil
  "Auxiliary variable for creating the file menu.")

(defmacro with-pandoc-output-buffer (&rest body)
  "Execute BODY with PANDOC-OUTPUT-BUFFER temporarily current.
Make sure that PANDOC-OUTPUT-BUFFER really exists."
  (declare (indent defun))
  `(progn
     (or (buffer-live-p pandoc-output-buffer)
         (setq pandoc-output-buffer (get-buffer-create " *Pandoc output*")))
     (with-current-buffer pandoc-output-buffer
       ,@body)))

(defmacro pandoc-define-binary-option (option description)
  "Create a binary option.
OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). DESCRIPTION is the
description of the option as it will appear in the menu."
  (declare (indent defun))
  `(progn
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-binary-options (cons ,description (quote ,option)) t)
     (add-to-list 'pandoc-options (list (quote ,option))) t))

(defmacro pandoc-define-file-option (option prompt &optional full-path default)
  "Define a file option.
The option is added to PANDOC-CLI-OPTIONS and PANDOC-OPTIONS, and to
PANDOC-FILEPATH-OPTIONS (unless FULL-PATH is NIL). Furthermore,
a menu entry is created and a function to set/unset the option.

The function to set the option can be called with the prefix
argument C-u - (or M--) to unset the option. A default value (if
any) can be set by calling the function with any other prefix
argument. If no prefix argument is given, the user is prompted
for a value.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). PROMPT is a string that is
used to prompt for setting and unsetting the option. It must be
formulated in such a way that the strings \"No \", \"Set \" and
\"Default \" can be added before it. If FULL-PATH is T, the full
path to the file is stored, otherwise just the file name without
directory. DEFAULT must be either NIL or T and indicates whether
the option can have a default value."
  (declare (indent defun))
  `(progn
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     ,(when full-path
        `(add-to-list 'pandoc-filepath-options (quote ,option) t))
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-files-menu
                  (list ,@(delq nil ; if DEFAULT is nil, we need to remove it from the list.
                                (list prompt
                                      (vector (concat "No " prompt) `(pandoc-set 'local (quote ,option) nil)
                                              :active t
                                              :style 'radio
                                              :selected `(null (pandoc-get 'local (quote ,option))))
                                      (when default
                                        (vector (concat "Default " prompt) `(pandoc-set 'local (quote ,option) t)
                                                :active t
                                                :style 'radio
                                                :selected `(eq (pandoc-get 'local (quote ,option)) t)))
                                      (vector (concat "Set " prompt "...") (intern (concat "pandoc-set-"
                                                                                           (symbol-name option)))
                                              :active t
                                              :style 'radio
                                              :selected `(stringp (pandoc-get 'local (quote ,option)))))))
                  t)
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (pandoc-set 'local (quote ,option)
                           (cond
                            ((eq prefix '-) nil)
                            ((null prefix) ,(if full-path
                                                `(read-file-name ,(concat prompt ": "))
                                              `(file-name-nondirectory (read-file-name ,(concat prompt ": ")))))
                            (t ,default)))))))

(defmacro pandoc-define-numeric-option (option prompt)
  "Define a numeric option.
The option is added to PANDOC-CLI-OPTIONS and PANDOC-OPTIONS.
Furthermore, a menu entry is created and a function to set/unset
the option.

The function to set the option can be called with the prefix
argument C-u - (or M--) to unset the option. If no prefix
argument is given, the user is prompted for a value.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). PROMPT is a string that is
used to prompt for setting and unsetting the option. It must be
formulated in such a way that the strings \"Default \" and \"Set
\" can be added before it."
  (declare (indent defun))
  `(progn
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-options-menu
                  (list ,prompt
                        ,(vector (concat "Default " prompt) `(pandoc-set 'local (quote ,option) nil)
                                 :active t
                                 :style 'radio
                                 :selected `(null (pandoc-get 'local (quote ,option))))
                        ,(vector (concat "Set " prompt "...") (intern (concat "pandoc-set-" (symbol-name option)))
                                 :active t
                                 :style 'radio
                                 :selected `(pandoc-get 'local (quote ,option))))
                  t)
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (pandoc-set 'local (quote ,option)
                           (if (eq prefix '-)
                               nil
                             (string-to-number (read-string ,(concat prompt ": ")))))))))

(defmacro pandoc-define-string-option (option prompt &optional default)
  "Define a option whose value is a string.
The option is added to PANDOC-CLI-OPTIONS and PANDOC-OPTIONS.
Furthermore, a menu entry is created and a function to set the
option.

The function to set the option can be called with the prefix
argument C-u - (or M--) to unset the option. A default value (if
any) can be set by calling the function with any other prefix
argument. If no prefix argument is given, the user is prompted
for a value.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). PROMPT is a string that is
used to prompt for setting and unsetting the option. It must be
formulated in such a way that the strings \"No \", \"Set \" and
\"Default \" can be added before it. DEFAULT must be either NIL
or T and indicates whether the option can have a default value."
  `(progn
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-options-menu
                  (list ,@(delq nil ; if DEFAULT is nil, we need to remove it from the list.
                                (list prompt
                                      (vector (concat "No " prompt) `(pandoc-set 'local (quote ,option) nil)
                                              :active t
                                              :style 'radio
                                              :selected `(null (pandoc-get 'local (quote ,option))))
                                      (when default
                                        (vector (concat "Default " prompt) `(pandoc-set 'local (quote ,option) t)
                                                :active t
                                                :style 'radio
                                                :selected `(eq (pandoc-get 'local (quote ,option)) t)))
                                      (vector (concat "Set " prompt "...") (intern (concat "pandoc-set-" (symbol-name option)))
                                              :active t
                                              :style 'radio
                                              :selected `(stringp (pandoc-get 'local (quote ,option)))))))
                  t)
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (pandoc-set 'local (quote ,option)
                           (cond
                            ((eq prefix '-) nil)
                            ((null prefix) (read-string ,(concat prompt ": ")))
                            (t ,default)))))))

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
    (define-key map "\C-c/p" 'pandoc-convert-to-pdf)
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

;;;###autoload
(define-minor-mode pandoc-mode
  "Minor mode for interacting with Pandoc."
  :init-value nil :lighter (:eval (concat " Pandoc/" (pandoc-get 'local 'write))) :global nil
  (cond
   (pandoc-mode    ; pandoc-mode is turned on
    (setq pandoc-local-options (copy-alist pandoc-options))
    (pandoc-set 'local 'read (cdr (assq major-mode pandoc-major-modes)))
    (setq pandoc-settings-modified-flag nil)
    (or (buffer-live-p pandoc-output-buffer)
        (setq pandoc-output-buffer (get-buffer-create " *Pandoc output*"))))
   ((not pandoc-mode)    ; pandoc-mode is turned off
    (setq pandoc-local-options nil
          pandoc-settings-modified-flag nil))))

;;;###autoload
(defun turn-on-pandoc ()
  "Unconditionally turn on pandoc-mode."
  (interactive)
  (pandoc-mode 1))

(defun turn-off-pandoc ()
  "Unconditionally turn off pandoc-mode"
  (interactive)
  (pandoc-mode -1))

;;;###autoload
(defun conditionally-turn-on-pandoc ()
  "Turn on pandoc-mode if a pandoc settings file exists.
This is for use in major mode hooks."
  (when (file-exists-p (pandoc-create-settings-filename 'settings (buffer-file-name) "default"))
    (turn-on-pandoc)))

(defun pandoc-set (type option value)
  "Sets the local value of OPTION to VALUE.
If OPTION is 'variable, VALUE should be a cons of the
form (variable-name . value), which is then added to the
variables already stored, or just (variable-name), in which case
the named variable is deleted from the list.
TYPE is either 'local or 'project."
  (when (assq option pandoc-options) ; check if the option is licit
    (let ((new-value
           (cond
            ((eq option 'variable)
               ;; new variables are added to the list; existing variables are
               ;; overwritten or deleted.
               (append (assq-delete-all (car value) (pandoc-get type 'variable))
                       (if (cdr value)
                           (list value)
                         nil)))
            ;; the extensions are only supported here to make reading
            ;; settings files easier. once these are converted to lisp
            ;; format, the following two cond-clauses can be removed again.
            ((eq option 'read-extensions)
             (pandoc-set-extension (car value) 'read (cdr value) type))
            ((eq option 'write-extensions)
             (pandoc-set-extension (car value) 'write (cdr value) type))
             ;; all other options simply override the existing value.
             (t value))))
      (setcdr (assq option (cond
                            ((eq type 'local) pandoc-local-options)
                            ((eq type 'project) pandoc-project-options)))
              new-value))
    (setq pandoc-settings-modified-flag t)))

(defun pandoc-get (type option &optional buffer)
  "Returns the value of OPTION according to TYPE (either 'local or 'project).
Optional argument BUFFER is the buffer from which the value is to
be retrieved."
  (cdr (assq option (buffer-local-value (intern (format "pandoc-%s-options" type))
                                        (or buffer (current-buffer))))))

(defun pandoc-toggle (type option)
  "Toggles the value of an on/off option.
TYPE is either 'local or 'project."
  (pandoc-set 'local option (not (pandoc-get type option))))


;; Note: the extensions appear to be binary options, but they are not: they
;; can be on or off, but that doesn't tell us whether they're on or off
;; because the user set them that way or because that's the default setting
;; for the relevant format.
;;
;; What we do is we create an alist of the extensions, where each extension
;; can have one of three values: nil, meaning default, the symbol -,
;; meaning switched off by the user, or the symbol +, meaning switched on
;; by the user.

(defun pandoc-extension-in-format-p (extension format &optional rw)
  "Check if EXTENSION is a default extension for FORMAT.
RW must be either 'read or 'write, indicating whether FORMAT is
being considered as an input or an output format."
  (let ((formats (cadr (assoc extension pandoc-extensions))))
    (or (member format formats)
        (member format (cadr (assoc rw formats))))))

(defun pandoc-extension-active-p (extension rw)
  "Return T if EXTENSION is active in the current buffer.
RW is either 'read or 'write, indicating whether to test for the
input or the output format.

An extension is active either if it's part of the in/output
format and hasn't been deactivated by the user, or if the user
has activated it."
  (let ((value (pandoc-get-extension extension rw)))
    (or (eq value '+)
        (and (not value)
             (pandoc-extension-in-format-p extension (pandoc-get 'local rw) rw)))))

(defun pandoc-set-extension (extension rw value &optional type)
  "Set the value of EXTENSION for RW to VALUE.
RW is either 'read or 'write, indicating whether the read or
write extension is to be set.

TYPE is either 'local or 'project."
  (or type (setq type 'local))
  (setcdr (assoc extension (if (eq rw 'read)
                               (pandoc-get type 'read-extensions)
                             (pandoc-get type 'write-extensions)))
          value))

(defun pandoc-get-extension (extension rw)
  "Return the value of EXTENSION for RW.
RW is either 'read or 'write, indicating whether the read or
write extension is to be queried."
  (cdr (assoc extension (if (eq rw 'read)
                            (pandoc-get 'local 'read-extensions)
                          (pandoc-get 'local 'write-extensions)))))

(defun pandoc-toggle-extension (extension rw)
  "Toggle the value of EXTENSION.
RW is either 'read or 'write, indicating whether the extension
should be toggled for the input or the output format."
  (interactive (list (completing-read "Extension: " pandoc-extensions nil t)
                     (intern (completing-read "Read/write: " '("read" "write") nil t))))
  (let* ((current-value (pandoc-get-extension extension rw))
         (new-value (cond
                     ((memq current-value '(+ -)) ; if the value is set explicitly
                      nil)  ; we can simply return it to the default
                     ((pandoc-extension-in-format-p extension (pandoc-get 'local rw) rw) ; if the extension is part of the current format
                      '-)  ; we explicitly unset it
                     (t '+)))) ; otherwise we explicitly set it
    (pandoc-set-extension extension rw new-value)))

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
  "Create a list of strings with pandoc options for the current buffer.
INPUT-FILE is the name of the input file. If PDF is non-nil, an
output file is always set, derived either from the input file or
from the output file set for the \"latex\" output profile, and
gets the suffix `.pdf'. If the output format is \"odt\", \"epub\"
or \"docx\" but no output file is specified, one will be created,
since pandoc does not support output to stdout for those two
formats."
  (let ((read (format "--read=%s%s%s" (pandoc-get 'local 'read) (if (pandoc-get 'local 'read-lhs) "+lhs" "")
                      (pandoc-create-extensions-string (pandoc-get 'local 'read-extensions))))
        (write (if pdf
                   nil
                 (format "--write=%s%s%s" (pandoc-get 'local 'write) (if (pandoc-get 'local 'write-lhs) "+lhs" "")
                         (pandoc-create-extensions-string (pandoc-get 'local 'write-extensions)))))
        (output (cond
                 ((or (eq (pandoc-get 'local 'output) t)                     ; if the user set the output file to T
                      (and (null (pandoc-get 'local 'output))                ; or if the user set no output file but either
                           (or pdf                                    ; (i) we're converting to pdf, or
                               (member (pandoc-get 'local 'write)            ; (ii) the output format is odt, epub or docx
                                       '("odt" "epub" "docx")))))
                  (format "--output=%s/%s%s"                          ; we create an output file name.
                          (expand-file-name (or (pandoc-get 'local 'output-dir)
                                                (file-name-directory input-file)))
                          (file-name-sans-extension (file-name-nondirectory input-file))
                          (if pdf
                              ".pdf"
                            (cdr (assoc (pandoc-get 'local 'write) pandoc-output-formats-list)))))
                 ((stringp (pandoc-get 'local 'output))                      ; if the user set an output file,
                  (format "--output=%s/%s"                            ; we combine it with the output directory
                          (expand-file-name (or (pandoc-get 'local 'output-dir)
                                                (file-name-directory input-file)))
                          (if pdf                                     ; and check if we're converting to pdf
                              (concat (file-name-sans-extension (pandoc-get 'local 'output)) ".pdf")
                            (pandoc-get 'local 'output))))
                 (t nil)))
        (variables (mapcar #'(lambda (variable)
                               (format "--variable=%s:%s" (car variable) (cdr variable)))
                           (pandoc-get 'local 'variable)))
        (other-options (mapcar #'(lambda (option)
                                   (let ((value (pandoc-get 'local option)))
                                     (when (and value (memq option pandoc-filepath-options))
                                       (setq value (expand-file-name value)))
                                     (cond
                                      ((eq value t) (format "--%s" option))
                                      ((stringp value) (format "--%s=%s" option value))
                                      (t nil))))
                               pandoc-cli-options)))
    (delq nil (append (list read write output) variables other-options))))

(defun pandoc-create-extensions-string (extensions)
  "Create a string of extensions to be added to the Pandoc command line."
  (mapconcat #'(lambda (elt)
                 (if (cdr elt)
                     (format "%s%s" (cdr elt) (car elt))
                   ""))
             extensions
             ""))

(defun pandoc-process-directives (output-format)
  "Processes pandoc-mode @@-directives in the current buffer.
OUTPUT-FORMAT is passed unchanged to the functions associated
with the @@-directives."
  (interactive (list (pandoc-get 'local 'write)))
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
format is used."
  (let ((filename (buffer-file-name buffer)))
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
            (pandoc-set 'local 'write output-format)
            (pandoc-set 'local 'read (pandoc-get 'local 'read buffer)))
        ;; if no output format was provided, we use the buffer's options:
        (setq pandoc-local-options (buffer-local-value 'pandoc-local-options buffer))
        (setq pandoc-project-options (buffer-local-value 'pandoc-project-options buffer)))
      (let ((option-list (pandoc-create-command-option-list filename pdf)))
        (insert-buffer-substring-no-properties buffer)
        (message "Running pandoc...")
        (pandoc-process-directives (pandoc-get 'local 'write))
        (with-pandoc-output-buffer
          (erase-buffer)
          (insert (format "Running `pandoc %s'\n\n" (mapconcat #'identity option-list " "))))
        (if (= 0 (apply 'call-process-region (point-min) (point-max) pandoc-binary nil pandoc-output-buffer t option-list))
            (message "Running pandoc... Finished.")
          (message "Error in pandoc process.")
          (display-buffer pandoc-output-buffer))))))

(defun pandoc-run-pandoc (prefix)
  "Run pandoc on the current document.
If called with a prefix argument, the user is asked for an output
format. Otherwise, the output format currently set in the buffer
is used."
  (interactive "P")
  (pandoc-call-external (current-buffer)
                        (if prefix
                            (completing-read "Output format to use: " pandoc-output-formats-list nil t)
                          nil)))

(defun pandoc-convert-to-pdf (prefix)
  "Convert the current document to pdf.
If the output format of the current buffer is set to \"latex\",
the buffer's options are used. If called with a prefix argument,
or if the current buffer's output format is not \"latex\", a
LaTeX settings file is searched for and loaded when found. If no
such settings file is found, all options are unset except for the
input and output formats."
  (interactive "P")
  (pandoc-call-external (current-buffer)
                        (if (or prefix
                                (not (string= (pandoc-get 'local 'write) "latex")))
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
    (let ((current-settings-file (file-name-nondirectory (pandoc-create-settings-filename 'settings (buffer-file-name) (pandoc-get 'local 'write))))
          (current-project-file (file-name-nondirectory (pandoc-create-settings-filename 'project (buffer-file-name) (pandoc-get 'local 'write)))))
      (when (not (file-exists-p current-settings-file))
        (pandoc-save-settings 'settings (pandoc-get 'local 'write)))
      (make-symbolic-link current-settings-file (pandoc-create-settings-filename 'settings (buffer-file-name) "default") t)
      (when (file-exists-p current-project-file)
          (make-symbolic-link current-project-file (pandoc-create-settings-filename 'project (buffer-file-name) "default") t))
      (message "`%s' set as default output format." (pandoc-get 'local 'write)))))

(defun pandoc-save-settings-file ()
  "Save the settings of the current buffer.
This function just calls pandoc-save-settings with the
appropriate output format."
  (interactive)
  (pandoc-save-settings 'settings (pandoc-get 'local 'write)))

(defun pandoc-save-project-file ()
  "Save the current settings as a project file.
In order to achieve this, the current local settings are copied
to the project settings."
  (interactive)
  (setq pandoc-project-options (copy-alist pandoc-local-options))
  (pandoc-save-settings 'project (pandoc-get 'local 'write)))

;; A few notes regarding PANDOC-SAVE-SETTINGS:
;;
;; * If TYPE is 'settings, we only need the options in pandoc-local-options
;;   that differ from pandoc-project-options. Note that we convert all
;;   values to strings, so that options that are nil in
;;   pandoc-local-options but non-nil in pandoc-project-options are also
;;   saved below.
;;
;; * The options variable and read|write-extenions are treated specially.
;;   Their values are passed unchanged to pandoc-insert-options. (Unless
;;   the value is NIL, but that's only possible for the option variable.)
;;   If only a single local variable or extension is different from the
;;   project settings, they are *all* saved to the local settings file.

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
        (options (cond
                  ((eq type 'settings)
                   (delq nil (mapcar #'(lambda (option)
                                         (when (not (equal (pandoc-get 'local option)
                                                           (pandoc-get 'project option)))
                                           (cons option 
                                                 (cond
                                                  ((memq option '(variable read-extensions write-extensions))
                                                   (or (pandoc-get 'local option)
                                                       "nil"))
                                                  (t (format "%s" (pandoc-get 'local option)))))))
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
               ((memq (car option) '(read-extensions write-extensions))
                (mapc #'(lambda (extension)
                          (when (cdr extension)
                            (insert (format "%s::%s:%s\n" (car option) (car extension) (cdr extension)))))
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
             (y-or-n-p (format "Current settings for format \"%s\" modified. Save first? " (pandoc-get 'local 'write))))
    (pandoc-save-settings (pandoc-get 'local 'write) t))
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
                (pandoc-set 'local (car option) (cdr option))
                (pandoc-set 'project (car option) (cdr option)))
            project-settings)
      ;; the local settings are processed second, so that they can override the
      ;; project settings.
      (mapc #'(lambda (option)
                (pandoc-set 'local (car option) (cdr option)))
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
            ;; We do the same with the extensions.
            (add-to-list 'options (if (memq option '(variable read-extension write-extensions))
                                      (progn
                                        (string-match "^\\(.*?\\):\\(.*?\\)$" value)
                                        (cons option (cons (intern (match-string 1 value))
                                                           (if (eq option 'variable)
                                                               (match-string 2 value)
                                                             (intern (match-string 2 value))))))
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
  (interactive (list (completing-read "Set output format to: " pandoc-output-formats-list nil t)))
  (when (and pandoc-settings-modified-flag
             (y-or-n-p (format "Current settings for output format \"%s\" changed. Save? " (pandoc-get 'local 'write))))
    (pandoc-save-settings (pandoc-get 'local 'write) t))
  (unless (pandoc-load-settings-profile format t)
    (setq pandoc-local-options (copy-alist pandoc-options))
    (pandoc-set 'local 'write format)
    (pandoc-set 'local 'read (cdr (assq major-mode pandoc-major-modes)))))

(defun pandoc-set-output (prefix)
  "Set the output file.
If called with the prefix argument C-u - (or M--), the output
file is unset. If called with any other prefix argument, the
output file is created on the basis of the input file and the
output format."
  (interactive "P")
  (pandoc-set 'local 'output
              (cond
               ((eq prefix '-) nil)
               ((null prefix) (file-name-nondirectory (read-file-name "Output file: ")))
               (t t))))

(defun pandoc-set-data-dir (prefix)
  "Set the option `Data Directory'.
If called with the prefix argument C-u - (or M--), the data
directory is set to NIL, which means use $HOME/.pandoc."
  (interactive "P")
  (pandoc-set 'local 'data-dir
              (if (eq prefix '-)
                  nil
                (read-directory-name "Data directory: " nil nil t))))

(defun pandoc-set-output-dir (prefix)
  "Set the option `Output Directory'.
If called with the prefix argument C-u - (or M--), the output
directory is set to NIL, which means use the directory of the
input file."
  (interactive "P")
  (pandoc-set 'local 'output-dir
              (if (eq prefix '-)
                  nil
                (read-directory-name "Output directory: " nil nil t))))

(defun pandoc-set-template-variable (prefix)
  "Add/change/remove a template variable.
The user is asked for a variable name. If the function is called
with a prefix value, this variable is removed from the list of
variables, otherwise the user is asked for a value."
  (interactive "P")
  (let ((var (nonempty (completing-read "Variable name: " (pandoc-get 'local 'variable)))))
    (when var
      (setq var (intern var))
      (let ((value (if (eq prefix '-)
                       nil
                     (read-string "Value: " nil nil (cdr (assq var (pandoc-get 'local 'variable)))))))
        (pandoc-set 'local 'variable (cons var value))
        (message "Template variable %s %s." var (if value
                                                    (format "added with value `%s'" value)
                                                  "removed"))))))

(defun pandoc-set-email-obfuscation (prefix)
  "Set the option `Email Obfuscation'.
If called with prefix argument C-u - (or M--), Email Obfuscation
is unset."
  (interactive "P")
  (pandoc-set 'local 'email-obfuscation
              (if (eq prefix '-)
                  "none"
                (let ((value (completing-read "Set email obfuscation: " '("none" "javascript" "references") nil t)))
                  (if (member value '("" "none"))
                      "none"
                    value))))
  (message "Email obfuscation: %s." (or (pandoc-get 'local 'email-obfuscation)
                                        "unset")))

(defun pandoc-set-latex-engine (prefix)
  "Set the option `LaTeX Engine'.
If called with prefix argument C-u - (or M--), LaTeX Engine
is unset."
  (interactive "P")
  (pandoc-set 'local 'latex-engine
              (if (eq prefix '-)
                  nil
                (let ((value (completing-read "Set LaTeX Engine: " '("pdflatex" "xelatex" "lualatex") nil t)))
                  (if (member value '("" "pdflatex"))
                      nil
                    value))))
  (message "LaTeX Engine: %s." (or (pandoc-get 'local 'latex-engine)
                                   "default")))

(pandoc-define-file-option template "Template File" t)
(pandoc-define-file-option css "CSS Style Sheet")
(pandoc-define-file-option reference-odt "Reference ODT File" t)
(pandoc-define-file-option reference-docx "Reference docx File" t)
(pandoc-define-file-option epub-metadata "EPUB Metadata File" t)
(pandoc-define-file-option epub-stylesheet "EPUB Style Sheet" t t)
(pandoc-define-file-option epub-cover-image "EPUB Cover Image" t)
(pandoc-define-file-option epub-embed-font "EPUB Embedded Font" t) ; this option can be repeated, so it should be handled differently.
(pandoc-define-file-option bibliography "Bibliography File" t)
(pandoc-define-file-option csl "CSL File" t)
(pandoc-define-file-option citation-abbreviations "Citation Abbreviations File" t)
(pandoc-define-file-option include-in-header "Include Header" t)
(pandoc-define-file-option include-before-body "Include Before Body" t)
(pandoc-define-file-option include-after-body "Include After Body" t)

(pandoc-define-numeric-option columns "Column Width")
(pandoc-define-numeric-option tab-stop "Tab Stop Width")
(pandoc-define-numeric-option base-header-level "Base Header Level")
(pandoc-define-numeric-option slide-level "Slide Level Header")
(pandoc-define-numeric-option toc-depth "TOC Depth")
(pandoc-define-numeric-option epub-chapter-level "EPub Chapter Level")

(pandoc-define-string-option latexmathml "LaTeXMathML URL" t)
(pandoc-define-string-option mathml "MathML URL" t)
(pandoc-define-string-option mimetex "MimeTeX CGI Script" t)
(pandoc-define-string-option webtex "WebTeX URL" t)
(pandoc-define-string-option jsmath "jsMath URL" t)
(pandoc-define-string-option mathjax "MathJax URL")
(pandoc-define-string-option title-prefix "Title prefix")
(pandoc-define-string-option id-prefix "ID prefix")
(pandoc-define-string-option indented-code-classes "Indented Code Classes")
(pandoc-define-string-option highlight-style "Highlighting Style")
(pandoc-define-string-option number-offset "Number Offsets")
(pandoc-define-string-option default-image-extension "Default Image Extension")

(pandoc-define-binary-option standalone "Standalone")
(pandoc-define-binary-option preserve-tabs "Preserve Tabs")
(pandoc-define-binary-option strict "Strict")
(pandoc-define-binary-option normalize "Normalize Document")
(pandoc-define-binary-option reference-links "Reference Links")
(pandoc-define-binary-option parse-raw "Parse Raw")
(pandoc-define-binary-option smart "Smart")
(pandoc-define-binary-option gladtex "gladTeX")
(pandoc-define-binary-option incremental "Incremental")
(pandoc-define-binary-option self-contained "Self-contained Document")
(pandoc-define-binary-option chapters "Top-level Headers Are Chapters")
(pandoc-define-binary-option number-sections "Number Sections")
(pandoc-define-binary-option listings "Use LaTeX listings Package")
(pandoc-define-binary-option section-divs "Wrap Sections in <div> Tags")
(pandoc-define-binary-option no-wrap "No Wrap")
(pandoc-define-binary-option no-highlight "No Highlighting")
(pandoc-define-binary-option table-of-contents "Table of Contents")
(pandoc-define-binary-option natbib "Use NatBib")
(pandoc-define-binary-option biblatex "Use BibLaTeX")
(pandoc-define-binary-option ascii "Use Only ASCII in HTML")
(pandoc-define-binary-option atx-headers "Use ATX-style Headers")
(pandoc-define-binary-option old-dashes "Use Old-style Dashes")
(pandoc-define-binary-option no-tex-ligatures "Do Not Use TeX Ligatures")
(pandoc-define-binary-option html-q-tags "Use <q> Tags for Quotes in HTML")

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
                                               pandoc-binary-options nil t) pandoc-binary-options))))
      (pandoc-set 'local option (cond
                          ((eq prefix '-) nil)
                          ((null prefix) (not (pandoc-get 'local option)))
                          (t t)))
      (message "Option `%s' %s." (car (rassq option pandoc-binary-options)) (if (pandoc-get 'local option)
                                                                                 "set"
                                                                               "unset")))))

(easy-menu-define pandoc-mode-menu pandoc-mode-map "Pandoc menu"
  `("Pandoc"
    ["Run Pandoc" pandoc-run-pandoc :active t]
    ["Create PDF" pandoc-convert-to-pdf :active t]
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
                                       `(pandoc-set 'local 'read ,(cdr option))
                                       :active t
                                       :style 'radio
                                       :selected `(string= (pandoc-get 'local 'read)
                                                           ,(cdr option))))
                           pandoc-input-formats-menu))
             (list ["Literal Haskell" (pandoc-toggle 'local 'read-lhs)
                    :active (member (pandoc-get 'local 'read) '("markdown" "rst" "latex"))
                    :style toggle :selected (pandoc-get 'local 'read-lhs)])
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc-get 'local 'read)))
                           (mapcar #'(lambda (ext)
                                       (vector (car ext)
                                               `(pandoc-toggle-extension ,(car ext) 'read)
                                               :active t
                                               :style 'toggle
                                               :selected `(pandoc-extension-active-p ,(car ext) 'read)))
                                   pandoc-extensions))))

    ,(append (cons "Output Format"
                   (mapcar #'(lambda (option)
                               (vector (car option)
                                       `(pandoc-set-write ,(cdr option))
                                       :active t
                                       :style 'radio
                                       :selected `(string= (pandoc-get 'local 'write)
                                                           ,(cdr option))))
                           pandoc-output-formats-menu))
             (list ["Literal Haskell" (pandoc-toggle 'local 'write-lhs)
                    :active (member (pandoc-get 'local 'write)
                                    '("markdown" "rst" "latex" "beamer" "html" "html5"))
                    :style toggle :selected (pandoc-get 'local 'write-lhs)])
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc-get 'local 'write)))
                           (mapcar #'(lambda (ext)
                                       (vector (car ext)
                                               `(pandoc-toggle-extension ,(car ext) 'write)
                                               :active t
                                               :style 'toggle
                                               :selected `(pandoc-extension-active-p ,(car ext) 'write)))
                                   pandoc-extensions))))

    ("Files"
     ("Output File"
      ["Output To Stdout" (pandoc-set 'local 'output nil) :active t
       :style radio :selected (null (pandoc-get 'local 'output))]
      ["Create Output Filename" (pandoc-set 'local 'output t) :active t
       :style radio :selected (eq (pandoc-get 'local 'output) t)]
      ["Set Output File..." pandoc-set-output :active t
      :style radio :selected (stringp (pandoc-get 'local 'output))])
     ("Output Directory"
      ["Use Input Directory" (pandoc-set 'local 'output-dir nil) :active t
       :style radio :selected (null (pandoc-get 'local 'output-dir))]
      ["Set Output Directory" pandoc-set-output-dir :active t
       :style radio :selected (pandoc-get 'local 'output-dir)])
     ("Data Directory"
      ["Use Default Data Directory" (pandoc-set 'local 'data-dir nil) :active t
       :style radio :selected (null (pandoc-get 'local 'data-dir))]
      ["Set Data Directory" pandoc-set-data-dir :active t
       :style radio :selected (pandoc-get 'local 'data-dir)])
     ,@pandoc-files-menu)

    ("Options"
     ,@pandoc-options-menu
     ("Template Variables"
      ["Set/Change Template Variable" pandoc-set-template-variable :active t]
      ["Unset Template Variable" (pandoc-set-template-variable '-) :active t])
     ("LaTeX Engine"
      ["PdfLaTeX" (pandoc-set 'local 'latex-engine "pdflatex") :active (string= (pandoc-get 'local 'write) "latex")
       :style radio :selected (null (pandoc-get 'local 'latex-engine))]
      ["XeLaTeX" (pandoc-set 'local 'latex-engine "xelatex") :active (string= (pandoc-get 'local 'write) "latex")
       :style radio :selected (string= (pandoc-get 'local 'latex-engine) "xelatex")]
      ["LuaLaTeX" (pandoc-set 'local 'latex-engine "lualatex") :active (string= (pandoc-get 'local 'write) "latex")
       :style radio :selected (string= (pandoc-get 'local 'latex-engine) "lualatex")])
     ("Email Obfuscation"
      ["None" (pandoc-set 'local 'email-obfuscation "none") :active t
       :style radio :selected (string= (pandoc-get 'local 'email-obfuscation) "none")]
      ["Javascript" (pandoc-set 'local 'email-obfuscation "javascript") :active t
       :style radio :selected (string= (pandoc-get 'local 'email-obfuscation) "javascript")]
      ["References" (pandoc-set 'local 'email-obfuscation "references") :active t
       :style radio :selected (string= (pandoc-get 'local 'email-obfuscation) "references")]))
    ("Switches"
     ;; put the binary options into the menu
     ,@(mapcar #'(lambda (option)
                   (vector (car option) `(pandoc-toggle 'local(quote ,(cdr option)))
                           :active t
                           :style 'toggle
                           :selected `(pandoc-get 'local (quote ,(cdr option)))))
               pandoc-binary-options))))

(easy-menu-add pandoc-mode-menu pandoc-mode-map)

(provide 'pandoc-mode)

;;; pandoc-mode ends here
