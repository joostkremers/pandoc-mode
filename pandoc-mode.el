;;; pandoc-mode.el --- Minor mode for interacting with Pandoc

;; Copyright (c) 2009-2013 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 31 Oct 2009
;; Version: 2.5
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
  "The full path of the pandoc binary."
  :group 'pandoc
  :type 'file)

(defcustom pandoc-data-dir "~/.emacs.d/pandoc-mode/"
  "Default `pandoc-mode' data dir.
This is where `pandoc-mode' looks for global settings files."
  :group 'pandoc
  :type 'directory)

(defcustom pandoc-directives '(("include" . pandoc-process-include-directive)
                               ("lisp" . pandoc-process-lisp-directive))
  "List of directives to be processed before pandoc is called.
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
  "List of functions to call before the directives are processed."
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
  "List of major modes and their default pandoc input formats."
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
    ("JSON" . "json")
    ("OPML" . "opml")
    ("Haddock Markup" . "haddock"))
  "List of items in pandoc-mode's input format menu.")

(defvar pandoc-input-formats
  (mapcar #'cdr pandoc-input-formats-menu)
  "List of pandoc input formats.")

(defvar pandoc-output-formats-menu nil
  "List of items in pandoc-mode's output format menu.")

(defvar pandoc-output-formats-list nil
  "List of Pandoc output formats.")

(defun pandoc-set-output-formats (var value)
  "Set `pandoc-output-formats'.
The value of this option is the basis for setting
`pandoc-output-formats-menu' and `pandoc-output-formats-list'."
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
    ("revealjs"          ".html"    "RevealJS Slide Show")
    ("rtf"               ".rtf"     "Rich Text Format")
    ("textile"           ".textile" "Textile")
    ("org"               ".org"     "Org-mode")
    ("json"              ".json"    "JSON")
    ("asciidoc"          ".txt"     "AsciiDoc")
    ("opml"              ".opml"    "OPML"))
  "List of Pandoc output formats and their associated file extensions.
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
  '(("footnotes"                           ("markdown" "markdown_phpextra"))
    ("inline_notes"                        ("markdown"))
    ("pandoc_title_block"                  ("markdown"))
    ("mmd_title_block"                     ())
    ("table_captions"                      ("markdown"))
    ("implicit_figures"                    ("markdown"))
    ("simple_tables"                       ("markdown"))
    ("multiline_tables"                    ("markdown"))
    ("grid_tables"                         ("markdown"))
    ("pipe_tables"                         ("markdown" "markdown_phpextra" "markdown_github"))
    ("citations"                           ("markdown"))
    ("raw_tex"                             ("markdown"))
    ("raw_html"                            ("markdown" "markdown_phpextra" "markdown_github"))
    ("tex_math_dollars"                    ("markdown"))
    ("tex_math_single_backslash"           ("markdown_github"))
    ("tex_math_double_backslash"           ())
    ("latex_macros"                        ("markdown"))
    ("fenced_code_blocks"                  ("markdown" "markdown_phpextra" "markdown_github"))
    ("fenced_code_attributes"              ("markdown" "markdown_github"))
    ("backtick_code_blocks"                ("markdown" "markdown_github"))
    ("inline_code_attributes"              ("markdown"))
    ("markdown_in_html_blocks"             ("markdown"))
    ("markdown_attribute"                  ("markdown_phpextra"))
    ("escaped_line_breaks"                 ("markdown"))
    ("link_attributes"                     ())
    ("autolink_bare_uris"                  ("markdown_github"))
    ("fancy_lists"                         ("markdown"))
    ("startnum"                            ("markdown"))
    ("definition_lists"                    ("markdown" "markdown_phpextra"))
    ("example_lists"                       ("markdown"))
    ("all_symbols_escapable"               ("markdown"))
    ("intraword_underscores"               ("markdown" "markdown_phpextra" "markdown_github"))
    ("blank_before_blockquote"             ("markdown"))
    ("blank_before_header"                 ("markdown"))
    ("strikeout"                           ("markdown" "markdown_github"))
    ("superscript"                         ("markdown"))
    ("subscript"                           ("markdown"))
    ("hard_line_breaks"                    ("markdown_github"))
    ("abbreviations"                       ("markdown_phpextra"))
    ("auto_identifiers"                    ("markdown"))
    ("header_attributes"                   ("markdown" "markdown_phpextra"))
    ("mmd_header_identifiers"              ())
    ("implicit_header_references"          ("markdown"))
    ("line_blocks"                         ("markdown"))
    ("ignore_line_breaks"                  ())
    ("yaml_metadata_block"                 ("markdown"))
    ("ascii_identifiers"                   ("markdown_github"))
    ("lists_without_preceding_blankline"   ("markdown_github")))
  "List of Markdown extensions supported by Pandoc.")

(defvar pandoc-cli-options nil
  "List of Pandoc command-line options that do not need special treatment.
This includes all command-line options except the list and alist
options, because they need to be handled separately in
`pandoc-format-all-options'.")

(defvar pandoc-filepath-options
  '(data-dir)
  "List of options that have a file path as value.
These file paths are expanded before they are sent to pandoc. For
relative paths, the file's working directory is used as base
directory. One option is preset, others are added by
`define-pandoc-file-option'.")

(defvar pandoc-binary-options nil
  "List of binary options.
These are set by `define-pandoc-binary-option'.")

(defvar pandoc-list-options nil
  "List of options that have a list as value.
These are set by `define-pandoc-list-option'.")

(defvar pandoc-alist-options nil
  "List of options that have an alist as value.
These are set by `define-pandoc-alist-option'.")

(defvar pandoc-options
  `((read)
    (read-lhs)
    (read-extensions ,@(mapcar 'list (sort (mapcar 'car pandoc-extensions) 'string<)))
    (write . "native")
    (write-lhs)
    (write-extensions ,@(mapcar 'list (sort (mapcar 'car pandoc-extensions) 'string<)))
    (output)
    (data-dir)
    (output-dir)) ; this is not actually a pandoc option
  "Pandoc option alist.
List of options and their default values. For each buffer in
which pandoc-mode is activated, a buffer-local copy of this list
is made that stores the local values of the options. The
`define-pandoc-*-option' functions add their options to this list
with the default value NIL.")

(defvar pandoc-local-settings nil "A buffer-local variable holding a file's pandoc options.")
(make-variable-buffer-local 'pandoc-local-settings)

(defvar pandoc-settings-modified-flag nil "T if the current settings were modified and not saved.")
(make-variable-buffer-local 'pandoc-settings-modified-flag)

(defvar pandoc-output-buffer (get-buffer-create " *Pandoc output*"))

(defvar pandoc-options-menu nil
  "Auxiliary variable for creating the options menu.")

(defvar pandoc-files-menu nil
  "Auxiliary variable for creating the file menu.")

(defmacro with-pandoc-output-buffer (&rest body)
  "Execute BODY with `pandoc-output-buffer' temporarily current.
Make sure that `pandoc-output-buffer' really exists."
  (declare (indent defun))
  `(progn
     (or (buffer-live-p pandoc-output-buffer)
         (setq pandoc-output-buffer (get-buffer-create " *Pandoc output*")))
     (with-current-buffer pandoc-output-buffer
       ,@body)))

(defmacro define-pandoc-binary-option (option description)
  "Create a binary option.
OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). DESCRIPTION is the
description of the option as it will appear in the menu."
  (declare (indent defun))
  `(progn
     (add-to-list 'pandoc-binary-options (cons ,description (quote ,option)) t)
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-options (list (quote ,option))) t))

(defmacro define-pandoc-file-option (option prompt &optional full-path default)
  "Define a file option.
The option is added to `pandoc-options', `pandoc-cli-options',
and to `pandoc-filepath-options' (unless FULL-PATH is NIL).
Furthermore, a menu entry is created and a function to set/unset
the option.

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
     ,(when full-path
        `(add-to-list 'pandoc-filepath-options (quote ,option) t))
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-files-menu
                  (list ,@(delq nil ; if DEFAULT is nil, we need to remove it from the list.
                                (list prompt
                                      (vector (concat "No " prompt) `(pandoc-set (quote ,option) nil)
                                              :active t
                                              :style 'radio
                                              :selected `(null (pandoc-get (quote ,option))))
                                      (when default
                                        (vector (concat "Default " prompt) `(pandoc-set (quote ,option) t)
                                                :active t
                                                :style 'radio
                                                :selected `(eq (pandoc-get (quote ,option)) t)))
                                      (vector (concat "Set " prompt "...") (intern (concat "pandoc-set-"
                                                                                           (symbol-name option)))
                                              :active t
                                              :style 'radio
                                              :selected `(stringp (pandoc-get (quote ,option)))))))
                  t) ; add to the end of `pandoc-options-menu'
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (pandoc-set (quote ,option)
                           (cond
                            ((eq prefix '-) nil)
                            ((null prefix) ,(if full-path
                                                `(read-file-name ,(concat prompt ": "))
                                              `(file-name-nondirectory (read-file-name ,(concat prompt ": ")))))
                            (t ,default)))))))

(defmacro define-pandoc-numeric-option (option prompt)
  "Define a numeric option.
The option is added to `pandoc-options' and to
`pandoc-cli-options'. Furthermore, a menu entry is created and a
function to set/unset the option.

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
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-options-menu
                  (list ,prompt
                        ,(vector (concat "Default " prompt) `(pandoc-set (quote ,option) nil)
                                 :active t
                                 :style 'radio
                                 :selected `(null (pandoc-get (quote ,option))))
                        ,(vector (concat "Set " prompt "...") (intern (concat "pandoc-set-" (symbol-name option)))
                                 :active t
                                 :style 'radio
                                 :selected `(pandoc-get (quote ,option))))
                  t) ; add to the end of `pandoc-options-menu'
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (pandoc-set (quote ,option)
                           (if (eq prefix '-)
                               nil
                             (string-to-number (read-string ,(concat prompt ": ")))))))))

(defmacro define-pandoc-string-option (option prompt &optional default)
  "Define a option whose value is a string.
The option is added to `pandoc-options' and to
`pandoc-cli-options'. Furthermore, a menu entry is created and a
function to set the option.

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
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-options-menu
                  (list ,@(delq nil ; if DEFAULT is nil, we need to remove it from the list.
                                (list prompt
                                      (vector (concat "No " prompt) `(pandoc-set (quote ,option) nil)
                                              :active t
                                              :style 'radio
                                              :selected `(null (pandoc-get (quote ,option))))
                                      (when default
                                        (vector (concat "Default " prompt) `(pandoc-set (quote ,option) t)
                                                :active t
                                                :style 'radio
                                                :selected `(eq (pandoc-get (quote ,option)) t)))
                                      (vector (concat "Set " prompt "...") (intern (concat "pandoc-set-" (symbol-name option)))
                                              :active t
                                              :style 'radio
                                              :selected `(stringp (pandoc-get (quote ,option)))))))
                  t) ; add to the end of `pandoc-options-menu'
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (pandoc-set (quote ,option)
                           (cond
                            ((eq prefix '-) nil)
                            ((null prefix) (read-string ,(concat prompt ": ")))
                            (t ,default)))))))

(defmacro define-pandoc-list-option (option type description prompt)
  "Define an option whose value is a list.
The option is added to `pandoc-options' and
`pandoc-list-options'. Furthermore, a menu entry is created and a
function to set the option. This function can also be called with
the prefix argument C-u - (or M--) to unset the option.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). TYPE specifies the kind of
data that is stored in the list. Currently, possible values are
'string and 'file. DESCRIPTION is the description for the
option's submenu. PROMPT is a string that is used to prompt for
setting and unsetting the option. It must be formulated in such a
way that the strings \"Add \", \"Remove \" can be added before
it."
  `(progn
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-list-options (quote ,option) t)
     (add-to-list (quote ,(if (eq type 'string)
                              'pandoc-options-menu
                            'pandoc-files-menu))
                  (list ,description
                        ,(vector (concat "Add " prompt) (intern (concat "pandoc-set-" (symbol-name option)))
                                 :active t)
                        ,(vector (concat "Remove " prompt) (list (intern (concat "pandoc-set-" (symbol-name option))) `(quote -))
                                 :active `(pandoc-get (quote ,option))))
                  t)              ; add to the end of `pandoc-{options|files}-menu'
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (if (eq prefix '-)
                   (let ((value (completing-read "Remove item: " (pandoc-get (quote ,option)) nil t)))
                     (pandoc-remove-from-list-option (quote ,option) value)
                     (message ,(concat prompt " \"%s\" removed.") value))
                 (let ((value ,(cond
                                ((eq type 'string)
                                 `(read-string "Add value: " nil nil (pandoc-get (quote ,option))))
                                ((eq type 'file)
                                 `(read-file-name "Add file: ")))))
                   (pandoc-set (quote ,option) value)
                   (message ,(concat prompt " \"%s\" added.") value)))))))

(defmacro define-pandoc-alist-option (option type description prompt)
  "Define an option whose value is an alist.
The option is added to `pandoc-options' and
`pandoc-alist-options'. Furthermore, a menu entry is created and
a function to set the option. This function can also be called
with the prefix argument C-u - (or M--) to unset the option.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). TYPE specifies the kind of
data that is stored in the list. Currently, possible values are
'string and 'file. DESCRIPTION is the description for the
option's submenu. PROMPT is a string that is used to prompt for
setting and unsetting the option. It must be formulated in such a
way that the strings \"Set/Change \" and \"Unset \" can be added
before it."
  `(progn
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-alist-options (quote ,option) t)
     (add-to-list (quote ,(if (eq type 'string)
                              'pandoc-options-menu
                            'pandoc-files-menu))
                  (list ,description
                        ,(vector (concat "Set/Change " prompt) (intern (concat "pandoc-set-" (symbol-name option)))
                                 :active t)
                        ,(vector (concat "Unset " prompt) (list (intern (concat "pandoc-set-" (symbol-name option))) `(quote -))
                                 :active t))
                  t)              ; add to the end of `pandoc-options-menu'
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (let ((var (nonempty (completing-read (concat ,prompt ": ") (pandoc-get (quote ,option))))))
                 (when var
                   (let ((value (if (eq prefix '-)
                                    nil
                                  ,(cond
                                    ((eq type 'string)
                                     `(read-string "Value: " nil nil (cdr (assq var (pandoc-get (quote ,option))))))
                                    ((eq type 'file)
                                     `(read-file-name "File: "))))))
                     ,(when (eq type 'string) ;; strings may be empty (which corresponds to boolean True in Pandoc)
                        '(when (string= value "")
                           (setq value t)))
                     (pandoc-set (quote ,option) (cons var value))
                     (message ,(concat prompt " `%s' \"%s\".") var (if value
                                                                 (format "added with value `%s'" value)
                                                               "removed")))))))))

(defmacro define-pandoc-choice-option (option prompt choices output-formats)
  "Define an option whose value is a choice between several items.
The option is added to `pandoc-options' and `pandoc-cli-options'.
Furthermore, a menu entry is created and a function to set the
option.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes). PROMPT is a string that is
used to prompt for setting and unsetting the option and is also
used in the menu. CHOICES is the list of choices, which must be
strings. The first of these is the default value, i.e., the one
that Pandoc uses if the option is unspecified. OUTPUT-FORMATS is
a list of output formats for which OPTION should be active in the
menu."
  `(progn
     (add-to-list 'pandoc-options (list (quote ,option)) t)
     (add-to-list 'pandoc-cli-options (quote ,option) t)
     (add-to-list 'pandoc-options-menu (list ,prompt
                                             :active (quote (member (pandoc-get 'write) (quote ,output-formats)))
                                             ,(vector (car choices) `(pandoc-set (quote ,option) ,(car choices))
                                                      :style 'radio
                                                      :selected `(null (pandoc-get (quote ,option))))
                                             ,@(mapcar #'(lambda (choice)
                                                           (vector choice `(pandoc-set (quote ,option) ,choice)
                                                                   :style 'radio
                                                                   :selected `(string= (pandoc-get (quote ,option)) ,choice)))
                                                       (cdr choices)))
                  t)              ; add to the end of `pandoc-options-menu'
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           #'(lambda (prefix)
               (interactive "P")
               (pandoc-set (quote ,option)
                           (if (eq prefix '-)
                               nil
                             (let ((value (completing-read ,(format "Set %s: " prompt) (quote ,choices) nil t)))
                               (if (or (not value)
                                       (member value '("" (car ,choices))))
                                   nil
                                 value))))))))

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
    (define-key map "\C-c/c" 'pandoc-insert-@)
    (define-key map "\C-c/C" 'pandoc-select-@)
    (define-key map "\C-c/m" 'pandoc-set-metadata)
    (define-key map "\C-c/p" 'pandoc-convert-to-pdf)
    (define-key map "\C-c/r" 'pandoc-run-pandoc)
    (define-key map "\C-c/s" 'pandoc-save-settings-file)
    (define-key map "\C-c/S" 'pandoc-view-settings)
    (define-key map "\C-c/v" 'pandoc-set-variable)
    (define-key map "\C-c/V" 'pandoc-view-output)
    (define-key map "\C-c/w" 'pandoc-set-write)
    map)
  "Keymap for pandoc-mode.")

;;;###autoload
(define-minor-mode pandoc-mode
  "Minor mode for interacting with Pandoc."
  :init-value nil :lighter (:eval (concat " Pandoc/" (pandoc-get 'write))) :global nil
  (cond
   (pandoc-mode    ; pandoc-mode is turned on
    (setq pandoc-local-settings (copy-tree pandoc-options))
    (pandoc-set 'read (cdr (assq major-mode pandoc-major-modes)))
    (setq pandoc-settings-modified-flag nil)
    (or (buffer-live-p pandoc-output-buffer)
        (setq pandoc-output-buffer (get-buffer-create " *Pandoc output*"))))
   ((not pandoc-mode)    ; pandoc-mode is turned off
    (setq pandoc-local-settings nil
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

(defun pandoc-get (option &optional buffer)
  "Returns the value of OPTION.
Optional argument BUFFER is the buffer from which the value is to
be retrieved."
  (cdr (assq option (buffer-local-value 'pandoc-local-settings (or buffer (current-buffer))))))

;; TODO list options aren't set correctly.
(defun pandoc-set (option value)
  "Sets the local value of OPTION to VALUE.
If OPTION is 'variable, VALUE should be a cons of the
form (variable-name . value), which is then added to the
variables already stored, or just (variable-name), in which case
the named variable is deleted from the list."
  (when (assq option pandoc-options) ; check if the option is licit
    (cond
     ((memq option pandoc-alist-options)
      (pandoc-set-alist-option option value))
     ((memq option pandoc-list-options)
      (pandoc-set-list-option option value))
     ((eq option 'read-extensions)
      (pandoc-set-extension (car value) 'read (cdr value)))
     ((eq option 'write-extensions)
      (pandoc-set-extension (car value) 'write (cdr value)))
     (t (setcdr (assq option pandoc-local-settings) value)))
    (setq pandoc-settings-modified-flag t)))

(defun pandoc-set-alist-option (option new-elem)
  "Set an alist option.
NEW-ELEM is a cons (<name> . <value>), which is added to the alist
for OPTION in `pandoc-local-settings'. If an element with <name>
already exists, it is replaced, or removed if <value> is NIL."
  (let* ((value (cdr new-elem))
         (items (pandoc-get option)) 
         (item (assoc (car new-elem) items)))
    (cond
     ((and item value) ; if <name> exists and we have a new value
      (setcdr item value)) ; replace the existing value
     ((and item (not value)) ; if <name> exists but we have no new value
      (setq items (delq item items))) ; remove <name>
     ((and (not item) value) ; if <name> does not exist
      (setq items (cons new-elem items)))) ; add it
    (setcdr (assoc option pandoc-local-settings) items)))    

(defun pandoc-set-list-option (option value)
  "Add VALUE to list option OPTION."
  (let* ((values (pandoc-get option))
         (new-values (cons value values)))
    (setcdr (assoc option pandoc-local-settings) new-values)))

(defun pandoc-remove-from-list-option (option value)
  "Remove VALUE from the list of OPTION."
  (let* ((values (pandoc-get option))
         (new-values (remove value values)))
    (setcdr (assoc option pandoc-local-settings) new-values)))

(defun pandoc-toggle (option)
  "Toggles the value of a switch."
  (pandoc-set option (not (pandoc-get option))))

;; Note: the extensions appear to be binary options, but they are not:
;; they're really (balanced) ternary options. They can be on or off, but
;; that doesn't tell us whether they're on or off because the user set them
;; that way or because that's the default setting for the relevant format.
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
             (pandoc-extension-in-format-p extension (pandoc-get rw) rw)))))

(defun pandoc-set-extension (extension rw value)
  "Set the value of EXTENSION for RW to VALUE.
RW is either 'read or 'write, indicating whether the read or
write extension is to be set."
  (setcdr (assoc extension (if (eq rw 'read)
                               (pandoc-get 'read-extensions)
                             (pandoc-get 'write-extensions)))
          value))

(defun pandoc-get-extension (extension rw)
  "Return the value of EXTENSION for RW.
RW is either 'read or 'write, indicating whether the read or
write extension is to be queried."
  (cdr (assoc extension (if (eq rw 'read)
                            (pandoc-get 'read-extensions)
                          (pandoc-get 'write-extensions)))))

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
                     ((pandoc-extension-in-format-p extension (pandoc-get rw) rw) ; if the extension is part of the current format
                      '-)  ; we explicitly unset it
                     (t '+)))) ; otherwise we explicitly set it
    (pandoc-set-extension extension rw new-value)))

(defun pandoc-create-settings-filename (type filename output-format)
  "Create a settings filename.
TYPE is the type of settings file, either 'settings or 'project.
The return value is an absolute filename."
  (setq filename (expand-file-name filename))
  (cond
   ((eq type 'settings)
    (concat (file-name-directory filename) "." (file-name-nondirectory filename) "." output-format ".pandoc"))
   ((eq type 'project)
    (concat (file-name-directory filename) "Project." output-format ".pandoc"))))

(defun pandoc-create-global-settings-filename (format)
  "Create a global settings filename.
FORMAT is the output format to use."
  (concat (file-name-as-directory pandoc-data-dir) format ".pandoc"))

(defun pandoc-format-all-options (input-file &optional pdf)
  "Create a list of strings with pandoc options for the current buffer.
INPUT-FILE is the name of the input file. If PDF is non-nil, an
output file is always set, derived either from the input file or
from the output file set for the \"latex\" output profile, and
gets the suffix `.pdf'. If the output format is \"odt\", \"epub\"
or \"docx\" but no output file is specified, one will be created,
since pandoc does not support output to stdout for those two
formats."
  (let ((read (format "--read=%s%s%s" (pandoc-get 'read) (if (pandoc-get 'read-lhs) "+lhs" "")
                      (pandoc-format-extensions (pandoc-get 'read-extensions))))
        (write (if pdf
                   nil
                 (format "--write=%s%s%s" (pandoc-get 'write) (if (pandoc-get 'write-lhs) "+lhs" "")
                         (pandoc-format-extensions (pandoc-get 'write-extensions)))))
        (output (pandoc-format-output-option input-file pdf))
        (list-options (mapcar #'(lambda (option)
                                  (pandoc-format-list-options option (pandoc-get option)))
                              pandoc-list-options))
        (alist-options (mapcar #'(lambda (option)
                                   (pandoc-format-alist-options option (pandoc-get option)))
                               pandoc-alist-options))
        (cli-options (pandoc-format-cli-options)))
    ;; Note: list-options and alist-options are both lists of lists, so we need to flatten them first.
    (delq nil (append (list read write output) cli-options (apply #'append list-options) (apply #'append alist-options)))))

(defun pandoc-format-extensions (extensions)
  "Create a string of extensions to be added to the Pandoc command line."
  (mapconcat #'(lambda (elt)
                 (if (cdr elt)
                     (format "%s%s" (cdr elt) (car elt))
                   ""))
             extensions
             ""))

(defun pandoc-format-output-option (input-file pdf)
  "Create the output option for calling Pandoc.
Return a string that can be added to the call to Pandoc."
  (cond
   ((or (eq (pandoc-get 'output) t) ; if the user set the output file to T
        (and (null (pandoc-get 'output)) ; or if the user set no output file but either
             (or pdf                    ; (i) we're converting to pdf, or
                 (member (pandoc-get 'write) ; (ii) the output format is odt, epub or docx
                         '("odt" "epub" "docx")))))
    (format "--output=%s/%s%s"          ; we create an output file name.
            (expand-file-name (or (pandoc-get 'output-dir)
                                  (file-name-directory input-file)))
            (file-name-sans-extension (file-name-nondirectory input-file))
            (if pdf
                ".pdf"
              (cdr (assoc (pandoc-get 'write) pandoc-output-formats-list)))))
   ((stringp (pandoc-get 'output)) ; if the user set an output file,
    (format "--output=%s/%s"      ; we combine it with the output directory
            (expand-file-name (or (pandoc-get 'output-dir)
                                  (file-name-directory input-file)))
            (if pdf                  ; and check if we're converting to pdf
                (concat (file-name-sans-extension (pandoc-get 'output)) ".pdf")
              (pandoc-get 'output))))
   (t nil)))

(defun pandoc-format-list-options (option values)
  "Create a list of cli options for OPTION from the values in VALUES."
  (mapcar #'(lambda (value)
              (format "--%s=%s" option value))
          values))

(defun pandoc-format-alist-options (option alist)
  "Create a list of cli options for OPTION from the key-value pairs in ALIST."
  (mapcar #'(lambda (kv)
              (let ((key (car kv))
                    (value (cdr kv)))
                (format "--%s=%s%s" option key
                        (if (eq value t)
                            ""
                          (format ":%s" value)))))
          alist))

(defun pandoc-format-cli-options ()
  "Create a list of options in `pandoc-cli-options'."
  (mapcar #'(lambda (option)
              (let ((value (pandoc-get option)))
                (when (and value (memq option pandoc-filepath-options))
                  (setq value (expand-file-name value)))
                (cond
                 ((eq value t) (format "--%s" option))
                 ((or (numberp value)
                      (stringp value)) (format "--%s=%s" option value))
                 (t nil))))
          pandoc-cli-options))

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
format is used. If PDF is non-NIL, a pdf file is created."
  (let ((filename (buffer-file-name buffer)))
    ;; we do this in a temp buffer so we can process @@-directives without
    ;; having to undo them and set the options independently of the
    ;; original buffer.
    (with-temp-buffer
      (if (and output-format ; if an output format was provided (and the buffer is visiting a file)
               filename)     ; we want to use settings for that format or no settings at all.
          (unless (pandoc-load-settings-for-file (expand-file-name filename) output-format t)
            ;; if we do not find a settings file, we unset all options:
            (setq pandoc-local-settings (copy-tree pandoc-options))
            ;; and specify only the input and output formats:
            (pandoc-set 'write output-format)
            (pandoc-set 'read (pandoc-get 'read buffer)))
        ;; if no output format was provided, we use the buffer's options:
        (setq pandoc-local-settings (buffer-local-value 'pandoc-local-settings buffer)))
      (let ((option-list (pandoc-format-all-options filename pdf)))
        (insert-buffer-substring-no-properties buffer)
        (message "Running pandoc...")
        (pandoc-process-directives (pandoc-get 'write))
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
    (let ((current-settings-file
           (file-name-nondirectory (pandoc-create-settings-filename 'settings (buffer-file-name)
                                                                    (pandoc-get 'write))))
          (current-project-file
           (file-name-nondirectory (pandoc-create-settings-filename 'project (buffer-file-name)
                                                                    (pandoc-get 'write)))))
      (when (not (file-exists-p current-settings-file))
        (pandoc-save-settings 'settings (pandoc-get 'write)))
      (make-symbolic-link current-settings-file
                          (pandoc-create-settings-filename 'settings (buffer-file-name) "default") t)
      (when (file-exists-p current-project-file)
          (make-symbolic-link current-project-file
                              (pandoc-create-settings-filename 'project (buffer-file-name) "default") t))
      (message "`%s' set as default output format." (pandoc-get 'write)))))

(defun pandoc-save-settings-file ()
  "Save the settings of the current buffer.
This function just calls pandoc-save-settings with the
appropriate output format."
  (interactive)
  (pandoc-save-settings 'settings (pandoc-get 'write)))

(defun pandoc-save-project-file ()
  "Save the current settings as a project file."
  (interactive)
  (pandoc-save-settings 'project (pandoc-get 'write)))

(defun pandoc-save-global-settings-file ()
  "Save the current settings to a global settings file."
  (interactive)
  (pandoc-save-settings 'global (pandoc-get 'write)))

(defun pandoc-save-settings (type format &optional no-confirm)
  "Save the settings of the current buffer for FORMAT.
TYPE must be a quoted symbol and specifies the type of settings
file. If its value is 'settings, a normal settings file is
created for the current file. If TYPE's value is 'project, a
project settings file is written. If optional argument NO-CONFIRM
is non-nil, any existing settings file is overwritten without
asking."
  (let* ((filename (buffer-file-name))
         (settings pandoc-local-settings)
         (settings-file (if (eq type 'global)
                            (pandoc-create-global-settings-filename format)
                          (pandoc-create-settings-filename type filename format))))
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
      (setq pandoc-settings-modified-flag nil))))

(defun pandoc-revert-settings ()
  "Revert settings for the current buffer.
The settings file is reread from disk, so that any changes made
to the settings that have not been saved are reverted."
  (interactive)
  (let ((format (pandoc-get 'write)))
    (setq pandoc-local-settings (copy-tree pandoc-options))
    (pandoc-load-settings-profile format 'no-confirm)))

(defun pandoc-load-default-settings ()
  "Load the default settings of the file in the current buffer.
This function is for use in `pandoc-mode-hook'."
  (pandoc-load-settings-profile "default"))

(defun pandoc-load-settings-profile (format &optional no-confirm)
  "Load the options for FORMAT from the corresponding settings file.
If NO-CONFIRM is t, no confirmation is asked if the current
settings have not been saved."
  (when (buffer-file-name)
    (pandoc-load-settings-for-file (expand-file-name (buffer-file-name)) format no-confirm)))

(defvar pandoc-counter) ; We use this to keep track of which kind of settings file is being read.

(defun pandoc-load-settings-for-file (file format &optional no-confirm)
  "Load the settings for FILE.
Load FILE's own settings file if it exists, otherwise check for a
project file and load that. If NO-CONFIRM is t, no confirmation
is asked if the current settings have not been saved. FILE must
be an absolute path name. The settings are stored in the current
buffer's `pandoc-local-settings'. Returns NIL if no settings or
project file is found for FILE, otherwise non-NIL."
  (when (and (not no-confirm)
             pandoc-settings-modified-flag
             (y-or-n-p (format "Current settings for format \"%s\" modified. Save first? " (pandoc-get 'write))))
    (pandoc-save-settings 'settings (pandoc-get 'write) t))
  (let* ((pandoc-counter -1)
         (settings (or (pandoc-read-settings-from-file (pandoc-create-settings-filename 'settings file format))
                       (pandoc-read-settings-from-file (pandoc-create-settings-filename 'project file format))
                       (pandoc-read-settings-from-file (pandoc-create-global-settings-filename format)))))
    (when settings
      (setq pandoc-local-settings settings)
      (message "%s settings file loaded for format \"%s\"." (nth pandoc-counter '("Local" "Project" "Global")) format))))

(defun pandoc-read-settings-from-file (file)
  "Read the settings in FILE and return them.
If FILE does not exist or cannot be read, return NIL."
  (setq pandoc-counter (1+ pandoc-counter)) ; Increase our file type counter.
  (if (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (looking-at "#") ; We're probably dealing with an old settings file.
            (pandoc-read-old-settings-from-buffer)
          (let ((flist (when (search-forward "(" nil t)
                         (forward-char -1)
                         (read (current-buffer)))))
            (if (listp flist)
                flist))))))

(defun pandoc-read-old-settings-from-buffer ()
  "Read old-style settings from the current buffer.
`pandoc-settings-modified-flag' is set, so that the user will be
asked to save the settings on exit. Return an alist with the
options and their values."
  (goto-char (point-min))
  (let (options)                        ; we collect the options in a list
    (while (re-search-forward "^\\([a-z-]*\\)::\\(.*?\\)$" nil t)
      (let ((option (intern (match-string 1)))
            (value (match-string 2)))
        ;; If the option is a variable or extension, we read its name and
        ;; value and add them to the alist as a dotted list.
        (add-to-list 'options (if (memq option '(variable read-extensions write-extensions))
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
                                              (t value)))))))
    ;; `options' isn't in the proper format for pandoc-local-settings yet:
    ;; there may be multiple variables and extensions in it. Since we're in
    ;; a temp buffer, we can simply use pandoc-set to set all options and
    ;; then return the local value of `pandoc-local-settings'.
    (setq pandoc-local-settings (copy-tree pandoc-options))
    (mapc #'(lambda (option)
              (pandoc-set (car option) (cdr option)))
          options)
    pandoc-local-settings))

(defun pandoc-view-output ()
  "Displays the *Pandoc output* buffer."
  (interactive)
  (display-buffer pandoc-output-buffer))

(defun pandoc-view-settings ()
  "Displays the settings file in the *Pandoc output* buffer."
  (interactive)
  ;; remove all options that do not have a value.
  (let* ((remove-defaults #'(lambda (alist)
                              (delq nil (mapcar #'(lambda (option)
                                                    (if (cdr option)
                                                        option))
                                                alist))))
         (settings (copy-tree pandoc-local-settings))
         (read-extensions (assq 'read-extensions settings))
         (write-extensions (assq 'write-extensions settings)))
    (setcdr read-extensions (funcall remove-defaults (cdr read-extensions)))
    (setcdr write-extensions (funcall remove-defaults (cdr write-extensions)))
    (setq settings (funcall remove-defaults settings))
    (with-current-buffer pandoc-output-buffer
      (let ((print-length nil)
            (print-level nil)
            (print-circle nil))
        (erase-buffer)
        (pp settings (current-buffer))))
    (display-buffer pandoc-output-buffer)))

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
             (y-or-n-p (format "Current settings for output format \"%s\" changed. Save? " (pandoc-get 'write))))
    (pandoc-save-settings 'settings (pandoc-get 'write) t))
  (unless (pandoc-load-settings-profile format t)
    (setq pandoc-local-settings (copy-tree pandoc-options))
    (pandoc-set 'write format)
    (pandoc-set 'read (cdr (assq major-mode pandoc-major-modes))))
  (setq pandoc-settings-modified-flag nil))

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

(define-pandoc-file-option template "Template File" t)
(define-pandoc-file-option css "CSS Style Sheet")
(define-pandoc-file-option reference-odt "Reference ODT File" t)
(define-pandoc-file-option reference-docx "Reference docx File" t)
(define-pandoc-file-option epub-metadata "EPUB Metadata File" t)
(define-pandoc-file-option epub-stylesheet "EPUB Style Sheet" t t)
(define-pandoc-file-option epub-cover-image "EPUB Cover Image" t)
(define-pandoc-file-option csl "CSL File" t)
(define-pandoc-file-option citation-abbreviations "Citation Abbreviations File" t)
(define-pandoc-file-option include-in-header "Include Header" t)
(define-pandoc-file-option include-before-body "Include Before Body" t)
(define-pandoc-file-option include-after-body "Include After Body" t)

(define-pandoc-list-option epub-embed-font file "EPUB Fonts" "EPUB Embedded Font")
(define-pandoc-list-option filter file "Filters" "Filter")
(define-pandoc-list-option bibliography file "Bibliography Files" "Bibliography File")

(define-pandoc-alist-option variable string "Variables" "Variable")
(define-pandoc-alist-option metadata string "Metadata" "Metadata item")

(define-pandoc-choice-option latex-engine "LaTeX Engine"
  ("pdflatex" "xelatex" "lualatex")
  ("latex" "beamer" "context"))
(define-pandoc-choice-option email-obfuscation "Email Obfuscation"
  ("none" "javascript" "references")
  ("html" "html5" "s5" "slidy" "slideous" "dzslides" "revealjs"))

(define-pandoc-numeric-option columns "Column Width")
(define-pandoc-numeric-option tab-stop "Tab Stop Width")
(define-pandoc-numeric-option base-header-level "Base Header Level")
(define-pandoc-numeric-option slide-level "Slide Level Header")
(define-pandoc-numeric-option toc-depth "TOC Depth")
(define-pandoc-numeric-option epub-chapter-level "EPub Chapter Level")

(define-pandoc-string-option latexmathml "LaTeXMathML URL" t)
(define-pandoc-string-option mathml "MathML URL" t)
(define-pandoc-string-option mimetex "MimeTeX CGI Script" t)
(define-pandoc-string-option webtex "WebTeX URL" t)
(define-pandoc-string-option jsmath "jsMath URL" t)
(define-pandoc-string-option mathjax "MathJax URL")
(define-pandoc-string-option title-prefix "Title prefix")
(define-pandoc-string-option id-prefix "ID prefix")
(define-pandoc-string-option indented-code-classes "Indented Code Classes")
(define-pandoc-string-option highlight-style "Highlighting Style")
(define-pandoc-string-option number-offset "Number Offsets")
(define-pandoc-string-option default-image-extension "Default Image Extension")

(define-pandoc-binary-option standalone "Standalone")
(define-pandoc-binary-option preserve-tabs "Preserve Tabs")
(define-pandoc-binary-option strict "Strict")
(define-pandoc-binary-option normalize "Normalize Document")
(define-pandoc-binary-option reference-links "Reference Links")
(define-pandoc-binary-option parse-raw "Parse Raw")
(define-pandoc-binary-option smart "Smart")
(define-pandoc-binary-option gladtex "gladTeX")
(define-pandoc-binary-option incremental "Incremental")
(define-pandoc-binary-option self-contained "Self-contained Document")
(define-pandoc-binary-option chapters "Top-level Headers Are Chapters")
(define-pandoc-binary-option number-sections "Number Sections")
(define-pandoc-binary-option listings "Use LaTeX listings Package")
(define-pandoc-binary-option section-divs "Wrap Sections in <div> Tags")
(define-pandoc-binary-option no-wrap "No Wrap")
(define-pandoc-binary-option no-highlight "No Highlighting")
(define-pandoc-binary-option table-of-contents "Table of Contents")
(define-pandoc-binary-option natbib "Use NatBib")
(define-pandoc-binary-option biblatex "Use BibLaTeX")
(define-pandoc-binary-option ascii "Use Only ASCII in HTML")
(define-pandoc-binary-option atx-headers "Use ATX-style Headers")
(define-pandoc-binary-option old-dashes "Use Old-style Dashes")
(define-pandoc-binary-option no-tex-ligatures "Do Not Use TeX Ligatures")
(define-pandoc-binary-option html-q-tags "Use <q> Tags for Quotes in HTML")

(defun pandoc-toggle-interactive (prefix)
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
                                              pandoc-binary-options nil t) pandoc-binary-options))))
    (pandoc-set option (cond
                        ((eq prefix '-) nil)
                        ((null prefix) (not (pandoc-get option)))
                        (t t)))
    (message "Option `%s' %s." (car (rassq option pandoc-binary-options)) (if (pandoc-get option)
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
                   (mapcar #'(lambda (option)
                               (vector (car option)
                                       `(pandoc-set 'read ,(cdr option))
                                       :active t
                                       :style 'radio
                                       :selected `(string= (pandoc-get 'read)
                                                           ,(cdr option))))
                           pandoc-input-formats-menu))
             (list ["Literal Haskell" (pandoc-toggle 'read-lhs)
                    :active (member (pandoc-get 'read) '("markdown" "rst" "latex"))
                    :style toggle :selected (pandoc-get 'read-lhs)])
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc-get 'read)))
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
                                       :selected `(string= (pandoc-get 'write)
                                                           ,(cdr option))))
                           pandoc-output-formats-menu))
             (list ["Literal Haskell" (pandoc-toggle 'write-lhs)
                    :active (member (pandoc-get 'write)
                                    '("markdown" "rst" "latex" "beamer" "html" "html5"))
                    :style toggle :selected (pandoc-get 'write-lhs)])
             (list (append (list "Extensions" :visible `(string-match "markdown" (pandoc-get 'write)))
                           (mapcar #'(lambda (ext)
                                       (vector (car ext)
                                               `(pandoc-toggle-extension ,(car ext) 'write)
                                               :active t
                                               :style 'toggle
                                               :selected `(pandoc-extension-active-p ,(car ext) 'write)))
                                   pandoc-extensions))))

    ("Files"
     ("Output File"
      ["Output To Stdout" (pandoc-set 'output nil) :active t
       :style radio :selected (null (pandoc-get 'output))]
      ["Create Output Filename" (pandoc-set 'output t) :active t
       :style radio :selected (eq (pandoc-get 'output) t)]
      ["Set Output File..." pandoc-set-output :active t
       :style radio :selected (stringp (pandoc-get 'output))])
     ("Output Directory"
      ["Use Input Directory" (pandoc-set 'output-dir nil) :active t
       :style radio :selected (null (pandoc-get 'output-dir))]
      ["Set Output Directory" pandoc-set-output-dir :active t
       :style radio :selected (pandoc-get 'output-dir)])
     ("Data Directory"
      ["Use Default Data Directory" (pandoc-set 'data-dir nil) :active t
       :style radio :selected (null (pandoc-get 'data-dir))]
      ["Set Data Directory" pandoc-set-data-dir :active t
       :style radio :selected (pandoc-get 'data-dir)])
     ,@pandoc-files-menu)

    ("Options"
     ,@pandoc-options-menu)
    ("Switches"
     ;; put the binary options into the menu
     ,@(mapcar #'(lambda (option)
                   (vector (car option) `(pandoc-toggle (quote ,(cdr option)))
                           :active t
                           :style 'toggle
                           :selected `(pandoc-get (quote ,(cdr option)))))
               pandoc-binary-options))))

(easy-menu-add pandoc-mode-menu pandoc-mode-map)

(provide 'pandoc-mode)

;;; pandoc-mode ends here
