;;; pandoc-mode-utils.el --- Part of `pandoc-mode'

;; Copyright (c) 2009-2015 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 31 Oct 2009
;; Version: 2.13
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

;; This file is part of `pandoc-mode'.
;;
;; Pandoc-mode is a minor mode for interacting with Pandoc, a 'universal
;; document converter': <http://johnmacfarlane.net/pandoc/>.
;;
;; See the pandoc-mode manual for usage and installation instructions.

;;; Code:

(require 'dash)
(require 'cl-lib)

(defun nonempty (string)
  "Return STRING, unless it is \"\", in which case return NIL."
  (when (not (string= string ""))
    string))

(defgroup pandoc nil "Minor mode for interacting with pandoc." :group 'wp)

(defcustom pandoc-binary "pandoc"
  "The name of the pandoc binary.
You can specify a full path here or a relative path (the
default).  In the latter case, the value of `exec-path` is used
to search the binary."
  :group 'pandoc
  :type 'file)

(defcustom pandoc-use-async t
  "If non-NIL, use an asynchronous process to run pandoc.
Using an asynchronous subprocess avoids freezing Emacs, but can
cause problems sometimes.  Setting this option to nil runs pandoc
in a synchronous subprocess."
  :group 'pandoc
  :type 'boolean)

(defcustom pandoc-async-success-hook nil
  "List of functions to call when `pandoc' returns successfully.
Note, this hook is only run when `pandoc-use-async' is set to t."
  :group 'pandoc
  :type 'function)

(defcustom pandoc-data-dir "~/.emacs.d/pandoc-mode/"
  "Default `pandoc-mode' data dir.
This is where `pandoc-mode' looks for global settings files."
  :group 'pandoc
  :type 'directory)

(defcustom pandoc-directives '(("include" . pandoc--process-include-directive)
                               ("lisp" . pandoc--process-lisp-directive))
  "List of directives to be processed before pandoc is called.
The directive must be given without `@@'; the function should
return a string that will replace the directive and its
argument (if any).

The directives are processed in the order in which they appear in
this list.  If a directive produces output that contains another
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

(defcustom pandoc-extension-active-marker "X"
  "Marker used to indicate an active extension."
  :group 'pandoc
  :type 'string)

(defcustom pandoc-extension-inactive-marker " "
  "Marker used to indicate an inactive extension."
  :group 'pandoc
  :type 'string)

(defcustom pandoc-major-modes
  '((haskell-mode . "native")
    (text-mode . "markdown")
    (markdown-mode . "markdown")
    (org-mode . "org")
    (gfm-mode . "markdown_github")
    (mediawiki-mode . "mediawiki")
    (textile-mode . "textile")
    (rst-mode . "rst")
    (html-mode . "html")
    (latex-mode . "latex")
    (json-mode . "json"))
  "List of major modes and their default pandoc input formats."
  :group 'pandoc
  :type '(repeat (cons (symbol :tag "Major mode") (string :tag "Input format"))))

(defvar pandoc--input-formats
  '(("commonmark"        "CommonMark"          "c")
    ("haddock"           "Haddock Markup"      "k")
    ("html"              "HTML"                "h")
    ("json"              "JSON"                "j")
    ("latex"             "LaTeX"               "l")
    ("markdown"          "Markdown"            "m")
    ("markdown_github"   "Markdown (Github)"   "G")
    ("markdown_mmd"      "Markdown (MMD)"      "M")
    ("markdown_phpextra" "Markdown (PHPExtra)" "P")
    ("markdown_strict"   "Markdown (Strict)"   "S")
    ("mediawiki"         "MediaWiki"           "w")
    ("native"            "Native Haskell"      "N")
    ("opml"              "OPML"                "O")
    ("org"               "Orgmode"             "o")
    ("rst"               "reStructuredText"    "r")
    ("textile"           "Textile"             "T")
    ("twiki"             "Twiki"               "t")
    ("t2t"               "Txt2Tags"            "x"))
  "List of pandoc input formats.")

(defvar pandoc--input-formats-menu
  (mapcar (lambda (f)
            (cons (cadr f) (car f)))
          pandoc--input-formats)
  "List of items in pandoc-mode's input format menu.")

(defvar pandoc--output-formats
  '(("asciidoc"          "AsciiDoc"                 "a")
    ("beamer"            "Beamer Slide Show"        "B")
    ("commonmark"        "CommonMark"               "C")
    ("context"           "ConTeXt"                  "c")
    ("docbook"           "DocBook XML"              "D")
    ("dokuwiki"          "DokuWiki"                 "W")
    ("dzslides"          "DZSlides Slide Show"      "z")
    ("epub"              "EPUB E-Book"              "e")
    ("epub3"             "EPUB3 E-Book"             "E")
    ("fb2"               "FictionBook2"             "f")
    ("haddock"           "Haddock"                  "k")
    ("html"              "HTML"                     "h")
    ("html5"             "HTML5"                    "H")
    ("icml"              "InDesign ICML"            "I")
    ("json"              "JSON"                     "j")
    ("latex"             "LaTeX"                    "l")
    ("man"               "Man Page"                 "n")
    ("markdown"          "Markdown"                 "m")
    ("markdown_github"   "Markdown (Github)"        "G")
    ("markdown_mmd"      "Markdown (MMD)"           "M")
    ("markdown_phpextra" "Markdown (PHPExtra)"      "P")
    ("markdown_strict"   "Markdown (Strict)"        "S")
    ("mediawiki"         "MediaWiki"                "w")
    ("docx"              "MS Word (docx)"           "d")
    ("native"            "Native Haskell"           "N")
    ("opendocument"      "OpenDocument XML"         "p")
    ("odt"               "OpenOffice Text Document" "L")
    ("opml"              "OPML"                     "O")
    ("org"               "Org-mode"                 "o")
    ("plain"             "Plain Text"               "t")
    ("rst"               "reStructuredText"         "r")
    ("revealjs"          "RevealJS Slide Show"      "J")
    ("rtf"               "Rich Text Format"         "R")
    ("s5"                "S5 HTML/JS Slide Show"    "s")
    ("slideous"          "Slideous Slide Show"      "u")
    ("slidy"             "Slidy Slide Show"         "y")
    ("texinfo"           "TeXinfo"                  "i")
    ("textile"           "Textile"                  "T"))
  "List of Pandoc output formats, their descriptions and hydra shortcut keys.")

(defcustom pandoc-output-format-extensions
  '(("asciidoc"          ".txt")
    ("beamer"            ".tex")
    ("context"           ".tex")
    ("docbook"           ".xml")
    ("dokuwiki"          ".txt")
    ("dzslides"          ".html")
    ("epub"              ".epub")
    ("epub3"             ".epub")
    ("fb2"               ".fb2")
    ("haddock"           ".hs")
    ("html"              ".html")
    ("html5"             ".html")
    ("icml"              ".icml")
    ("json"              ".json")
    ("latex"             ".tex")
    ("man"               "")
    ("markdown"          ".md")
    ("markdown_github"   ".md")
    ("markdown_mmd"      ".md")
    ("markdown_phpextra" ".md")
    ("markdown_strict"   ".md")
    ("mediawiki"         ".mw")
    ("docx"              ".docx")
    ("native"            ".hs")
    ("opendocument"      ".odf")
    ("odt"               ".odt")
    ("opml"              ".opml")
    ("org"               ".org")
    ("plain"             ".txt")
    ("rst"               ".rst")
    ("revealjs"          ".html")
    ("rtf"               ".rtf")
    ("s5"                ".html")
    ("slideous"          ".html")
    ("slidy"             ".html")
    ("texinfo"           ".texi")
    ("textile"           ".textile"))
  "List of Pandoc output formats and their associated file extensions.
These extensions are used when pandoc-mode creates an output
file.  The file extension should include a dot.  Note that it does
not make sense to change the names of the output formats, since
Pandoc only recognizes the ones listed here.  However, it is
possible to customize the extensions."
  :group 'pandoc
  :type '(repeat :tag "Output Format" (list (string :tag "Format") (string :tag "Extension"))))

(defvar pandoc--extensions
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
    ("lists_without_preceding_blankline"   ("markdown_github"))
    ("shortcut_reference_links"            ("markdown" "markdown_strict" "markdown_github" "markdown_php")))
  "List of Markdown extensions supported by Pandoc.")

(defvar pandoc--cli-options nil
  "List of Pandoc command line options that do not need special treatment.
This includes all command line options except the list and alist
options, because they need to be handled separately in
`pandoc--format-all-options'.")

(defvar pandoc--filepath-options
  '(data-dir
    extract-media)
  "List of options that have a file path as value.
These file paths are expanded before they are sent to Pandoc.
For relative paths, the file's working directory is used as base
directory.  Two options are preset, others are added by
`define-pandoc-file-option'.")

(defvar pandoc--switches nil
  "List of binary options.
These are set by `define-pandoc-switch'.")

(defvar pandoc--list-options nil
  "List of options that have a list as value.
These are set by `define-pandoc-list-option'.")

(defvar pandoc--alist-options nil
  "List of options that have an alist as value.
These are set by `define-pandoc-alist-option'.")

(defvar pandoc--options
  `((read)
    (read-lhs)
    (read-extensions ,@(mapcar 'list (sort (mapcar #'car pandoc--extensions) #'string<)))
    (write . "native")
    (write-lhs)
    (write-extensions ,@(mapcar 'list (sort (mapcar #'car pandoc--extensions) #'string<)))
    (output)
    (data-dir)
    (extract-media)
    (output-dir)
    (master-file))                      ; the last two are not actually pandoc options
  "Pandoc option alist.
List of options and their default values.  For each buffer in
which pandoc-mode is activated, a buffer-local copy of this list
is made that stores the local values of the options.  The
`define-pandoc-*-option' functions add their options to this list
with the default value NIL.")

(defvar-local pandoc--local-settings nil "A buffer-local variable holding a file's pandoc options.")

(defvar-local pandoc--settings-modified-flag nil "T if the current settings were modified and not saved.")

(defvar pandoc--output-buffer (get-buffer-create " *Pandoc output*"))

(defvar pandoc--options-menu nil
  "Auxiliary variable for creating the options menu.")

(defvar pandoc--files-menu nil
  "Auxiliary variable for creating the file menu.")

(defmacro with-pandoc-output-buffer (&rest body)
  "Execute BODY with `pandoc--output-buffer' temporarily current.
Make sure that `pandoc--output-buffer' really exists."
  (declare (indent defun))
  `(progn
     (or (buffer-live-p pandoc--output-buffer)
         (setq pandoc--output-buffer (get-buffer-create " *Pandoc output*")))
     (with-current-buffer pandoc--output-buffer
       ,@body)))

(defun pandoc--pp-switch (switch)
  "Return a pretty-printed representation of SWITCH."
  (if (pandoc--get switch)
      "yes"
    "no"))

(defun pandoc--pp-option (option)
  "Return an pretty-printed representation of OPTION."
  (or (pandoc--get option)
      ""))

(defun pandoc--get (option &optional buffer)
  "Return the value of OPTION.
Optional argument BUFFER is the buffer from which the value is to
be retrieved."
  (or buffer (setq buffer (current-buffer)))
  (let ((var (intern (concat "pandoc/" (symbol-name option)))))
    (if (local-variable-p var buffer)
        (buffer-local-value var buffer)
      (cdr (assq option (buffer-local-value 'pandoc--local-settings buffer))))))

;; TODO list options aren't set correctly.
(defun pandoc--set (option value)
  "Set the local value of OPTION to VALUE.
If OPTION is 'variable, VALUE should be a cons of the
form (variable-name . value), which is then added to the
variables already stored, or just (variable-name), in which case
the named variable is deleted from the list."
  (when (assq option pandoc--options) ; check if the option is licit
    (unless (assq option pandoc--local-settings) ; add the option if it's not there
      (push (list option) pandoc--local-settings)
      ;; in case of extensions, also add the list of extensions themselves.
      (if (memq option '(read-extensions write-extensions))
          (setcdr (assq option pandoc--local-settings) (mapcar #'list (sort (mapcar #'car pandoc--extensions) #'string<)))))
    (cond
     ((memq option pandoc--alist-options)
      (pandoc--set-alist-option option value))
     ((memq option pandoc--list-options)
      (pandoc--set-list-option option value))
     ((eq option 'read-extensions)
      (pandoc--set-extension (car value) 'read (cdr value)))
     ((eq option 'write-extensions)
      (pandoc--set-extension (car value) 'write (cdr value)))
     (t (setcdr (assq option pandoc--local-settings) value)))
    (setq pandoc--settings-modified-flag t)))

(defun pandoc--set-alist-option (option new-elem)
  "Set an alist OPTION.
NEW-ELEM is a cons (<name> . <value>), which is added to the alist
for OPTION in `pandoc--local-settings'.  If an element with <name>
already exists, it is replaced, or removed if <value> is NIL."
  (let* ((value (cdr new-elem))
         (items (pandoc--get option))
         (item (assoc (car new-elem) items)))
    (cond
     ((and item value) ; if <name> exists and we have a new value
      (setcdr item value)) ; replace the existing value
     ((and item (not value)) ; if <name> exists but we have no new value
      (setq items (delq item items))) ; remove <name>
     ((and (not item) value) ; if <name> does not exist
      (setq items (cons new-elem items)))) ; add it
    (setcdr (assoc option pandoc--local-settings) items)))

(defun pandoc--set-list-option (option value)
  "Add VALUE to list option OPTION."
  (let* ((values (pandoc--get option))
         (new-values (cons value values)))
    (setcdr (assoc option pandoc--local-settings) new-values)))

(defun pandoc--remove-from-list-option (option value)
  "Remove VALUE from the list of OPTION."
  (let* ((values (pandoc--get option))
         (new-values (remove value values)))
    (setcdr (assoc option pandoc--local-settings) new-values)))

(defun pandoc--toggle (switch)
  "Toggle the value of SWITCH."
  (pandoc--set switch (not (pandoc--get switch))))

;; Note: the extensions appear to be binary options, but they are not:
;; they're really (balanced) ternary options. They can be on or off, but
;; that doesn't tell us whether they're on or off because the user set them
;; that way or because that's the default setting for the relevant format.
;;
;; What we do is we create an alist of the extensions, where each extension
;; can have one of three values: nil, meaning default, the symbol -,
;; meaning switched off by the user, or the symbol +, meaning switched on
;; by the user.

(defun pandoc--extension-in-format-p (extension format &optional rw)
  "Check if EXTENSION is a default extension for FORMAT.
RW must be either 'read or 'write, indicating whether FORMAT is
being considered as an input or an output format."
  (let ((formats (cadr (assoc extension pandoc--extensions))))
    (or (member format formats)
        (member format (cadr (assoc rw formats))))))

(defun pandoc--extension-active-p (extension rw)
  "Return T if EXTENSION is active in the current buffer.
RW is either 'read or 'write, indicating whether to test for the
input or the output format.

An extension is active either if it's part of the in/output
format and hasn't been deactivated by the user, or if the user
has activated it."
  (let ((value (pandoc--get-extension extension rw)))
    (or (eq value '+)
        (and (not value)
             (pandoc--extension-in-format-p extension (pandoc--get rw) rw)))))

(defun pandoc--set-extension (extension rw value)
  "Set the value of EXTENSION for RW to VALUE.
RW is either 'read or 'write, indicating whether the read or
write extension is to be set."
  (setcdr (assoc extension (if (eq rw 'read)
                               (pandoc--get 'read-extensions)
                             (pandoc--get 'write-extensions)))
          value))

(defun pandoc--get-extension (extension rw)
  "Return the value of EXTENSION for RW.
RW is either 'read or 'write, indicating whether the read or
write extension is to be queried."
  (cdr (assoc extension (if (eq rw 'read)
                            (pandoc--get 'read-extensions)
                          (pandoc--get 'write-extensions)))))

;; hydra variables
(defvar pandoc--reader-hydra-list nil)
(defvar pandoc--writer-hydra-list nil)
(defvar pandoc--specific-hydra-list nil)
(defvar pandoc--html-hydra-list nil)
(defvar pandoc--tex-hydra-list nil)
(defvar pandoc--epub-hydra-list nil)
(defvar pandoc--citations-hydra-list nil)
(defvar pandoc--math-hydra-list nil)

;; menu variables
(defvar pandoc--reader-menu-list nil)
(defvar pandoc--writer-menu-list nil)
(defvar pandoc--specific-menu-list nil)
(defvar pandoc--html-menu-list nil)
(defvar pandoc--tex-menu-list nil)
(defvar pandoc--epub-menu-list nil)
(defvar pandoc--citations-menu-list nil)
(defvar pandoc--math-menu-list nil)

(defmacro define-pandoc-switch (option hydra description)
  "Create a binary option.
OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

DESCRIPTION is the description of the option as it will appear in
the menu."
  (declare (indent defun))
  `(progn
     (push ,(vector description `(pandoc--toggle (quote ,option))
                    :active t
                    :style 'toggle
                    :selected `(pandoc--get (quote ,option)))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-menu-list")))
     (push (cons ,description (quote ,option)) pandoc--switches)
     (push (quote ,option) pandoc--cli-options)
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,(list (concat "_" (cadr hydra) "_: " (format (cl-caddr hydra) description) (format " [%%s(pandoc--pp-switch '%s)]" option))
                         (cadr hydra)
                         `(pandoc--toggle (quote ,option))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-hydra-list")))))

(defmacro define-pandoc-file-option (option hydra prompt &optional full-path default)
  "Define OPTION as a file option.
The option is added to `pandoc--options', `pandoc--cli-options',
and to `pandoc--filepath-options' (unless FULL-PATH is NIL).
Furthermore, a menu entry is created and a function to set/unset
the option.

The function to set the option can be called with the prefix
argument C-u - (or M--) to unset the option.  A default value (if
any) can be set by calling the function with any other prefix
argument.  If no prefix argument is given, the user is prompted
for a value.

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"No \", \"Set \" and
\"Default \" can be added before it. If FULL-PATH is T, the full
path to the file is stored, otherwise just the file name without
directory. DEFAULT must be either NIL or T and indicates whether
the option can have a default value."
  (declare (indent defun))
  `(progn
     ,(when full-path
        `(push (quote ,option) pandoc--filepath-options))
     (push (quote ,option) pandoc--cli-options)
     (push (list (quote ,option)) pandoc--options)
     (push (list ,@(delq nil ; if DEFAULT is nil, we need to remove it from the list.
                         (list prompt
                               (vector (concat "No " prompt) `(pandoc--set (quote ,option) nil)
                                       :active t
                                       :style 'radio
                                       :selected `(null (pandoc--get (quote ,option))))
                               (when default
                                 (vector (concat "Default " prompt) `(pandoc--set (quote ,option) t)
                                         :active t
                                         :style 'radio
                                         :selected `(eq (pandoc--get (quote ,option)) t)))
                               (vector (concat "Set " prompt "...") (intern (concat "pandoc-set-"
                                                                                    (symbol-name option)))
                                       :active t
                                       :style 'radio
                                       :selected `(stringp (pandoc--get (quote ,option)))))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-menu-list")))
     (push (quote ,(list (concat "_" (cadr hydra) "_: " (format (cl-caddr hydra) prompt) (format " [%%s(pandoc--pp-option '%s)]" option))
                         (cadr hydra)
                         (intern (concat "pandoc-set-" (symbol-name option)))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-hydra-list")))
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           (lambda (prefix)
             (interactive "P")
             (pandoc--set (quote ,option)
                          (cond
                           ((eq prefix '-) nil)
                           ((null prefix) ,(if full-path
                                               `(read-file-name ,(concat prompt ": "))
                                             `(file-name-nondirectory (read-file-name ,(concat prompt ": ")))))
                           (t ,default)))))))

(defmacro define-pandoc-number-option (option hydra prompt)
  "Define OPTION as a numeric option.
The option is added to `pandoc--options' and to
`pandoc--cli-options'. Furthermore, a menu entry is created and a
function to set/unset the option.

The function to set the option can be called with the prefix
argument C-u - (or M--) to unset the option.  If no prefix
argument is given, the user is prompted for a value.

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"Default \" and \"Set
\" can be added before it."
  (declare (indent defun))
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--cli-options)
     (push (list ,prompt
                 ,(vector (concat "Default " prompt) `(pandoc--set (quote ,option) nil)
                          :active t
                          :style 'radio
                          :selected `(null (pandoc--get (quote ,option))))
                 ,(vector (concat "Set " prompt "...") (intern (concat "pandoc-set-" (symbol-name option)))
                          :active t
                          :style 'radio
                          :selected `(pandoc--get (quote ,option))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-menu-list")))
     (push (quote ,(list (concat "_" (cadr hydra) "_: " (format (cl-caddr hydra) prompt) (format " [%%s(pandoc--pp-option '%s)]" option))
                         (cadr hydra)
                         (intern (concat "pandoc-set-" (symbol-name option)))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-hydra-list")))
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           (lambda (prefix)
             (interactive "P")
             (pandoc--set (quote ,option)
                          (if (eq prefix '-)
                              nil
                            (string-to-number (read-string ,(concat prompt ": ")))))))))

(defmacro define-pandoc-string-option (option hydra prompt &optional default)
  "Define OPTION as a string option.
The option is added to `pandoc--options' and to
`pandoc--cli-options'.  Furthermore, a menu entry is created and a
function to set the option.

The function to set the option can be called with the prefix
argument C-u - (or M--) to unset the option.  A default value (if
any) can be set by calling the function with any other prefix
argument.  If no prefix argument is given, the user is prompted
for a value.

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"No \", \"Set \" and
\"Default \" can be added before it. DEFAULT must be either NIL
or T and indicates whether the option can have a default value."
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--cli-options)
     (push (list ,@(delq nil ; if DEFAULT is nil, we need to remove it from the list.
                         (list prompt
                               (vector (concat "No " prompt) `(pandoc--set (quote ,option) nil)
                                       :active t
                                       :style 'radio
                                       :selected `(null (pandoc--get (quote ,option))))
                               (when default
                                 (vector (concat "Default " prompt) `(pandoc--set (quote ,option) t)
                                         :active t
                                         :style 'radio
                                         :selected `(eq (pandoc--get (quote ,option)) t)))
                               (vector (concat "Set " prompt "...") (intern (concat "pandoc-set-" (symbol-name option)))
                                       :active t
                                       :style 'radio
                                       :selected `(stringp (pandoc--get (quote ,option)))))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-menu-list")))
     (push (quote ,(list (concat "_" (cadr hydra) "_: " (format (cl-caddr hydra) prompt) (format " [%%s(pandoc--pp-option '%s)]" option))
                         (cadr hydra)
                         (intern (concat "pandoc-set-" (symbol-name option)))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-hydra-list")))
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           (lambda (prefix)
             (interactive "P")
             (pandoc--set (quote ,option)
                          (cond
                           ((eq prefix '-) nil)
                           ((null prefix) (read-string ,(concat prompt ": ")))
                           (t ,default)))))))

(defmacro define-pandoc-list-option (option hydra type description prompt)
  "Define OPTION as a list option.
The option is added to `pandoc--options' and
`pandoc--list-options'.  Furthermore, a menu entry is created and a
function to set the option.  This function can also be called with
the prefix argument C-u - (or M--) to unset the option.

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  TYPE specifies the kind of
data that is stored in the list.  Currently, possible values are
`string' and `file'.  DESCRIPTION is the description for the
option's submenu.  PROMPT is a string that is used to prompt for
setting and unsetting the option.  It must be formulated in such a
way that the strings \"Add \", \"Remove \" can be added before
it."
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--list-options)
     (push (list ,description
                 ,(vector (concat "Add " prompt) (intern (concat "pandoc-set-" (symbol-name option)))
                          :active t)
                 ,(vector (concat "Remove " prompt) (list (intern (concat "pandoc-set-" (symbol-name option))) `(quote -))
                          :active `(pandoc--get (quote ,option))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-menu-list")))
     (push (quote ,(list (concat "_" (cadr hydra) "_: " (format (cl-caddr hydra) description) (format " [%%s(pandoc--pp-option '%s)]" option))
                         (cadr hydra)
                         (intern (concat "pandoc-set-" (symbol-name option)))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-hydra-list")))
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           (lambda (prefix)
             (interactive "P")
             (if (eq prefix '-)
                 (let ((value (completing-read "Remove item: " (pandoc--get (quote ,option)) nil t)))
                   (pandoc--remove-from-list-option (quote ,option) value)
                   (message ,(concat prompt " \"%s\" removed.") value))
               (let ((value ,(cond
                              ((eq type 'string)
                               `(read-string "Add value: " nil nil (pandoc--get (quote ,option))))
                              ((eq type 'file)
                               `(read-file-name "Add file: ")))))
                 (pandoc--set (quote ,option) value)
                 (message ,(concat prompt " \"%s\" added.") value)))))))

(defmacro define-pandoc-alist-option (option hydra type description prompt)
  "Define OPTION as an alist option.
The option is added to `pandoc--options' and
`pandoc--alist-options'.  Furthermore, a menu entry is created and
a function to set the option.  This function can also be called
with the prefix argument C-u - (or M--) to unset the option.

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  TYPE specifies the kind of
data that is stored in the list.  Currently, possible values are
`string' and `file'.  DESCRIPTION is the description for the
option's submenu.  PROMPT is a string that is used to prompt for
setting and unsetting the option.  It must be formulated in such a
way that the strings \"Set/Change \" and \"Unset \" can be added
before it."
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--alist-options)
     (push (list ,description
                 ,(vector (concat "Set/Change " prompt) (intern (concat "pandoc-set-" (symbol-name option)))
                          :active t)
                 ,(vector (concat "Unset " prompt) (list (intern (concat "pandoc-set-" (symbol-name option))) `(quote -))
                          :active t))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-menu-list")))
     (push (quote ,(list (concat "_" (cadr hydra) "_: " (format (cl-caddr hydra) description) (format " [%%s(pandoc--pp-option '%s)]" option))
                         (cadr hydra)
                         (intern (concat "pandoc-set-" (symbol-name option)))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-hydra-list")))
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           (lambda (prefix)
             (interactive "P")
             (let ((var (nonempty (completing-read (concat ,prompt ": ") (pandoc--get (quote ,option))))))
               (when var
                 (let ((value (if (eq prefix '-)
                                  nil
                                ,(cond
                                  ((eq type 'string)
                                   `(read-string "Value: " nil nil (cdr (assq var (pandoc--get (quote ,option))))))
                                  ((eq type 'file)
                                   `(read-file-name "File: "))))))
                   ,(when (eq type 'string) ;; strings may be empty (which corresponds to boolean True in Pandoc)
                      '(when (string= value "")
                         (setq value t)))
                   (pandoc--set (quote ,option) (cons var value))
                   (message ,(concat prompt " `%s' \"%s\".") var (if value
                                                                     (format "added with value `%s'" value)
                                                                   "removed")))))))))

(defmacro define-pandoc-choice-option (option hydra prompt choices output-formats)
  "Define OPTION as a choice option.
The option is added to `pandoc--options' and `pandoc--cli-options'.
Furthermore, a menu entry is created and a function to set the
option.

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  PROMPT is a string that is
used to prompt for setting and unsetting the option and is also
used in the menu.  CHOICES is the list of choices, which must be
strings.  The first of these is the default value, i.e., the one
that Pandoc uses if the option is unspecified.  OUTPUT-FORMATS is
a list of output formats for which OPTION should be active in the
menu."
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--cli-options)
     (push (list ,prompt
                 :active (quote (member (pandoc--get 'write) (quote ,output-formats)))
                 ,(vector (car choices) `(pandoc--set (quote ,option) ,(car choices))
                          :style 'radio
                          :selected `(null (pandoc--get (quote ,option))))
                 ,@(mapcar (lambda (choice)
                             (vector choice `(pandoc--set (quote ,option) ,choice)
                                     :style 'radio
                                     :selected `(string= (pandoc--get (quote ,option)) ,choice)))
                           (cdr choices)))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-menu-list")))
     (push (quote ,(list (concat "_" (cadr hydra) "_: " (format (cl-caddr hydra) prompt) (format " [%%s(pandoc--pp-option '%s)]" option))
                         (cadr hydra)
                         (intern (concat "pandoc-set-" (symbol-name option)))))
           ,(intern (concat "pandoc--" (symbol-name (car hydra)) "-hydra-list")))
     (fset (quote ,(intern (concat "pandoc-set-" (symbol-name option))))
           (lambda (prefix)
             (interactive "P")
             (pandoc--set (quote ,option)
                          (if (eq prefix '-)
                              nil
                            (let ((value (completing-read ,(format "Set %s: " prompt) (quote ,choices) nil t)))
                              (if (or (not value)
                                      (member value '("" (car ,choices))))
                                  nil
                                value))))))))

(defun pandoc--trim-right-padding (strings)
  "Trim right padding in STRINGS.
STRINGS is a list of strings ending in one or more spaces.  The
right padding of each string is trimmed to the longest string."
  (let ((n (-min (--map (let ((idx (string-match-p " *\\'" it)))
                          (length (substring it idx)))
                        strings))))
    (--map (substring it 0 (if (> n 0) (- n))) strings)))

(defun pandoc--tabulate (strings &optional colwidth width fmt-str colsep trim)
  "Tabulate STRINGS.
STRINGS is a list of strings.  The return value is a string
containing STRINGS tabulated top-to-bottom, left-to-right.
COLWIDTH is the width of the columns of the table, which defaults
to the width of the largest string in STRINGS.  Each string is
right-padded with spaces to make it the length of COLWIDTH.  WIDTH
is the width of the table, which defaults to the width of the
current frame.  The number of rows and columns is calculated on
the basis of COLWIDTH and WIDTH.

FMT-STR is a format string that is used to format STRINGS.  It
defaults to \"%-<n>s\", where <n> is colwidth.  FMT-STR must
contain a \"%s\" specifier for the strings to be tabulated. Note
that if FMT-STR is provided, COLWIDTH is only used to calculate
the number of rows and columns, not for padding the strings. The
calling function must then ensure that the strings are of
equal length.

COLSEP is the string placed between two columns. It defaults to
two spaces. The length of this string is taken into account when
calculating the number of columns.

If TRIM is t, each row is trimmed to its widest member."

  (or colwidth (setq colwidth (-max (--map (length it) strings))))
  (or width (setq width (frame-width)))
  (or fmt-str (setq fmt-str (format "%%-%ds" colwidth)))
  (or colsep (setq colsep "  "))
  (let* ((n-cols (/ width (+ (length colsep) colwidth)))
         (n-rows (if (= n-cols 0) ; happens when `width' is too small to hold `strings'.
                     (length strings)
                   (ceiling (/ (length strings) (float n-cols)))))
         (cols (-partition-all n-rows (--map (format fmt-str it) strings))))
    (if trim
        (setq cols (-map #'pandoc--trim-right-padding cols)))
    (let ((rows (apply #'-zip-fill "" cols)))
      (if (atom (cdar rows)) ; -zip-fill returns cons cells if it zips two lists
          (setq rows (mapc (lambda (c)
                             (setcdr c (list (cdr c))))
                           rows)))
      (mapconcat (lambda (line)
                   (mapconcat #'identity line colsep))
                 rows
                 "\n"))))

(defun pandoc--tabulate-extensions (rw)
  "Tabulate extension strings as a new string.
RW can be `read' or `write', indicating which extensions to
insert."
  (let* ((extensions (--map (car it) pandoc--extensions))
         (colwidth (-max (-map #'length extensions)))
         (fmt-str (format "%%2d %%%%s(pandoc--extension-active-marker \"%%s\" '%%s) %%-%ds" colwidth))
         (strings (--map (format fmt-str
                                 (1+ (-elem-index it extensions))
                                 it
                                 rw
                                 (replace-regexp-in-string "_" " " it))
                         extensions)))
    (pandoc--tabulate strings (+ 5 colwidth) nil "%s" nil 'trim)))

(defun pandoc--tabulate-input-formats ()
  "Tabulate input formats for `pandoc-input-format-hydra'."
  (let ((strings (--map (concat "_" (cl-caddr it) "_: " (cadr it)) pandoc--input-formats)))
    (pandoc--tabulate strings nil 70 nil nil 'trim)))

(defun pandoc--tabulate-output-formats ()
  "Tabulate output formats for `pandoc-output-format-hydra'."
  (let ((strings (--map (concat "_" (cl-caddr it) "_: " (cadr it)) pandoc--output-formats)))
    (pandoc--tabulate strings nil 150 nil nil 'trim)))

(defmacro define-pandoc-hydra (name body docstring hexpr &rest extra-heads)
  "Define a pandoc-mode hydra.
NAME, BODY and DOCSTRING are as in `defhydra'.  HEXPR is an
expression that is evaluated and should yield a list of hydra
heads.  EXTRA-HEADS are additional heads, which are not
evaluated."
  (let ((heads (eval hexpr)))
    `(defhydra ,name ,body
       ,docstring
       ,@heads
       ,@extra-heads)))

;;; Defining the options
;; Note that the options are added to the menus and hydras in reverse order.

;;; Reader options
(define-pandoc-choice-option  track-changes           (reader "T" "%-23s")        "Track Changes" ("accept" "reject" "all") ("docx"))
(define-pandoc-number-option  tab-stop                (reader "t" "%-23s")        "Tab Stop Width")
(define-pandoc-switch         preserve-tabs           (reader "p" "%-23s")        "Preserve Tabs")
(define-pandoc-switch         normalize               (reader "n" "%-23s")        "Normalize Document")
(define-pandoc-alist-option   metadata                (reader "m" "%-23s") string "Metadata" "Metadata item")
(define-pandoc-list-option    filter                  (reader "f" "%-23s") file   "Filters" "Filter")
(define-pandoc-string-option  default-image-extension (reader "i" "%-23s")        "Default Image Extension")
(define-pandoc-string-option  indented-code-classes   (reader "c" "%-23s")        "Indented Code Classes")
(define-pandoc-number-option  base-header-level       (reader "h" "%-23s")        "Base Header Level")
(define-pandoc-switch         old-dashes              (reader "o" "%-23s")        "Use Old-style Dashes")
(define-pandoc-switch         strict                  (reader "S" "%-23s")        "Strict")
(define-pandoc-switch         smart                   (reader "s" "%-23s")        "Smart")
(define-pandoc-switch         parse-raw               (reader "r" "%-23s")        "Parse Raw")
;; extract-media

;; TODO for data-dir, output-dir and extract-media, a macro define-pandoc-dir-option might be useful.


;;; General writer options
(define-pandoc-file-option          include-after-body  (writer "A" "%-19s") "Include After Body"  'full-path)
(define-pandoc-file-option          include-before-body (writer "B" "%-19s") "Include Before Body" 'full-path)
(define-pandoc-file-option          include-in-header   (writer "H" "%-19s") "Include Header"      'full-path)
(define-pandoc-string-option        highlight-style     (writer "S" "%-19s") "Highlighting Style")
(define-pandoc-switch               no-highlight        (writer "h" "%-19s") "No Highlighting")
(define-pandoc-number-option        toc-depth           (writer "D" "%-19s") "TOC Depth")
(define-pandoc-switch               table-of-contents   (writer "T" "%-19s") "Table of Contents")
(define-pandoc-number-option        columns             (writer "c" "%-19s") "Column Width")
(define-pandoc-switch               no-wrap             (writer "w" "%-19s") "No Wrap")
(define-pandoc-alist-option  variable            (writer "v" "%-19s") string "Variables"           "Variable")
(define-pandoc-file-option          template            (writer "t" "%-19s") "Template File"       'full-path)
(define-pandoc-switch               standalone          (writer "s" "%-19s") "Standalone")
;; print-default-template ; not actually included


;;; Options affecting specific writers

;; general
(define-pandoc-file-option    reference-docx  (specific "d" "%-21s") "Reference docx File" 'full-path)
(define-pandoc-file-option    reference-odt   (specific "o" "%-21s") "Reference ODT File"  'full-path)
(define-pandoc-number-option  slide-level     (specific "h" "%-21s") "Slide Level Header")
(define-pandoc-switch         incremental     (specific "i" "%-21s") "Incremental")
(define-pandoc-switch         number-sections (specific "n" "%-21s") "Number Sections")
(define-pandoc-switch         atx-headers     (specific "a" "%-21s") "Use ATX-style Headers")
(define-pandoc-switch         reference-links (specific "r" "%-21s") "Reference Links")

;; html-based
(define-pandoc-string-option id-prefix    (html "i" "%-31s")      "ID prefix")
(define-pandoc-list-option   css          (html "c" "%-31s") file "CSS Style Sheet" "CSS")

(define-pandoc-string-option  title-prefix      (html "t" "%-31s") "Title prefix")
(define-pandoc-choice-option  email-obfuscation (html "e" "%-31s") "Email Obfuscation" ("none" "javascript" "references") ("html" "html5" "s5" "slidy" "slideous" "dzslides" "revealjs"))
(define-pandoc-switch         section-divs      (html "d" "%-31s") "Wrap Sections in <div> Tags")
(define-pandoc-string-option  number-offset     (html "o" "%-31s") "Number Offsets")
(define-pandoc-switch         ascii             (html "a" "%-31s") "Use Only ASCII in HTML")
(define-pandoc-switch         html-q-tags       (html "Q" "%-31s") "Use <q> Tags for Quotes in HTML")
(define-pandoc-switch         self-contained    (html "s" "%-31s") "Self-contained Document")

;; TeX-based (LaTeX, ConTeXt)
(define-pandoc-list-option    latex-engine-opt (tex "o" "%-30s") string "Options for LaTeX command" "LaTeX Options")
(define-pandoc-choice-option  latex-engine     (tex "e" "%-30s")        "LaTeX Engine" ("pdflatex" "xelatex" "lualatex") ("latex" "beamer" "context"))
(define-pandoc-switch         listings         (tex "L" "%-30s")        "Use LaTeX listings Package")
(define-pandoc-switch         no-tex-ligatures (tex "l" "%-30s")        "Do Not Use TeX Ligatures")
(define-pandoc-switch         chapters         (tex "c" "%-30s")        "Top-level Headers Are Chapters")

;; epub
(define-pandoc-number-option  epub-chapter-level (epub "c" "%-18s")      "EPub Chapter Level")
(define-pandoc-list-option    epub-embed-font    (epub "f" "%-18s") file "EPUB Fonts"         "EPUB Embedded Font")
(define-pandoc-file-option    epub-metadata      (epub "m" "%-18s")      "EPUB Metadata File" 'full-path)
(define-pandoc-file-option    epub-cover-image   (epub "C" "%-18s")      "EPUB Cover Image"   'full-path)
(define-pandoc-file-option    epub-stylesheet    (epub "s" "%-18s")      "EPUB Style Sheet"   'full-path 'default)


;;; Citation rendering
(define-pandoc-switch       biblatex               (citations "l" "%-27s")      "Use BibLaTeX")
(define-pandoc-switch       natbib                 (citations "n" "%-27s")      "Use NatBib")
(define-pandoc-file-option  citation-abbreviations (citations "a" "%-27s")      "Citation Abbreviations File" 'full-path)
(define-pandoc-file-option  csl                    (citations "c" "%-27s")      "CSL File"                    'full-path)
(define-pandoc-list-option  bibliography           (citations "B" "%-27s") file "Bibliography Files"          "Bibliography File")


;;; Math rendering in HTML
(define-pandoc-string-option  katex-stylesheet (math "K" "%-18s") "KaTeX Stylesheet"   'default)
(define-pandoc-string-option  katex            (math "k" "%-18s") "KaTeX URL"          'default)
(define-pandoc-string-option  webtex           (math "w" "%-18s") "WebTeX URL"         'default)
(define-pandoc-string-option  mimetex          (math "m" "%-18s") "MimeTeX CGI Script" 'default)
(define-pandoc-switch         gladtex          (math "g" "%-18s") "gladTeX")
(define-pandoc-string-option  mathjax          (math "J" "%-18s") "MathJax URL"        'default)
(define-pandoc-string-option  jsmath           (math "j" "%-18s") "jsMath URL"         'default)
(define-pandoc-string-option  mathml           (math "m" "%-18s") "MathML URL"         'default)
(define-pandoc-string-option  latexmathml      (math "L" "%-18s") "LaTeXMathML URL"    'default)

(provide 'pandoc-mode-utils)

;;; pandoc-mode-utils.el ends here
