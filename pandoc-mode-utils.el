;;; pandoc-mode-utils.el --- Part of `pandoc-mode'  -*- lexical-binding: t -*-

;; Copyright (c) 2009-2021 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 31 Oct 2009
;; Version: 2.31
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

(defcustom pandoc-process-connection-type (default-value process-connection-type)
  "Control type of device used to communicate with the pandoc subprocess.
This option only takes effect if `pandoc-use-async' is set to t.
The variable `process-connection-type' is set to the value of
this option before calling pandoc.  See the doc string of that
variable for details."
  :group 'pandoc
  :type '(choice (const :tag "Use a pty" t)
                 (const :tag "Use a pipe" nil)))

(defcustom pandoc-async-success-hook nil
  "List of functions to call when `pandoc' returns successfully.
This hook is only run when `pandoc-use-async' is set to t."
  :group 'pandoc
  :type 'hook)

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

(defcustom pandoc-citation-jump-function 'pandoc-goto-citation-reference
  "Action to take when locating a BibTeX reference from a citation key.
Three actions have been predefined: Open the BibTeX file in Emacs
and jump to the location of the key (the default option), open
the BibTeX file in Ebib and show the key, or show the key's entry
in a *Help* buffer.

It is also possible to use a custom function.  This function must
take two arguments:

1) The string that matches the citation KEY at point
2) A list of BIBLIOGRAPHY files

It should direct the user to a bibliographic reference that
matches KEY."
  :group 'pandoc
  :type '(choice (const :tag "Open BibTeX file" pandoc-goto-citation-reference)
                 (const :tag "Open Ebib" pandoc-open-in-ebib)
                 (const :tag "Show entry in *Help* buffer" pandoc-show-citation-as-help)
                 (function :tag "Use a custom function")))

(defcustom pandoc-major-modes
  '((gfm-mode       . "gfm")
    (haskell-mode   . "native")
    (html-mode      . "html")
    (json-mode      . "json")
    (latex-mode     . "latex")
    (markdown-mode  . "markdown")
    (mediawiki-mode . "mediawiki")
    (muse-mode      . "muse")
    (org-mode       . "org")
    (rst-mode       . "rst")
    (text-mode      . "plain")
    (textile-mode   . "textile"))
  "List of major modes and their default pandoc input formats."
  :group 'pandoc
  :type '(repeat (cons (symbol :tag "Major mode") (string :tag "Input format"))))

;; The formats known to Pandoc are defined in `pandoc--formats'.  They are divided
;; into different categories.  From this list, the input and output format menus
;; are created, which both consist of a main menu listing the categories and
;; submenus for each category.
;;
;; Note: the keys "b" and "q" cannot be used, because they are used in the
;; hydras for returning to the higher level menu and for quiting, respectively.

(defvar pandoc--formats
  '(("markdown" "Markdown Formats" "m"
     ("markdown"               "Pandoc Markdown"               "m" both)
     ("markdown_mmd"           "Markdown (MMD)"                "M" both)
     ("markdown_phpextra"      "Markdown (PHPExtra)"           "P" both)
     ("markdown_strict"        "Markdown (Strict)"             "S" both)
     ("commonmark"             "CommonMark"                    "C" both)
     ("gfm"                    "GitHub-flavoured Markdown"     "g" both)
     ("markdown_github"        "Markdown (Github; obsolete)"   "G" both))

    ("html" "HTML Formats" "h"
     ("html"                   "HTML (default)"               "h" both)
     ("html4"                  "HTML4"                        "t" output)
     ("html5"                  "HTML5"                        "H" output))

    ("slide-show" "Slide Show Formats" "s"
     ("beamer"                 "Beamer"                       "B" output) ; Also under TeX
     ("dzslides"               "DZSlides"                     "d" output)
     ("revealjs"               "RevealJS"                     "j" output)
     ("pptx"                   "MS PowerPoint"                "p" output)
     ("s5"                     "S5 HTML/JS"                   "s" output)
     ("slideous"               "Slideous"                     "u" output)
     ("slidy"                  "Slidy"                        "y" output))

    ("wiki" "Wiki Formats" "w"
     ("creole"                 "Creole 1.0"                   "c" both)
     ("dokuwiki"               "DokuWiki"                     "d" both)
     ("jira"                   "JiraWiki"                     "j" both)
     ("mediawiki"              "MediaWiki"                    "m" both)
     ("tikiwiki"               "TikiWiki"                     "t" both)
     ("twiki"                  "Twiki"                        "T" input)
     ("vimwiki"                "Vimwiki"                      "v" both)
     ("zimwiki"                "ZimWiki"                      "z" both))

    ("wordprocessor" "Wordprocessor Formats" "W"
     ("docx"                   "MS Word (docx)"               "d" both)
     ("icml"                   "InDesign ICML"                "i" output)
     ("odt"                    "LibreOffice Text Document"    "l" both)
     ("opendocument"           "OpenDocument XML"             "o" output)
     ("rtf"                    "Rich Text Format"             "r" output))

    ("tex" "TeX-based Formats" "t"
     ("beamer"                 "Beamer Slide Show"            "B" output) ; Also under Slide Shows Formats.
     ("context"                "ConTeXt"                      "c" output)
     ("latex"                  "LaTeX"                        "l" both)
     ("texinfo"                "TeXinfo"                      "i" output)) ; Also under Documentation Formats.

    ("ebook" "E-Book Formats" "e"
     ("epub"                   "EPUB (default)"               "e" both)
     ("epub2"                  "EPUB2 E-Book"                 "p" output)
     ("epub3"                  "EPUB3 E-Book"                 "E" output)
     ("fb2"                    "FictionBook2"                 "f" both))

    ("text" "Text-Based Formats" "T"
     ("asciidoc"               "AsciiDoc"                     "a" output)
     ("csv"                    "CSV"                          "c" input)
     ("plain"                  "Plain Text"                   "p" output)
     ("rst"                    "reStructuredText"             "r" both)
     ("textile"                "Textile"                      "t" both)
     ("t2t"                    "txt2tags"                     "T" both))

    ("documentation" "Documentation Formats" "d"
     ("docbook"                "DocBook XML"                  "d" input) ; docbook and docbook4 share the same key.
     ("docbook4"               "DocBook XML v.4"              "d" output) ; They won't appear in the same menu anyway.
     ("docbook5"               "DocBook XML v.5"              "D" output)
     ("haddock"                "Haddock"                      "h" both)
     ("man"                    "Man Page"                     "m" output)
     ("ms"                     "Groff MS"                     "g" output)
     ("tei"                    "TEI"                          "t" output)
     ("texinfo"                "TeXinfo"                      "i" output)) ; Also under TeX Formats.

    ("emacs" "Emacs-based Formats" "E"
     ("muse"                   "Muse"                         "m" both)
     ("org"                    "Org-mode"                     "o" both))

    ("jats" "JATS formats" "j"
     ("jats"                   "Archiving Tag Set"            "j" both)
     ("jats_articleauthoring"  "Article Authoring Tag Set"    "a" both)
     ("jats_publishing"        "Publishing Tag Set"           "p" both)
     ("jats_archiving"         "Archiving Tag Set"            "x" both))

    ("misc" "Miscellaneous Formats" "v"
     ("ipynb"                  "Jupyter Notebook"             "p" both)
     ("json"                   "JSON"                         "j" both)
     ("native"                 "Native Haskell"               "n" both)
     ("opml"                   "OPML"                         "o" both)))
  "List of Pandoc formats, their descriptions and hydra shortcut keys.")

(defun pandoc--extract-formats (io)
  "Extract the input or output formats in `pandoc--formats'.
IO is a symbol, either `input' or `output'.  Return a list of formats."
  (-flatten-n 1 (-map (-lambda ((_name _descr _key . formats))
                        (--filter (memq (-last-item it) `(,io both)) formats))
                      pandoc--formats)))

(defvar pandoc--input-formats-menu
  (mapcar (lambda (f)
            (cons (cadr f) (car f)))
          (pandoc--extract-formats 'input))
  "List of items in pandoc-mode's input format menu.")

(defcustom pandoc-output-format-extensions
  '(("asciidoc"          ".txt")
    ("beamer"            ".tex")
    ("commonmark"        ".md")
    ("context"           ".tex")
    ("docbook"           ".xml")
    ("docbook4"          ".xml")
    ("docbook5"          ".xml")
    ("docx"              ".docx")
    ("dokuwiki"          ".txt")
    ("dzslides"          ".html")
    ("epub"              ".epub")
    ("epub2"             ".epub")
    ("epub3"             ".epub")
    ("fb2"               ".fb2")
    ("gfm"               ".md")
    ("haddock"           ".hs")
    ("html"              ".html")
    ("html4"             ".html")
    ("html5"             ".html")
    ("icml"              ".icml")
    ("ipynb"             ".ipynb")
    ("jats"              ".xml")
    ("json"              ".json")
    ("latex"             ".tex")
    ("man"               "")
    ("markdown"          ".md")
    ("markdown_github"   ".md")
    ("markdown_mmd"      ".md")
    ("markdown_phpextra" ".md")
    ("markdown_strict"   ".md")
    ("mediawiki"         ".mw")
    ("ms"                ".ms")
    ("muse"              ".muse")
    ("native"            ".hs")
    ("odt"               ".odt")
    ("opendocument"      ".odf")
    ("opml"              ".opml")
    ("org"               ".org")
    ("plain"             ".txt")
    ("pptx"              ".pptx")
    ("revealjs"          ".html")
    ("rst"               ".rst")
    ("rtf"               ".rtf")
    ("s5"                ".html")
    ("slideous"          ".html")
    ("slidy"             ".html")
    ("tei"               ".xml")
    ("texinfo"           ".texi")
    ("textile"           ".textile")
    ("zimwiki"           ".txt"))
  "List of Pandoc output formats and their associated file extensions.
These extensions are used when pandoc-mode creates an output
file.  The file extension should include a dot.  Note that it does
not make sense to change the names of the output formats, since
Pandoc only recognizes the ones listed here.  However, it is
possible to customize the extensions."
  :group 'pandoc
  :type '(repeat :tag "Output Format" (list (string :tag "Format") (string :tag "Extension"))))

(defcustom pandoc-viewers
  '(("asciidoc"          emacs)
    ("beamer"            emacs)
    ("commonmark"        emacs)
    ("context"           emacs)
    ("docbook"           nil)
    ("docbook4"          nil)
    ("docbook5"          nil)
    ("docx"              "libreoffice")
    ("dokuwiki"          emacs)
    ("dzslides"          browe-url)
    ("epub"              nil)
    ("epub2"             nil)
    ("epub3"             nil)
    ("fb2"               nil)
    ("gfm"               emacs)
    ("haddock"           emacs)
    ("html"              browse-url)
    ("html4"             browse-url)
    ("html5"             browse-url)
    ("icml"              nil)
    ("ipynb"             nil)
    ("jats"              nil)
    ("json"              emacs)
    ("latex"             emacs)
    ("man"               emacs)
    ("markdown"          emacs)
    ("markdown_github"   emacs)
    ("markdown_mmd"      emacs)
    ("markdown_phpextra" emacs)
    ("markdown_strict"   emacs)
    ("mediawiki"         emacs)
    ("ms"                emacs)
    ("muse"              emacs)
    ("native"            emacs)
    ("odt"               "libreoffice")
    ("opendocument"      "liberoffice")
    ("opml"              nil)
    ("org"               emacs)
    ("plain"             emacs)
    ("pptx"              "libreoffice")
    ("revealjs"          browse-url)
    ("rst"               emacs)
    ("rtf"               "libreoffice")
    ("s5"                browse-url)
    ("slideous"          browse-url)
    ("slidy"             browse-url)
    ("tei"               nil)
    ("texinfo"           emacs)
    ("textile"           emacs)
    ("zimwiki"           emacs))
  "List of Pandoc output formats and their associated file viewers.
This option defines the viewers used in `pandoc-view-output'.
The viewer can be a string, in which case it is assumed to be a
shell command, which is executed through `start-process'.  The
viewer can also be an Emacs function, which is passed the full
file name of the output file.  Lastly, the viewer can be the
symbol `emacs', in which case the output file is opened in Emacs
with `find-file-noselect' and displayed with `display-buffer'."
  :group 'pandoc
  :type '(repeat :tag "File viewers" (list (string :tag "Format") (choice (const :tag "No viewer defined" nil)
                                                                          (string :tag "Use External viewer")
                                                                          (const :tag "Use Emacs" emacs)
                                                                          (function :tag "Use a specific function")))))

(defcustom pandoc-pdf-viewer 'emacs
  "Viewer for pdf files.
This can be the symbol `emacs', in which case the pdf file is
opened with `find-file-noselect' and displayed with
`display-buffer'.  The value can also be a string, in which case
it is assumed to be an external viewer, which is called with
`start-process'."
  :group 'pandoc
  :type '(choice (const :tag "Use Emacs" emacs)
                 (string :tag "Use external viewer")))

(defvar pandoc--pdf-able-formats '("latex" "context" "beamer" "html" "ms")
  "List of output formats that can be used to generate pdf output.")

(defvar pandoc--extensions
  '(("abbreviations"                       ("markdown_phpextra"))
    ("all_symbols_escapable"               ("markdown" "markdown_mmd"))
    ("amuse"                               ())
    ("angle_brackets_escapable"            ("markdown_github"))
    ("ascii_identifiers"                   ("markdown_github"))
    ("auto_identifiers"                    ("markdown" "markdown_github" "markdown_mmd" "latex" "rst" "mediawiki" "textile"))
    ("autolink_bare_uris"                  ("markdown_github"))
    ("backtick_code_blocks"                ("markdown" "markdown_github"))
    ("blank_before_blockquote"             ("markdown"))
    ("blank_before_header"                 ("markdown"))
    ("bracketed_spans"                     ("markdown"))
    ("citations"                           ("markdown"))
    ("compact_definition_lists"            ())
    ("definition_lists"                    ("markdown" "markdown_phpextra" "markdown_mmd"))
    ("east_asian_line_breaks"              ())
    ("emoji"                               ("markdown_github"))
    ("empty_paragraphs"                    ())
    ("epub_html_exts"                      ())
    ("escaped_line_breaks"                 ("markdown"))
    ("example_lists"                       ("markdown"))
    ("fancy_lists"                         ("markdown"))
    ("fenced_code_attributes"              ("markdown" "markdown_github"))
    ("fenced_code_blocks"                  ("markdown" "markdown_phpextra" "markdown_github"))
    ("fenced_divs"                         ("markdown"))
    ("footnotes"                           ("markdown" "markdown_phpextra" "markdown_mmd"))
    ("four_space_rule"                     ())
    ("gfm_auto_identifiers"                ())
    ("grid_tables"                         ("markdown"))
    ("hard_line_breaks"                    ("markdown_github"))
    ("header_attributes"                   ("markdown" "markdown_phpextra"))
    ("ignore_line_breaks"                  ())
    ("implicit_figures"                    ("markdown"))
    ("implicit_header_references"          ("markdown" "markdown_mmd"))
    ("inline_code_attributes"              ("markdown"))
    ("inline_notes"                        ("markdown"))
    ("intraword_underscores"               ("markdown" "markdown_phpextra" "markdown_github" "markdown_mmd"))
    ("latex_macros"                        ("markdown"))
    ("line_blocks"                         ("markdown"))
    ("link_attributes"                     ("markdown" "markdown_phpextra")) ; TODO check
    ("lists_without_preceding_blankline"   ("markdown_github"))
    ("literate_haskell"                    ())
    ("markdown_attribute"                  ("markdown_phpextra" "markdown_mmd"))
    ("markdown_in_html_blocks"             ("markdown"))
    ("mmd_header_identifiers"              ("markdown_mmd"))
    ("mmd_link_attributes"                 ("markdown_mmd"))
    ("mmd_title_block"                     ("markdown_mmd"))
    ("multiline_tables"                    ("markdown"))
    ("native_divs"                         ("markdown"))
    ("native_spans"                        ("markdown"))
    ("ntb"                                 ())
    ("old_dashes"                          ())
    ("pandoc_title_block"                  ("markdown"))
    ("pipe_tables"                         ("markdown" "markdown_phpextra" "markdown_github" "markdown_mmd"))
    ("raw_attribute"                       ("markdown"))
    ("raw_html"                            ("markdown" "markdown_phpextra" "markdown_github" "markdown_mmd" "markdown_strict"))
    ("raw_tex"                             ("markdown" "markdown_mmd"))
    ("shortcut_reference_links"            ("markdown" "markdown_strict" "markdown_github" "markdown_phpextra" "markdown_mmd"))
    ("simple_tables"                       ("markdown"))
    ("smart"                               ("markdown"))
    ("space_in_atx_header"                 ("markdown"))
    ("spaced_reference_links"              ())
    ("startnum"                            ("markdown"))
    ("strikeout"                           ("markdown" "markdown_github"))
    ("subscript"                           ("markdown"))
    ("superscript"                         ("markdown"))
    ("styles"                              ())
    ("table_captions"                      ("markdown"))
    ("task_lists"                          ("markdown" "gfm" "org"))
    ("tex_math_dollars"                    ("markdown" "markdown_mmd" "html"))
    ("tex_math_double_backslash"           ("markdown_mmd" "html"))
    ("tex_math_single_backslash"           ("markdown_github" "html"))
    ("yaml_metadata_block"                 ("markdown")))
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

(defvar pandoc--switches '(("Use File Scope" . file-scope))
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
    (defaults)
    (extract-media)
    (file-scope)
    (output-dir)
    (master-file))                      ; the last two are not actually pandoc options
  "Pandoc option alist.
List of options and their default values.  For each buffer in
which pandoc-mode is activated, a buffer-local copy of this list
is made that stores the local values of the options.  The
`define-pandoc-*-option' functions add their options to this list
with the default value nil.")

(defvar-local pandoc--local-settings nil "A buffer-local variable holding a file's pandoc options.")

(defvar-local pandoc--settings-modified-flag nil "T if the current settings were modified and not saved.")

(defvar-local pandoc--latest-run nil "Cons of the output format and the output file created during the most recent call to Pandoc.")

(defvar pandoc--output-buffer-name " *Pandoc output*")
(defvar pandoc--log-buffer-name " *Pandoc log*")
(defvar pandoc--viewer-buffer-name " *Pandoc viewer*")

(defvar pandoc--options-menu nil
  "Auxiliary variable for creating the options menu.")

(defvar pandoc--files-menu nil
  "Auxiliary variable for creating the file menu.")

(defun pandoc--read-file-name (prompt dir relative)
  "Read a file name using PROMPT.
DIR is the directory used for completing file names.  If RELATIVE
is non-nil, return the file path as a relative path starting from
DIR, otherwise return the full path."
  ;; we inhibit inserting the default directory, though not all completion
  ;; systems honor this.
  (let* ((insert-default-directory (not relative))
         (file (read-file-name prompt dir)))
    (if relative
        (file-relative-name file dir)
      file)))

(defun pandoc--expand-absolute-path (filename)
  "Expand FILENAME if it is an absolute path.
If FILENAME is a relative path, return it unchanged."
  (if (file-name-absolute-p filename)
      (expand-file-name filename)
    filename))

(defun pandoc--create-file-name-from-buffer (buffer-name)
  "Create a file name from BUFFER-NAME.
The file name is formed from BUFFER-NAME by removing any
characters that might be problematic in a file name.  Characters
that are retained are alphabetic characters, digits and the
characters `+' (plus sign), `_' (underscore), `.' (dot) and
`-' (minus sign).  All other characters are removed."
  (cl-remove-if-not (lambda (c)
                      (string-match-p "[[:alpha:][:digit:]+_.-]" (char-to-string c)))
                    buffer-name))

(defun pandoc--log (type format-string &rest args)
  "Write a message to the *Pandoc log* buffer.
If TYPE is `message', also display the message in the echo area.
Any other value just logs the message, adding an empty line after
it.  The arguments FORMAT-STRING and ARGS function as with
`message'."
  (with-current-buffer (get-buffer-create pandoc--log-buffer-name)
    (goto-char (point-max))
    (insert (apply #'format format-string args) "\n\n"))
  (when (eq type 'message)
    (apply #'message format-string args)))

(defun pandoc--pp-switch (switch)
  "Return a pretty-printed representation of SWITCH."
  (if (pandoc--get switch)
      "yes"
    "no"))

(defun pandoc--pp-option (option)
  "Return a pretty-printed representation of OPTION."
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

(defun pandoc--set (option value)
  "Set the local value of OPTION to VALUE."
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
already exists, it is replaced, or removed if <value> is NIL.

If NEW-ELEM is nil, OPTION is unset entirely."
  (let* ((value (cdr new-elem))
         (items (pandoc--get option))
         (item (assoc (car new-elem) items)))
    (cond
     ((null new-elem)
      (setq items nil))
     ((and item value) ; if <name> exists and we have a new value
      (setcdr item value)) ; replace the existing value
     ((and item (not value)) ; if <name> exists but we have no new value
      (setq items (delq item items))) ; remove <name>
     ((and (not item) value) ; if <name> does not exist
      (setq items (append items (list new-elem))))) ; add it
    (setcdr (assoc option pandoc--local-settings) items)))

(defun pandoc--set-list-option (option value)
  "Add VALUE to list option OPTION.
If VALUE is nil, OPTION is unset entirely."
  (let* ((values (pandoc--get option)))
    (setcdr (assoc option pandoc--local-settings)
            (if value
                (append values (list value))
              nil)))) ; if VALUE was nil, we unset the option

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
RW is either `read' or `write', indicating whether to test for the
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

(defmacro define-pandoc-file-option (option hydra prompt &optional default)
  "Define OPTION as a file option.
The option is added to `pandoc--options', `pandoc--cli-options',
and to `pandoc--filepath-options'.  Furthermore, a menu entry is
created and a function to set/unset the option.

The function to set the option can be called with the prefix
argument `\\[universal-argument] -' (or `\\[negative-argument]')
to unset the option.  It can also be called with the prefix
argument \\[universal-argument], in which case the file's full
path is stored.

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
\"Default \" can be added before it.  DEFAULT must be either NIL
or T and indicates whether the option can have a default value."
  (declare (indent defun))
  `(progn
     (push (quote ,option) pandoc--filepath-options)
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
                           ((eq prefix '-) nil) ; C-u - or M--
                           ((listp prefix) ; no prefix or C-u
                            (pandoc--read-file-name (concat ,prompt ": ") default-directory (not prefix)))
                           (t ,default))))))) ; any other prefix

(defmacro define-pandoc-number-option (option hydra prompt)
  "Define OPTION as a numeric option.
The option is added to `pandoc--options' and to
`pandoc--cli-options'.  Furthermore, a menu entry is created and
a function to set/unset the option.

The function to set the option can be called with the prefix
argument `\\[universal-argument] -' (or `\\[negative-argument]')
to unset the option.  If no prefix argument is given, the user is
prompted for a value.

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
argument `\\[universal-argument] -' (or `\\[negative-argument]')
to unset the option.  A default value (if any) can be set by
calling the function with any other prefix argument.  If no
prefix argument is given, the user is prompted for a value.

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
\"Default \" can be added before it.  DEFAULT must be either NIL
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
The option is added to `pandoc--options' and `pandoc--list-options'.

Furthermore, a menu entry is created and a function to set the
option.  This function can also be called with the prefix
argument `\\[universal-argument] -' (or `\\[negative-argument]') to remove an item from the list,
or with the prefix argument `\\[universal-argument] \\[universal-argument]' to clear the entire list.
If the list is a list of files, the function can also be called
with the prefix argument `\\[universal-argument]' to store the full path.

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
     (put (quote ,option) (quote pandoc-list-type) (quote ,type))
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
             (cond
              ((and (listp prefix)
                    (eq (car prefix) 16)) ; C-u C-u
               (pandoc--set (quote ,option) nil)
               (message ,(concat description " removed.")))
              ((listp prefix) ; C-u or no prefix arg
               (let ((value ,(cond
                              ((eq type 'string)
                               `(read-string "Add value: " nil nil (pandoc--get (quote ,option))))
                              ((eq type 'file)
                               `(pandoc--read-file-name "Add file: " default-directory (not prefix))))))
                 (pandoc--set (quote ,option) value)
                 (message ,(concat prompt " \"%s\" added.") value)))
              ((eq prefix '-)
               (let ((value (completing-read "Remove item: " (pandoc--get (quote ,option)) nil t)))
                 (pandoc--remove-from-list-option (quote ,option) value)
                 (message ,(concat prompt " \"%s\" removed.") value))))))))

(defmacro define-pandoc-alist-option (option hydra description prompt)
  "Define OPTION as an alist option.
The option is added to `pandoc--options' and `pandoc--alist-options'.
Furthermore, a menu entry is created and a function to set the
option.  This function can also be called with the prefix
argument `\\[universal-argument] -' (or `\\[negative-argument]') to remove an item from the list,
or with the prefix argument `\\[universal-argument] \\[universal-argument]' to clear the entire list.

HYDRA is a list describing how the option must be added to one of
the hydras.  The first element is a symbol naming the hydra (and
menu) to which the option must be added, The second element is a
string of one character, the key by which the option will be
available in the hydra, and the third is a format string
describing the width of the option (which must be the same for
all options in a single hydra).

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  DESCRIPTION is the
description for the option's submenu.  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"Set/Change \" and
\"Unset \" can be added before it."

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
             (if (and (listp prefix)
                      (eq (car prefix) 16)) ; C-u C-u
                 (progn
                   (pandoc--set (quote ,option) nil)
                   (message ,(concat description " removed")))
               ;; for prefix argument - or no prefix argument
               (let ((var (completing-read (concat ,prompt ": ") (pandoc--get (quote ,option)))))
                 (when (and var (not (string= "" var)))
                   (let ((value (if (eq prefix '-)
                                    nil
                                  (read-string "Value: " nil nil (cdr (assq var (pandoc--get (quote ,option))))))))
                     (when (string= value "") ;; strings may be empty (which corresponds to boolean True in Pandoc)
                       (setq value t))
                     (pandoc--set (quote ,option) (cons var value))
                     (message ,(concat prompt " `%s' \"%s\".") var (if value
                                                                       (format "added with value `%s'" value)
                                                                     "removed"))))))))))

(defmacro define-pandoc-choice-option (option hydra prompt choices &optional output-formats)
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
                 :active ,(if output-formats
                              `(quote (member (pandoc--get 'write) (quote ,output-formats)))
                            t)
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
contain a \"%s\" specifier for the strings to be tabulated.  Note
that if FMT-STR is provided, COLWIDTH is only used to calculate
the number of rows and columns, not for padding the strings.  The
calling function must then ensure that the strings are of
equal length.

COLSEP is the string placed between two columns.  It defaults to
two spaces.  The length of this string is taken into account when
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
(define-pandoc-file-option   abbreviations           (reader "a" "%-23s")      "Abbreviations File")
(define-pandoc-switch        strip-empty-paragraphs  (reader "e" "%-23s")      "Strip Empty Paragraphs")
(define-pandoc-choice-option track-changes           (reader "T" "%-23s")      "Track Changes" ("accept" "reject" "all") ("docx"))
(define-pandoc-number-option tab-stop                (reader "t" "%-23s")      "Tab Stop Width")
(define-pandoc-switch        preserve-tabs           (reader "p" "%-23s")      "Preserve Tabs")
(define-pandoc-switch        normalize               (reader "n" "%-23s")      "Normalize Document")
(define-pandoc-list-option   metadata-file           (reader "M" "%-23s") file "Metadata File" "Metadata File")
(define-pandoc-alist-option  metadata                (reader "m" "%-23s")      "Metadata" "Metadata item")
(define-pandoc-list-option   filter                  (reader "f" "%-23s") file "Filters" "Filter")
(define-pandoc-list-option   lua-filter              (reader "l" "%-23s") file "Lua Filters" "Lua Filter")
(define-pandoc-string-option default-image-extension (reader "i" "%-23s")      "Default Image Extension")
(define-pandoc-string-option indented-code-classes   (reader "c" "%-23s")      "Indented Code Classes")
(define-pandoc-number-option shift-heading-level-by  (reader "h" "%-23s")      "Header Level Shift")
(define-pandoc-number-option base-header-level       (reader "h" "%-23s")      "Base Header Level*")
(define-pandoc-switch        old-dashes              (reader "o" "%-23s")      "Use Old-style Dashes")
(define-pandoc-switch        smart                   (reader "s" "%-23s")      "Smart*") ; obsolete
(define-pandoc-switch        parse-raw               (reader "r" "%-23s")      "Parse Raw*") ; obsolete
;; extract-media

;; TODO for data-dir, output-dir and extract-media, a macro define-pandoc-dir-option might be useful.


;;; General writer options
(define-pandoc-switch        no-check-certificate  (writer "N" "%-24s")        "Do Not Check Certificates")
(define-pandoc-switch        strip-comments        (writer "C" "%-24s")        "Strip Comments")
(define-pandoc-switch        verbose               (writer "V" "%-24s")        "Verbose output") ; Pandoc's README places this in the general options
(define-pandoc-list-option   resource-path         (writer "r" "%-24s") string "Resource Path" "Resource Path")
(define-pandoc-alist-option  request-header        (writer "R" "%-24s")        "HTTP Request Header" "Request Header")
(define-pandoc-file-option   include-after-body    (writer "A" "%-24s")        "Include After Body") ; Also allows URL since Pandoc 2.6.
(define-pandoc-file-option   include-before-body   (writer "B" "%-24s")        "Include Before Body") ; Also allows URL since Pandoc 2.6.
(define-pandoc-file-option   include-in-header     (writer "H" "%-24s")        "Include Header") ; Also allows URL since Pandoc 2.6.
(define-pandoc-file-option   syntax-definition     (writer "y" "%-24s")        "Syntax Definition File")
(define-pandoc-string-option highlight-style       (writer "S" "%-24s")        "Highlighting Style")
(define-pandoc-switch        no-highlight          (writer "h" "%-24s")        "No Highlighting")
(define-pandoc-number-option toc-depth             (writer "D" "%-24s")        "TOC Depth")
(define-pandoc-switch        table-of-contents     (writer "T" "%-24s")        "Table of Contents")
(define-pandoc-number-option columns               (writer "c" "%-24s")        "Column Width")
(define-pandoc-switch        no-wrap               (writer "W" "%-24s")        "No Wrap")
(define-pandoc-choice-option wrap                  (writer "w" "%-24s")        "Wrap"                ("auto" "none" "preserve"))
(define-pandoc-choice-option eol                   (writer "e" "%-24s")        "Line Endings Style"  ("crlf" "lf" "native"))
(define-pandoc-number-option dpi                   (writer "d" "%-24s")        "DPI")
(define-pandoc-alist-option  variable              (writer "v" "%-24s")        "Variables"           "Variable")
(define-pandoc-file-option   template              (writer "t" "%-24s")        "Template File")
(define-pandoc-switch        standalone            (writer "s" "%-24s")        "Standalone")
;; print-default-template : not actually included
;; print-default-data-file : not actually included
;; print-highlight-style : not actually included


;;; Options affecting specific writers

;; general
(define-pandoc-choice-option ipynb-output       (specific "p" "%-21s")        "Jupyter Output Cells" ("best" "all" "none") ("ipynb"))
(define-pandoc-list-option   pdf-engine-opt     (specific "o" "%-21s") string "PDF Options" "PDF Option")
(define-pandoc-choice-option pdf-engine         (specific "e" "%-21s")        "PDF Engine"
  ("pdflatex" "lualatex" "xelatex" "wkhtmltopdf" "weasyprint" "prince" "context" "pdfroff"))
(define-pandoc-file-option   reference-doc      (specific "R" "%-21s")        "Reference Doc")
(define-pandoc-file-option   reference-docx     (specific "d" "%-21s")        "Reference docx File*") ;obsolete
(define-pandoc-file-option   reference-odt      (specific "O" "%-21s")        "Reference ODT File*") ;obsolete
(define-pandoc-number-option slide-level        (specific "H" "%-21s")        "Slide Level Header")
(define-pandoc-switch        incremental        (specific "i" "%-21s")        "Incremental")
(define-pandoc-switch        number-sections    (specific "n" "%-21s")        "Number Sections")
(define-pandoc-switch        atx-headers        (specific "a" "%-21s")        "Use ATX-style Headers*")
(define-pandoc-choice-option markdown-headings  (specific "h" "%-21s")        "Markdown Headings" ("atx" "setext")
  ("markdown" "markdown_github" "markdown_mmd" "markdown_phpextra" "markdown_strict"))
(define-pandoc-switch        reference-links    (specific "r" "%-21s")        "Reference Links")
(define-pandoc-choice-option reference-location (specific "l" "%-21s")        "Reference Location" ("block" "section" "document")
  ("markdown" "markdown_github" "markdown_mmd" "markdown_phpextra" "markdown_strict"))
(define-pandoc-choice-option top-level-division (specific "t" "%-21s")        "Top Level Division" ("section" "part" "chapter")
  ("latex" "context" "docbook" "docbook5" "tei"))

;; html-based
(define-pandoc-list-option   css               (html "c" "%-31s") file "CSS Style Sheet" "CSS")
(define-pandoc-string-option title-prefix      (html "t" "%-31s")      "Title prefix")
(define-pandoc-string-option id-prefix         (html "i" "%-31s")      "ID prefix")
(define-pandoc-choice-option email-obfuscation (html "e" "%-31s")      "Email Obfuscation" ("none" "javascript" "references") ("html" "html5" "s5" "slidy" "slideous" "dzslides" "revealjs"))
(define-pandoc-switch        section-divs      (html "d" "%-31s")      "Wrap Sections in <div> Tags")
(define-pandoc-string-option number-offset     (html "o" "%-31s")      "Number Offsets")
(define-pandoc-switch        ascii             (html "a" "%-31s")      "Use Only ASCII")
(define-pandoc-switch        html-q-tags       (html "Q" "%-31s")      "Use <q> Tags for Quotes in HTML")
(define-pandoc-switch        self-contained    (html "s" "%-31s")      "Self-contained Document")

;; TeX-based (LaTeX, ConTeXt)
(define-pandoc-list-option   latex-engine-opt (tex "o" "%-30s") string "LaTeX Options*" "LaTeX Option") ;obsolete
(define-pandoc-choice-option latex-engine     (tex "e" "%-30s")        "LaTeX Engine*" ("pdflatex" "xelatex" "lualatex") ("latex" "beamer" "context")) ; obsolete
(define-pandoc-switch        listings         (tex "L" "%-30s")        "Use LaTeX listings Package")
(define-pandoc-switch        no-tex-ligatures (tex "l" "%-30s")        "Do Not Use TeX Ligatures")
(define-pandoc-switch        chapters         (tex "c" "%-30s")        "Top-level Headers Are Chapters")

;; epub
(define-pandoc-file-option   epub-subdirectory  (epub "d" "%-18s")      "EPub Subdirectory")
(define-pandoc-number-option epub-chapter-level (epub "c" "%-18s")      "EPub Chapter Level")
(define-pandoc-list-option   epub-embed-font    (epub "f" "%-18s") file "EPUB Fonts"         "EPUB Embedded Font")
(define-pandoc-file-option   epub-metadata      (epub "m" "%-18s")      "EPUB Metadata File")
(define-pandoc-file-option   epub-cover-image   (epub "C" "%-18s")      "EPUB Cover Image")
(define-pandoc-file-option   epub-stylesheet    (epub "s" "%-18s")      "EPUB Style Sheet" t)


;;; Citation rendering
(define-pandoc-switch      biblatex               (citations "l" "%-27s")      "Use BibLaTeX")
(define-pandoc-switch      natbib                 (citations "n" "%-27s")      "Use NatBib")
(define-pandoc-file-option citation-abbreviations (citations "a" "%-27s")      "Citation Abbreviations File")
(define-pandoc-file-option csl                    (citations "C" "%-27s")      "CSL File")
(define-pandoc-list-option bibliography           (citations "B" "%-27s") file "Bibliography Files"          "Bibliography File")
(define-pandoc-switch      citeproc               (citations "c" "%-27s")      "Process Citations")

;;; Math rendering in HTML
(define-pandoc-string-option katex-stylesheet (math "K" "%-18s") "KaTeX Stylesheet"    t)
(define-pandoc-string-option katex            (math "k" "%-18s") "KaTeX URL"           t)
(define-pandoc-string-option webtex           (math "w" "%-18s") "WebTeX URL"          t)
(define-pandoc-string-option mimetex          (math "M" "%-18s") "MimeTeX CGI Script*" t)
(define-pandoc-switch        gladtex          (math "g" "%-18s") "gladTeX")
(define-pandoc-string-option mathjax          (math "J" "%-18s") "MathJax URL"         t)
(define-pandoc-string-option jsmath           (math "j" "%-18s") "jsMath URL*"         t)
(define-pandoc-switch        mathml           (math "m" "%-18s") "MathML URL")
(define-pandoc-string-option latexmathml      (math "L" "%-18s") "LaTeXMathML URL*"    t)

(provide 'pandoc-mode-utils)

;;; pandoc-mode-utils.el ends here
