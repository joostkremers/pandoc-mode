;;; pandoc-mode-utils.el --- Part of `pandoc-mode'  -*- lexical-binding: t -*-

;; Copyright (c) 2009-2025 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

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

(require 'cl-lib)

(defgroup pandoc nil
  "Minor mode for interacting with pandoc."
  :group 'text)

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

(defcustom pandoc-project-file-name "Project"
  "Base name of project defaults files.
When creating a defaults file for a project, use this name as the base
name."
  :group 'pandoc
  :type 'string)

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
    (textile-mode   . "textile")
    (typst-ts-mode  . "typst"))
  "List of major modes and their default pandoc input formats."
  :group 'pandoc
  :type '(repeat (cons (symbol :tag "Major mode") (string :tag "Input format"))))

;; The formats known to Pandoc are defined in `pandoc--formats'.  They are divided
;; into different categories.  From this list, the input and output format menus
;; are created, which both consist of a main menu listing the categories and
;; submenus for each category.
;;
;; Note: the keys "b" and "q" cannot be used, because they are used in the
;; transients for returning to the higher level menu and for quitting,
;; respectively.

(defvar pandoc--formats
  '(("markdown" "Markdown Formats" "m"
     ("markdown"               "Pandoc Markdown"               "m" both)
     ("markdown_mmd"           "MultiMarkdown"                 "M" both)
     ("markdown_phpextra"      "PHP Markdown Extra"            "P" both)
     ("markdown_strict"        "Markdown (original)"           "S" both)
     ("commonmark"             "CommonMark"                    "C" both)
     ("commonmark_x"           "CommonMark with extensions"    "x" both)
     ("gfm"                    "GitHub-flavoured Markdown"     "g" both)
     ("djot"                   "Djot"                          "d" both)
     ("markua"                 "Markua"                        "u" output)
     ("markdown_github"        "Markdown (Github; obsolete)"   "G" both))

    ("html" "HTML Formats" "h"
     ("html"                   "HTML (default)"               "h" both)
     ("html4"                  "HTML4"                        "t" output)
     ("html5"                  "HTML5"                        "H" output)
     ("chunkedhtml"            "Chunked HTML"                 "C" output))

    ("slide-show" "Slide Show Formats" "s"
     ("beamer"                 "Beamer"                       "B" output) ; Also under TeX
     ("dzslides"               "DZSlides"                     "d" output)
     ("revealjs"               "RevealJS"                     "j" output)
     ("pptx"                   "MS PowerPoint"                "p" output)
     ("s5"                     "S5 HTML/JS"                   "s" output)
     ("slideous"               "Slideous"                     "u" output)
     ("slidy"                  "Slidy"                        "y" output))

    ("wiki" "Wiki Formats" "w"
     ("creole"                 "Creole 1.0"                   "c" input)
     ("dokuwiki"               "DokuWiki"                     "d" both)
     ("jira"                   "JiraWiki"                     "j" both)
     ("mediawiki"              "MediaWiki"                    "m" both)
     ("tikiwiki"               "TikiWiki"                     "t" input)
     ("twiki"                  "Twiki"                        "T" input)
     ("vimwiki"                "Vimwiki"                      "v" input)
     ("xwiki"                  "Xwiki"                        "x" output)
     ("zimwiki"                "ZimWiki"                      "z" output))

    ("wordprocessor" "Wordprocessor Formats" "W"
     ("docx"                   "MS Word (docx)"               "d" both)
     ("icml"                   "InDesign ICML"                "i" output)
     ("odt"                    "LibreOffice Text Document"    "l" both)
     ("opendocument"           "OpenDocument XML"             "o" output)
     ("rtf"                    "Rich Text Format"             "r" both)
     ("endnotexml"             "EndNote XML"                  "e" input))

    ("tex" "Typesetting Formats" "t"
     ("latex"                  "LaTeX"                        "l"  both)
     ("beamer"                 "Beamer Slide Show"            "s"  output) ; Also under Slide Shows Formats.
     ("context"                "ConTeXt"                      "c"  output)
     ("texinfo"                "TeXinfo"                      "i"  output) ; Also under Documentation Formats.
     ("typst"                  "Typst"                        "y"  both)
     ("bibtex"                 "BibTeX"                       "Bt" both)
     ("biblatex"               "Biblatex"                     "Bl" both))

    ("ebook" "E-Book Formats" "e"
     ("epub"                   "EPUB (default)"               "e" both)
     ("epub2"                  "EPUB2 E-Book"                 "p" output)
     ("epub3"                  "EPUB3 E-Book"                 "E" output)
     ("fb2"                    "FictionBook2"                 "f" both))

    ("text" "Text-Based Formats" "T"
     ("asciidoc"               "AsciiDoc"                     "a" output)
     ("asciidoc_legacy"        "AsciiDoc (lecagy)"            "A" output)
     ("csv"                    "CSV"                          "c" input)
     ("tsv"                    "TSV"                          "t" input)
     ("plain"                  "Plain Text"                   "p" output)
     ("ansi"                   "Plain Text with ANSI codes"   "P" output)
     ("rst"                    "reStructuredText"             "r" both)
     ("textile"                "Textile"                      "T" both)
     ("t2t"                    "txt2tags"                     "2" input))

    ("documentation" "Documentation Formats" "d"
     ("docbook"                "DocBook XML"                  "d" input) ; docbook and docbook4 share the same key.
     ("docbook4"               "DocBook XML v.4"              "d" output) ; They won't appear in the same menu anyway.
     ("docbook5"               "DocBook XML v.5"              "D" output)
     ("haddock"                "Haddock"                      "h" both)
     ("man"                    "Man Page"                     "m" both)
     ("ms"                     "Groff MS"                     "g" output)
     ("tei"                    "TEI"                          "t" output)
     ("texinfo"                "TeXinfo"                      "i" output)) ; Also under TeX Formats.

    ("emacs" "Emacs-based Formats" "E"
     ("muse"                   "Muse"                         "m" both)
     ("org"                    "Org-mode"                     "o" both))

    ("jats" "JATS formats" "j"
     ("jats"                   "Archiving Tag Set"            "j" both)
     ("jats_articleauthoring"  "Article Authoring Tag Set"    "a" output)
     ("jats_publishing"        "Publishing Tag Set"           "p" output)
     ("jats_archiving"         "Archiving Tag Set"            "x" output)
     ("bits"                   "BITS XML"                     "b" input))

    ("misc" "Miscellaneous Formats" "M"
     ("ipynb"                  "Jupyter Notebook"             "p" both)
     ("json"                   "JSON"                         "j" both)
     ("csljson"                "CSL-JSON"                     "c" both)
     ("native"                 "Native Haskell"               "n" both)
     ("opml"                   "OPML"                         "o" both)
     ("ris"                    "RIS bibliography"             "r" input)))
  "List of Pandoc formats, their descriptions and transient shortcut keys.")

(defun pandoc--extract-formats (io)
  "Extract the input or output formats in `pandoc--formats'.
IO is a symbol, either `input' or `output'.  Return a list of formats."
  (apply #'append (mapcar (lambda (formats)
                            (seq-filter (lambda (it)
                                          (memq (car (last it)) `(,io both)))
                                        (drop 3 formats)))
                          pandoc--formats)))

(defvar pandoc--input-formats-menu
  (mapcar (lambda (f)
            (cons (cadr f) (car f)))
          (pandoc--extract-formats 'input))
  "List of items in pandoc-mode's input format menu.")

(defcustom pandoc-output-format-extensions
  '(("ansi"                  ".txt")
    ("asciidoc"              ".txt")
    ("asciidoc_legacy"       ".txt")
    ("beamer"                ".tex")
    ("bibtex"                ".bib")
    ("biblatex"              ".bib")
    ("chunkedhtml"           ".html")
    ("commonmark"            ".md")
    ("commonmark_x"          ".md")
    ("context"               ".tex")
    ("csljson"               ".json")
    ("djot"                  ".dj")
    ("docbook"               ".xml")
    ("docbook4"              ".xml")
    ("docbook5"              ".xml")
    ("docx"                  ".docx")
    ("dokuwiki"              ".txt")
    ("dzslides"              ".html")
    ("epub"                  ".epub")
    ("epub2"                 ".epub")
    ("epub3"                 ".epub")
    ("fb2"                   ".fb2")
    ("gfm"                   ".md")
    ("haddock"               ".hs")
    ("html"                  ".html")
    ("html4"                 ".html")
    ("html5"                 ".html")
    ("icml"                  ".icml")
    ("ipynb"                 ".ipynb")
    ("jats"                  ".xml")
    ("jats_archiving"        ".xml")
    ("jats_articleauthoring" ".xml")
    ("jats_publishing"       ".xml")
    ("jira"                  ".txt")
    ("json"                  ".json")
    ("latex"                 ".tex")
    ("man"                   "")
    ("markdown"              ".md")
    ("markdown_github"       ".md")
    ("markdown_mmd"          ".md")
    ("markdown_phpextra"     ".md")
    ("markdown_strict"       ".md")
    ("markua"                ".txt") ; Not sure about this one.
    ("mediawiki"             ".mw")
    ("ms"                    ".ms")
    ("muse"                  ".muse")
    ("native"                ".hs")
    ("odt"                   ".odt")
    ("opendocument"          ".odf")
    ("opml"                  ".opml")
    ("org"                   ".org")
    ("pdf"                   ".pdf")
    ("plain"                 ".txt")
    ("pptx"                  ".pptx")
    ("revealjs"              ".html")
    ("rst"                   ".rst")
    ("rtf"                   ".rtf")
    ("s5"                    ".html")
    ("slideous"              ".html")
    ("slidy"                 ".html")
    ("tei"                   ".xml")
    ("texinfo"               ".texi")
    ("textile"               ".textile")
    ("typst"                 ".typ")
    ("xwiki"                 ".txt") ; Probably.
    ("zimwiki"               ".txt"))
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
    ("opendocument"      "libreoffice")
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

(defvar pandoc--pdf-able-formats '("latex" "context" "beamer" "html" "ms" "typst")
  "List of output formats that can be used to generate pdf output.")

(defvar pandoc--extensions-alist
  '(("abbreviations" .
     ("markdown_phpextra" | "plain" "opml" "ipynb" "markdown_strict"
      "markdown_github" "markdown_mmd" "markdown"))

    ("alerts" . ("gfm" "commonmark_x" | "commonmark"))

    ("all_symbols_escapable" .
     ("opml" "ipynb" "markdown_github" "markdown_mmd" "markdown" | "plain"
      "markdown_strict" "markdown_phpextra"))

    ("amuse" . ("muse" |))

    ("angle_brackets_escapable" .
     (| "plain" "opml" "ipynb" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown"))

    ("ascii_identifiers" .
     (| "vimwiki" "twiki" "tikiwiki" "textile" "rst" "plain" "org" "opml"
        "odt" "muse" "mediawiki" "latex" "ipynb" "html5" "html4" "html" "gfm"
        "epub3" "epub" "epub" "docuwiki" "context" "commonmark_x" "commonmark"
        "beamer" "asciidoc" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown" "docx"))

    ("attributes" . ("commonmark_x" | "gfm" "commonmark"))

    ("auto_identifiers" .
     ("vimwiki" "twiki" "tikiwiki" "textile" "rst" "org" "opml" "odt" "muse"
      "mediawiki" "latex" "jats_publishing" "jats_articleauthoring"
      "jats_archiving" "jats" "ipynb" "html5" "html4" "html" "docuwiki"
      "context" "beamer" "asciidoc" "markdown_github" "markdown_mmd"
      "markdown" "docx" | "plain" "epub3" "epub" "epub" "markdown_strict"
      "markdown_phpextra"))

    ("autolink_bare_uris" .
     ("ipynb" "gfm" "markdown_github" | "plain" "opml" "commonmark_x"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_mmd"
      "markdown"))

    ("backtick_code_blocks" .
     ("opml" "ipynb" "markdown_github" "markdown_mmd" "markdown" | "plain"
      "markdown_strict" "markdown_phpextra"))

    ("blank_before_blockquote" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("blank_before_header" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("bracketed_spans" .
     ("opml" "commonmark_x" "markdown" | "plain" "ipynb" "gfm" "commonmark"
      "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("citations" .
     ("typst" "org" "opml" "markdown" | "plain" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd" "docx"))

    ("compact_definition_lists" .
     (| "plain" "opml" "ipynb" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown"))

    ("definition_lists" .
     ("plain" "opml" "commonmark_x" "markdown_phpextra" "markdown_mmd"
      "markdown" | "ipynb" "gfm" "commonmark" "markdown_strict"
      "markdown_github"))

    ("east_asian_line_breaks" .
     (| "zimwiki" "xwiki" "vimwiki" "typst" "twiki" "tsv" "tikiwiki" "textile"
        "texinfo" "tei" "t2t" "slidy" "slideous" "s5" "rtf" "rst" "ris"
        "revealjs" "pptx" "plain" "org" "opml" "opendocument" "odt" "native"
        "muse" "ms" "mediawiki" "man" "latex" "json" "jira" "jats_publishing"
        "jats_articleauthoring" "jats_archiving" "jats" "ipynb" "icml" "html5"
        "html4" "html" "haddock" "gfm" "fb2" "epub3" "epub" "epub"
        "endnotexml" "dzslides" "docuwiki" "docbook5" "docbook4" "docbook"
        "djot" "csv" "csljson" "creole" "context" "commonmark_x" "commonmark"
        "chunkedhtml" "bits" "bibtex" "biblatex" "beamer" "asciidoctor"
        "asciidoc_legacy" "asciidoc" "ansi" "markua" "markdown_strict"
        "markdown_phpextra" "markdown_github" "markdown_mmd" "markdown" "docx"))

    ("element_citations" .
     (| "jats_publishing" "jats_articleauthoring" "jats_archiving" "jats"))

    ("emoji" .
     ("gfm" "commonmark_x" "markdown_github" | "plain" "opml" "ipynb"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_mmd"
      "markdown"))

    ("empty_paragraphs" .
     (| "opendocument" "odt" "latex" "html5" "html4" "html" "epub3" "epub"
        "epub" "beamer" "docx"))

    ("epub_html_exts" . ("epub3" "epub" "epub" | "html5" "html4" "html"))

    ("escaped_line_breaks" .
     ("opml" "markdown" | "plain" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("example_lists" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("fancy_lists" .
     ("plain" "opml" "commonmark_x" "markdown" | "org" "ipynb" "gfm"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_github"
      "markdown_mmd"))

    ("fenced_code_attributes" .
     ("opml" "markdown" | "plain" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("fenced_code_blocks" .
     ("opml" "ipynb" "markdown_phpextra" "markdown_github" "markdown" |
      "plain" "markdown_strict" "markdown_mmd"))

    ("fenced_divs" .
     ("opml" "commonmark_x" "markdown" | "plain" "ipynb" "gfm" "commonmark"
      "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("footnotes" .
     ("opml" "gfm" "commonmark_x" "markdown_phpextra" "markdown_github"
      "markdown_mmd" "markdown" | "plain" "ipynb" "commonmark"
      "markdown_strict"))

    ("four_space_rule" .
     (| "plain" "opml" "ipynb" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown"))

    ("gfm_auto_identifiers" .
     ("ipynb" "gfm" "commonmark_x" "markdown_github" | "vimwiki" "twiki"
      "tikiwiki" "textile" "rst" "plain" "org" "opml" "odt" "muse" "mediawiki"
      "latex" "html5" "html4" "html" "epub3" "epub" "epub" "docuwiki"
      "context" "commonmark" "beamer" "asciidoc" "markdown_strict"
      "markdown_phpextra" "markdown_mmd" "markdown" "docx"))

    ("grid_tables" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("gutenberg" .
     (| "plain" "opml" "ipynb" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown"))

    ("hard_line_breaks" .
     (| "plain" "opml" "ipynb" "gfm" "commonmark_x" "commonmark"
        "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"
        "markdown"))

    ("header_attributes" .
     ("opml" "markdown_phpextra" "markdown" | "plain" "ipynb"
      "markdown_strict" "markdown_github" "markdown_mmd"))

    ("ignore_line_breaks" .
     (| "plain" "opml" "ipynb" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown"))

    ("implicit_figures" .
     ("plain" "opml" "markdown_mmd" "markdown" | "ipynb" "gfm" "commonmark_x"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_github"))

    ("implicit_header_references" .
     ("opml" "commonmark_x" "markdown_mmd" "markdown" | "plain" "ipynb" "gfm"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_github"))

    ("inline_code_attributes" .
     ("opml" "markdown" | "plain" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("inline_notes" .
     ("opml" "markdown" | "plain" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("intraword_underscores" .
     ("plain" "opml" "ipynb" "markdown_phpextra" "markdown_github"
      "markdown_mmd" "markdown" | "markdown_strict"))

    ("latex_macros" .
     ("plain" "opml" "latex" "beamer" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("line_blocks" .
     ("opml" "html5" "html4" "html" "markdown" | "plain" "ipynb" "epub3"
      "epub" "epub" "markdown_strict" "markdown_phpextra" "markdown_github"
      "markdown_mmd"))

    ("link_attributes" .
     ("opml" "markdown_phpextra" "markdown" | "plain" "ipynb"
      "markdown_strict" "markdown_github" "markdown_mmd"))

    ("lists_without_preceding_blankline" .
     ("ipynb" "markdown_github" | "plain" "opml" "markdown_strict"
      "markdown_phpextra" "markdown_mmd" "markdown"))

    ("literate_haskell" .
     (| "rst" "plain" "opml" "latex" "ipynb" "html5" "html4" "html" "epub3"
        "epub" "epub" "beamer" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown"))

    ("mark" .
     (| "plain" "opml" "ipynb" "markdown_strict" "markdown_phpextra"
        "markdown_github" "markdown_mmd" "markdown"))

    ("markdown_attribute" .
     ("markdown_phpextra" "markdown_mmd" | "plain" "opml" "ipynb"
      "markdown_strict" "markdown_github" "markdown"))

    ("markdown_in_html_blocks" .
     ("opml" "markdown" | "plain" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("mmd_header_identifiers" .
     ("markdown_mmd" | "plain" "opml" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown"))

    ("mmd_link_attributes" .
     ("markdown_mmd" | "plain" "opml" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown"))

    ("mmd_title_block" .
     ("markdown_mmd" | "plain" "opml" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown"))

    ("multiline_tables" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("native_divs" .
     ("opml" "html5" "html4" "html" "epub3" "epub" "epub" "markdown" | "plain"
      "ipynb" "markdown_strict" "markdown_phpextra" "markdown_github"
      "markdown_mmd"))

    ("native_numbering" . (| "opendocument" "odt" "docx"))

    ("native_spans" .
     ("opml" "html5" "html4" "html" "epub3" "epub" "epub" "markdown" | "plain"
      "ipynb" "markdown_strict" "markdown_phpextra" "markdown_github"
      "markdown_mmd"))

    ("ntb" . (| "context"))

    ("old_dashes" .
     ("textile" | "plain" "opml" "ipynb" "markdown_strict" "markdown_phpextra"
      "markdown_github" "markdown_mmd" "markdown"))

    ("pandoc_title_block" .
     ("opml" "markdown" | "plain" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("pipe_tables" .
     ("opml" "ipynb" "gfm" "commonmark_x" "markdown_phpextra"
      "markdown_github" "markdown_mmd" "markdown" | "plain" "commonmark"
      "markdown_strict"))

    ("raw_attribute" .
     ("opml" "commonmark_x" "markdown_mmd" "markdown" | "plain" "ipynb" "gfm"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_github"))

    ("raw_html" .
     ("opml" "ipynb" "gfm" "epub3" "epub" "epub" "commonmark_x" "commonmark"
      "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"
      "markdown" | "plain" "html5" "html4" "html"))

    ("raw_markdown" . (| "ipynb"))

    ("raw_tex" .
     ("opml" "markdown" | "textile" "plain" "latex" "ipynb" "html5" "html4"
      "html" "epub3" "epub" "epub" "context" "beamer" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("rebase_relative_paths" .
     (| "plain" "opml" "ipynb" "gfm" "commonmark_x" "commonmark"
        "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"
        "markdown"))

    ("short_subsuperscripts" .
     ("markdown_mmd" | "plain" "opml" "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown"))

    ("shortcut_reference_links" .
     ("opml" "ipynb" "markdown_strict" "markdown_phpextra" "markdown_github"
      "markdown_mmd" "markdown" | "plain"))

    ("simple_tables" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("smart" .
     ("textile" "opml" "latex" "context" "commonmark_x" "beamer" "markdown" |
      "twiki" "rst" "plain" "org" "mediawiki" "ipynb" "html5" "html4" "html"
      "gfm" "epub3" "epub" "epub" "commonmark" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("sourcepos" . (| "gfm" "djot" "commonmark_x" "commonmark"))

    ("space_in_atx_header" .
     ("opml" "ipynb" "markdown_github" "markdown" | "plain" "markdown_strict"
      "markdown_phpextra" "markdown_mmd"))

    ("spaced_reference_links" .
     ("markdown_strict" "markdown_phpextra" "markdown_mmd" | "plain" "opml"
      "ipynb" "markdown_github" "markdown"))

    ("startnum" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("strikeout" .
     ("plain" "opml" "ipynb" "gfm" "commonmark_x" "markdown_github" "markdown"
      | "commonmark" "markdown_strict" "markdown_phpextra" "markdown_mmd"))

    ("styles" . (| "docx"))

    ("subscript" .
     ("opml" "commonmark_x" "markdown_mmd" "markdown" | "plain" "ipynb" "gfm"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_github"))

    ("superscript" .
     ("opml" "commonmark_x" "markdown_mmd" "markdown" | "plain" "ipynb" "gfm"
      "commonmark" "markdown_strict" "markdown_phpextra" "markdown_github"))

    ("table_captions" .
     ("plain" "opml" "markdown" | "ipynb" "markdown_strict"
      "markdown_phpextra" "markdown_github" "markdown_mmd"))

    ("tagging" . (| "context"))

    ("task_lists" .
     ("org" "opml" "ipynb" "gfm" "commonmark_x" "markdown_github" "markdown" |
      "plain" "latex" "html5" "html4" "html" "epub3" "epub" "epub"
      "commonmark" "beamer" "markdown_strict" "markdown_phpextra"
      "markdown_mmd"))

    ("tex_math_dollars" .
     ("opml" "ipynb" "gfm" "commonmark_x" "markdown_mmd" "markdown" | "plain"
      "html5" "html4" "html" "epub3" "epub" "epub" "docuwiki" "commonmark"
      "markdown_strict" "markdown_phpextra" "markdown_github"))

    ("tex_math_double_backslash" .
     ("markdown_mmd" | "plain" "opml" "ipynb" "html5" "html4" "html" "epub3"
      "epub" "epub" "markdown_strict" "markdown_phpextra" "markdown_github"
      "markdown"))

    ("tex_math_gfm" . ("gfm" | "commonmark_x" "commonmark"))

    ("tex_math_single_backslash" .
     (| "plain" "opml" "ipynb" "html5" "html4" "html" "epub3" "epub" "epub"
        "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"
        "markdown"))

    ("wikilinks_title_after_pipe" .
     (| "plain" "opml" "ipynb" "gfm" "commonmark_x" "commonmark"
        "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"
        "markdown"))

    ("wikilinks_title_before_pipe" .
     (| "plain" "opml" "ipynb" "gfm" "commonmark_x" "commonmark"
        "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd"
        "markdown"))

    ("xrefs_name" . (| "opendocument" "odt"))

    ("xrefs_number" . (| "opendocument" "odt"))

    ("yaml_metadata_block" .
     ("opml" "gfm" "commonmark_x" "markdown" | "plain" "ipynb" "commonmark"
      "markdown_strict" "markdown_phpextra" "markdown_github" "markdown_mmd")))
  "Pandoc extensions alist.
Each extension maps to a list with the formats in which the extension is
supported.  Before the pipe symbol are the formats in which the
extension in active by default, after the pipe symbol are the formats in
which the extension is inactive by default.")

(defun pandoc-add-extensions (format)
  "Add extensions for FORMAT."
  (with-temp-buffer
    (insert-file-contents "extensions.txt")
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (flag (cdr (assoc (substring line 0 1) '(("+" . 1) ("-" . 2)))))
             (extension (substring line 1))
             (entry (assoc extension pandoc--extensions-alist)))
        (when (null entry)
          (setq entry (car (push (list extension nil nil) pandoc--extensions-alist))))
        (push format (nth flag entry)))
      (forward-line 1)))
  (with-current-buffer "extensions.eld"
    (erase-buffer)
    (insert (format "%S" pandoc--extensions-alist))
    (save-buffer)))

(defvar pandoc--cli-options nil
  "List of Pandoc command line options that do not need special treatment.
This includes all command line options except the list and alist
options, because they need to be handled separately in
`pandoc--format-all-options'.")

(defvar pandoc--filepath-options nil
  "List of options that have a file path as value.
These file paths are expanded before they are sent to Pandoc.  For
relative paths, the file's working directory is used as base directory.
The options are set by `define-pandoc-file-option'.")

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
  `((write . "native")))
"Pandoc option alist.
List of options and their default values.  For each buffer in which
pandoc-mode is activated, a buffer-local copy of this list is made that
stores the local values of the options.  The `define-pandoc-*-option'
functions add their options to this list with default value nil."

(defconst pandoc--html-math-methods '(("mathjax" . t)
                                      ("mathml")
                                      ("webtex" . t)
                                      ("katex" . t)
                                      ("gladtex"))
  "Possible values for the Pandoc option `html-math-method'.
If the cdr of an entry is t, the option can take an optional URL.")

(defvar-local pandoc--local-settings nil "A buffer-local variable holding a file's pandoc options.")

(defvar-local pandoc--settings-modified-flag nil "Non-nil means the current settings were modified and not saved.")

(defvar-local pandoc--latest-run nil
  "The output format and the output file created in the most recent call to Pandoc.")

(defvar pandoc--output-buffer-name " *Pandoc output*")
(defvar pandoc--log-buffer-name " *Pandoc log*")
(defvar pandoc--viewer-buffer-name " *Pandoc viewer*")

(defvar pandoc--options-menu nil
  "Auxiliary variable for creating the options menu.")

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

(defun pandoc--extension-active-marker (extension rw)
  "Return a marker indicating whether EXTENSION is active.
RW is either `read' or `write', indicating whether to take the
input or the output format."
  (if (pandoc--extension-active-p extension rw)
      pandoc-extension-active-marker
    pandoc-extension-inactive-marker))

(defun pandoc--extension-in-format-p (extension format &optional rw)
  "Check if EXTENSION is a default extension for FORMAT.
RW must be either `read' or `write', indicating whether FORMAT is
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
RW is either `read' or `write', indicating whether the read or
write extension is to be set."
  (setcdr (assoc extension (if (eq rw 'read)
                               (pandoc--get 'read-extensions)
                             (pandoc--get 'write-extensions)))
          value))

(defun pandoc--get-extension (extension rw)
  "Return the value of EXTENSION for RW.
RW is either `read' or `write', indicating whether the read or
write extension is to be queried."
  (cdr (assoc extension (if (eq rw 'read)
                            (pandoc--get 'read-extensions)
                          (pandoc--get 'write-extensions)))))

(defun pandoc--current-settings ()
  "Return a list of settings with non-default values."
  (let* ((remove-defaults (lambda (alist)
                            (seq-filter (lambda (option)
                                          (cdr option))
                                        alist)))
         (settings (copy-tree pandoc--local-settings))
         (read-extensions (assq 'read-extensions settings))
         (write-extensions (assq 'write-extensions settings)))
    (when read-extensions
      (setcdr read-extensions (funcall remove-defaults (cdr read-extensions))))
    (when write-extensions
      (setcdr write-extensions (funcall remove-defaults (cdr write-extensions))))
    (funcall remove-defaults settings)))

(defun pandoc--get-file-local-options (buffers)
  "Return all pandoc related file-local variables and their values.
These are file local variables beginning with `pandoc/'.  Return
value is an alist of (var . value) pairs.  The values are
searched in BUFFERS, which is a list of buffers.  The first value
found for a particular value is the one returned.  In other
words, a value from a buffer earlier in BUFFERS overrides the
value of a later buffer."
  (delq nil (mapcar (lambda (option)
                      (let ((var (intern (concat "pandoc/" (symbol-name (car option)))))
                            (bs buffers))
                        (while (and bs (not (local-variable-p var (car bs))))
                          (setq bs (cdr bs)))
                        (when bs
                          (cons var (buffer-local-value var (car bs))))))
                    pandoc--options)))

;; transient variables
(defvar pandoc--reader-transient-list nil)
(defvar pandoc--writer-transient-list nil)
(defvar pandoc--specific-transient-list nil)
(defvar pandoc--html-transient-list nil)
(defvar pandoc--epub-transient-list nil)
(defvar pandoc--obsolete-transient-list nil)
(defvar pandoc--citations-transient-list nil)

;; menu variables
(defvar pandoc--reader-menu-list nil)
(defvar pandoc--writer-menu-list nil)
(defvar pandoc--specific-menu-list nil)
(defvar pandoc--html-menu-list nil)
(defvar pandoc--epub-menu-list nil)
(defvar pandoc--obsolete-menu-list nil)
(defvar pandoc--citations-menu-list nil)

(defmacro define-pandoc-switch (option menu key description)
  "Create a binary option.
OPTION must be a symbol and must be identical to the long form of the
pandoc option (without dashes).  MENU is a symbol naming the menu to
which the switch should be added.  It can also be nil, in which case the
option is not added to any menu.  KEY is a string of one or two
characters, the key by which the option will be available in the
transient.  DESCRIPTION is the description of the option as it will
appear in the menu."
  (declare (indent defun))
  `(progn
     ,(when menu
        `(push ,(vector description `(pandoc--toggle (quote ,option))
                        :active t
                        :style 'toggle
                        :selected `(pandoc--get (quote ,option)))
               ,(intern (concat "pandoc--" (symbol-name menu) "-menu-list"))))
     (push (cons ,description (quote ,option)) pandoc--switches)
     (push (quote ,option) pandoc--cli-options)
     (push (list (quote ,option)) pandoc--options)
     ,(when menu
        `(push (quote ,(list key `(lambda () (interactive)
                                    (pandoc--toggle (quote ,option)))
                             :description `(lambda ()
                                             (format "%-35s[%s]" ,description (pandoc--pp-switch (quote ,option))))
                             :transient t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-transient-list"))))))

(defmacro define-pandoc-file-option (option menu key prompt)
  "Define OPTION as a file option.
The option is added to `pandoc--options', `pandoc--cli-options', and to
`pandoc--filepath-options'.  Furthermore, a menu entry is created under
MENU, which is a symbol naming the menu to which the option should be
added.  It can also be nil, in which case the option is not added to any
menu.  KEY is a string of one or two characters, the key by which the
option will be available in the transient.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"No \", \"Set \" and
\"Default \" can be added before it.  DEFAULT must be either nil
or t and indicates whether the option can have a default value."
  (declare (indent defun))
  `(progn
     (push (quote ,option) pandoc--filepath-options)
     (push (quote ,option) pandoc--cli-options)
     (push (list (quote ,option)) pandoc--options)
     ,(when menu
        `(push (list ,prompt
                     ,(vector (concat "No " prompt) `(pandoc--set (quote ,option) nil)
                              :active t
                              :style 'radio
                              :selected `(null (pandoc--get (quote ,option))))
                     ,(vector (concat "Set " prompt "...") (lambda ()
                                                             (interactive)
                                                             (pandoc-set-file-option nil option prompt))
                              :active t
                              :style 'radio
                              :selected `(stringp (pandoc--get (quote ,option)))))
               ,(intern (concat "pandoc--" (symbol-name menu) "-menu-list"))))
     ,(when menu
        `(push (quote ,(list key (lambda (prefix)
                                   (interactive "P")
                                   (pandoc-set-file-option prefix option prompt))
                             :description `(lambda ()
                                             (format "%-35s[%s]" ,prompt (pandoc--pp-option (quote ,option))))
                             :transient t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-transient-list"))))))

(defun pandoc-set-file-option (prefix option prompt)
  "Set file option OPTION interactively.
This function is meant to be called from an interactive function to do
the actual work.  PROMPT is used to prompt the user.  PREFIX is the raw
prefix argument from the calling function.  If it is nil, ask the user
for a file name.  If PREFIX is the negative prefix argument `\\[universal-argument] -' (or
`\\[negative-argument]'), unset the option.  If PREFIX is \\[universal-argument], store the file's full path.
If PREFIX is numeric , i.e., `\\[universal-argument] 1' or `M-1', read a string from the
user without completion.  This is useful for file options that can also
have a URL as argument."
  (pandoc--set option
               (cond
                ((eq prefix '-)     ; C-u - or M--
                 nil)
                ((numberp prefix)
                 (read-string (concat prompt ": ")))
                ;; otherwise no prefix or C-u
                (t (pandoc--read-file-name (concat prompt ": ") default-directory (not prefix))))))

(defmacro define-pandoc-number-option (option menu key prompt)
  "Define OPTION as a numeric option.
The option is added to `pandoc--options' and to `pandoc--cli-options'.
Furthermore, a menu entry is created under MENU, a symbol naming the
menu to which the option must be added.  It can also be nil, in which
case the option is not added to any menu.  KEY is a string of one or two
characters, the key by which the option will be available in the
transient.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"Default \" and \"Set \"
can be added before it."
  (declare (indent defun))
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--cli-options)
     ,(when menu
        `(push (list ,prompt
                     ,(vector (concat "Default " prompt) `(pandoc--set (quote ,option) nil)
                              :active t
                              :style 'radio
                              :selected `(null (pandoc--get (quote ,option))))
                     ,(vector (concat "Set " prompt "...") (lambda ()
                                                             (interactive)
                                                             (pandoc-set-number-option nil option prompt))
                              :active t
                              :style 'radio
                              :selected `(pandoc--get (quote ,option))))
               ,(intern (concat "pandoc--" (symbol-name menu) "-menu-list"))))
     ,(when menu
        `(push (quote ,(list key (lambda (prefix) (interactive "P")
                                   (pandoc-set-number-option prefix option prompt))
                             :description `(lambda ()
                                             (format "%-35s[%s]" ,prompt (pandoc--pp-option (quote ,option))))
                             :transient t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-transient-list"))))))

(defun pandoc-set-number-option (prefix option prompt)
  "Set number option OPTION interactively.
This function is meant to be called from an interactive function to do
the actual work.  PROMPT is used to prompt the user.  PREFIX is the raw
prefix argument from the calling function.  If PREFIX is `\\[universal-argument] -' (or
`\\[negative-argument]'), unset the option.  If PREFIX is nil, the user is prompted
for a value."
  (pandoc--set option
               (if (eq prefix '-)
                   nil
                 (string-to-number (read-string (concat prompt ": "))))))

(defmacro define-pandoc-string-option (option menu key prompt &optional default)
  "Define OPTION as a string option.
The option is added to `pandoc--options' and to `pandoc--cli-options'.
Furthermore, a menu entry is created under MENU, a symbol naming the
menu to which the option must be added.  It can also be nil, in which
case the option is not added to any menu.  KEY is a string of one or two
characters, the key by which the option will be available in the
transient.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"No \", \"Set \" and
\"Default \" can be added before it.  DEFAULT must be either NIL
or T and indicates whether the option can have a default value."
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--cli-options)
     ,(when menu
        `(push (list ,@(delq nil ; if DEFAULT is nil, we need to remove it from the list.
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
                                   (vector (concat "Set " prompt "...") (lambda ()
                                                                          (interactive)
                                                                          (pandoc-set-string-option nil option prompt default))
                                           :active t
                                           :style 'radio
                                           :selected `(stringp (pandoc--get (quote ,option)))))))
               ,(intern (concat "pandoc--" (symbol-name menu) "-menu-list"))))
     ,(when menu
        `(push (quote ,(list key (lambda (prefix)
                                   (interactive)
                                   (pandoc-set-string-option prefix option prompt default))
                             :description `(lambda ()
                                             (format "%-35s[%s]" ,prompt (pandoc--pp-option (quote ,option))))
                             :transient t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-transient-list"))))))

(defun pandoc-set-string-option (prefix option prompt default)
  "Set number option OPTION interactively.
This function is meant to be called from an interactive function to do
the actual work.  PROMPT is used to prompt the user.  PREFIX is the raw
prefix argument from the calling function.  If PREFIX is `\\[universal-argument] -' (or
`\\[negative-argument]'), unset the option.  If PREFIX is nil, the user is prompted
for a value.  With any other PREFIX argument, use the option's default
value, if it has one, otherwise unset it."
  ;; DEFAULT is either nil or t; if t, it just signals that the option
  ;; should be passed to Pandoc without a value.  Note that if DEFAULT is
  ;; nil, calling this function with a prefix argument other than `C-u -'
  ;; just sets the option to nil, which is the same as unsetting it.
  (pandoc--set option
               (cond
                ((eq prefix '-) nil)
                ((null prefix) (read-string (concat prompt ": ")))
                (t default))))

(defmacro define-pandoc-list-option (option menu key type description prompt)
  "Define OPTION as a list option.
The option is added to `pandoc--options' and `pandoc--list-options'.
Furthermore, a menu entry is created under MENU, a symbol naming the
menu to which the option must be added.  It can also be nil, in which
case the option is not added to any menu.  KEY is a string of one
character, the key by which the option will be available in the
transient.

OPTION must be a symbol and must be identical to the long form of the
pandoc option (without dashes).  TYPE specifies the kind of data that is
stored in the list.  Currently, possible values are `string', `file' and
`number'.  DESCRIPTION is the description for the option's submenu.
PROMPT is a string that is used to prompt for setting and unsetting the
option.  It must be formulated in such a way that the strings \"Add \",
\"Remove \" can be added before it."
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--list-options)
     (put (quote ,option) (quote pandoc-list-type) (quote ,type))
     ,(when menu
        `(push (list ,description
                     ,(vector (concat "Add " prompt) (lambda ()
                                                       (interactive)
                                                       (pandoc-set-list-option nil option prompt description type))
                              :active t)
                     ,(vector (concat "Remove " prompt) (list (intern (concat "pandoc-set-" (symbol-name option))) `(quote -))
                              :active `(pandoc--get (quote ,option))))
               ,(intern (concat "pandoc--" (symbol-name menu) "-menu-list"))))
     ,(when menu
        `(push (quote ,(list key (lambda (prefix)
                                   (interactive "P")
                                   (pandoc-set-list-option prefix option prompt description type))
                             :description `(lambda ()
                                             (format "%-35s[%s]" ,prompt (pandoc--pp-option (quote ,option))))
                             :transient t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-transient-list"))))))

(defun pandoc-set-list-option (prefix option prompt description type)
  "Set list option OPTION interactively.
Ask the user for a single item of OPTION and add/update the value of the
alist option.

This function is meant to be called from an interactive function to do
the actual work.  PROMPT is used to prompt the user, DESCRIPTION is used
to inform the user.  TYPE indicates the type of option, currently only
the symbols `string', `file' and number are distinguished.

PREFIX is the raw prefix argument from the calling function.  If it is
nil, a new item is added to the list.  If it is the negative prefix
argument `\\[universal-argument] -' (or `\\[negative-argument]'), an item is removed from the list.  If
it is `\\[universal-argument] \\[universal-argument]', the entire list is cleared.  If the list is a list
of files, the function can also be called with the prefix argument
`\\[universal-argument]' to store the full path.  If PREFIX is numeric (i.e., `\\[universal-argument] 1'
or `M--'), file name completion is not used.  This is useful for list
options that can take both a file name and a URL as argument."
  (cond
   ((and (listp prefix)
         (eq (car prefix) 16)) ; C-u C-u, use `eq' because `prefix' may be nil.
    (pandoc--set option nil)
    (message (concat description " removed.")))
   ((listp prefix)                      ; C-u or no prefix arg
    (let ((value (cond
                  ((eq type 'string)
                   (read-string "Add value: " nil nil (pandoc--get option)))
                  ((eq type 'number)
                   (read-number "Add number: " 1))
                  ((numberp prefix)
                   (read-string "Add URL: " nil nil (pandoc--get option)))
                  ((eq type 'file)
                   (pandoc--read-file-name "Add file: " default-directory (not prefix))))))
      (pandoc--set option value)
      (message (concat prompt " \"%s\" added.") value)))
   ((eq prefix '-)
    (let ((value (completing-read "Remove item: " (pandoc--get option) nil t)))
      (pandoc--remove-from-list-option option value)
      (message (concat prompt " \"%s\" removed.") value)))))

(defmacro define-pandoc-alist-option (option menu key description prompt)
  "Define OPTION as an alist option.
The option is added to `pandoc--options' and `pandoc--alist-options',
and a menu entry is created under MENU, a symbol naming the menu to
which the option must be added.  It can also be nil, in which case the
option is not added to any menu.  KEY is a string of one character, the
key by which the option will be available in the transient.

OPTION must be a symbol and must be identical to the long form of
the pandoc option (without dashes).  DESCRIPTION is the
description for the option's submenu.  PROMPT is a string that is
used to prompt for setting and unsetting the option.  It must be
formulated in such a way that the strings \"Set/Change \" and
\"Unset \" can be added before it."
  `(progn
     (push (list (quote ,option)) pandoc--options)
     (push (quote ,option) pandoc--alist-options)
     ;; Menu
     ,(when menu
        `(push (list ,description
                     ,(vector (concat "Set/Change " prompt) (lambda () (interactive) (pandoc-set-alist-option nil option description prompt))
                              :active t)
                     ,(vector (concat "Unset " prompt) (lambda () (interactive) (pandoc-set-alist-option '- option description prompt))
                              :active t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-menu-list"))))
     ;; Transient
     ,(when menu
        `(push (quote ,(list key (lambda (pfx)
                                   (interactive "P")
                                   (pandoc-set-alist-option pfx option description prompt))
                             :description `(lambda ()
                                             (format "%-35s[%s]" ,prompt (pandoc--pp-option (quote ,option))))
                             :transient t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-transient-list"))))))

(defun pandoc--alist-option-completion (option)
  "Return a collection function for pandoc-mode alist OPTION."
  (let ((variables (pandoc--get option)))
    (lambda (str pred flag)
      (if (eq flag 'metadata)
          ;; We add a category, so that Marginalia won't add its own annotation.
          `(metadata . ((category . pandoc-option)
                        (annotation-function . ,(lambda (key)
                                                  (propertize (concat (make-string (- 30 (length key)) ?\s)
                                                                      (cdr (assoc key variables)))
                                                              'face 'warning)))))
        (complete-with-action flag variables str pred)))))

(defun pandoc-set-alist-option (prefix option description prompt)
  "Set alist option OPTION interactively.
Ask the user for a single item of OPTION and possibly a value, and
add/update the value of the alist option.

This function is meant to be called from an interactive function to do
the actual work.  PREFIX is the raw prefix argument from the calling
function.  If it is nil, a new <key:value> item is added to the list,
where the user is asked for both <key> and <value>.  If PREFIX is a
number (i.e., the function was called with a numeric prefix argument), a
new <key:value> item is added to the list, where the user is asked for
<key>, but <value> is set to t.

If PREFIX is the negative prefix argument `\\[universal-argument] -' (or `\\[negative-argument]'), an item is
removed from the list.  If it is `\\[universal-argument] \\[universal-argument]', the entire list is cleared."
  (if (and (listp prefix)
           (eq (car prefix) 16)) ; C-u C-u
      (progn
        (pandoc--set option nil)
        (message (concat description " removed")))
    ;; Negative prefix argument M-- or no prefix argument.
    (let ((var (completing-read (format "%s %s: " (if (eq prefix '-) "Remove" "Add/update") prompt)
                                (pandoc--alist-option-completion option))))
      (when (and var (not (string= "" var)))
        (let ((value (cond
                      ((eq prefix '-) nil)
                      ((numberp prefix) t)
                      (t (read-string "Value: " nil nil (cdr (assq var (pandoc--get option))))))))
          (when (string= value "") ;; Strings may be empty, corresponding to boolean True in Pandoc.
            (setq value t))
          (pandoc--set option (cons var value))
          (message (concat prompt " `%s' \"%s\".") var (if value
                                                           (format "added with value `%s'" value)
                                                         "removed")))))))

(defmacro define-pandoc-choice-option (option menu key prompt choices &optional output-formats)
  "Define OPTION as a choice option.
The option is added to `pandoc--options' and `pandoc--cli-options'.
Furthermore, a menu entry is created under MENU, which is a symbol
naming the menu to which the option must be added.  It can also be nil,
in which case the option is not added to any menu.  KEY is a string of
one character, the key by which the option will be available in the
transient.

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
     ,(when menu
        `(push (list ,prompt
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
               ,(intern (concat "pandoc--" (symbol-name menu) "-menu-list"))))
     ,(when menu
        `(push (quote ,(list key (lambda (prefix)
                                   (interactive "P")
                                   (pandoc-set-choice-option prefix option prompt choices))
                             :description `(lambda ()
                                             (format "%-35s[%s]" ,prompt (pandoc--pp-option (quote ,option))))
                             :transient t))
               ,(intern (concat "pandoc--" (symbol-name menu) "-transient-list"))))))

(defun pandoc-set-choice-option (prefix option prompt choices)
  "Set number option OPTION interactively.
This function is meant to be called from an interactive function to do
the actual work.  PROMPT is used to prompt the user.  PREFIX is the raw
prefix argument from the calling function.  If PREFIX is
`\\[universal-argument] -' (or `\\[negative-argument]'), unset the
option.  If PREFIX is nil, the user is prompted for a value.  CHOICES is
a list of possible values for OPTION, the first of which is the default
value."
  (pandoc--set option
               (if (eq prefix '-)
                   nil
                 (let ((value (completing-read (format "Set %s: " prompt) choices nil t)))
                   (if (or (not value)
                           (member value '("" (car choices)))) ; `(car choices)' is the default value.
                       nil
                     value)))))

;;; Defining the options
;; Note that the options are added to the menus and transients in reverse order.

;; Options added to menus manually.
;;
;; Note that `write' is nod defined here, because it has a default value
;; other than nil.
(define-pandoc-string-option read          nil nil "Input Format")
(define-pandoc-string-option output        nil nil "Output File")
(define-pandoc-file-option   output-dir    nil nil "Output Directory")
(define-pandoc-file-option   defaults      nil nil "Defaults File")
(define-pandoc-switch        file-scope    nil nil "Use File Scope")
(define-pandoc-switch        sandbox       nil nil "Run In Sandbox")
(define-pandoc-file-option   data-dir      nil nil "Data Directory")
(define-pandoc-file-option   extract-media nil nil "Extract Media Files")
(define-pandoc-file-option   master-file   nil nil "Master File")
(define-pandoc-string-option verbosity     nil nil "Verbosity")

;;; Reader options
(define-pandoc-file-option   abbreviations           reader "a"      "Abbreviations File")
(define-pandoc-switch        strip-empty-paragraphs  reader "e"      "Strip Empty Paragraphs")
(define-pandoc-choice-option track-changes           reader "T"      "Track Changes" ("accept" "reject" "all") ("docx"))
(define-pandoc-number-option tab-stop                reader "t"      "Tab Stop Width")
(define-pandoc-switch        preserve-tabs           reader "p"      "Preserve Tabs")
(define-pandoc-list-option   metadata-files          reader "M" file "Metadata Files" "Metadata File")
(define-pandoc-alist-option  metadata                reader "m"      "Metadata" "Metadata item")
(define-pandoc-list-option   filter                  reader "f" file "Filters" "Filter")
(define-pandoc-list-option   lua-filter              reader "l" file "Lua Filters" "Lua Filter")
(define-pandoc-string-option default-image-extension reader "i"      "Default Image Extension")
(define-pandoc-string-option indented-code-classes   reader "c"      "Indented Code Classes")
(define-pandoc-number-option shift-heading-level-by  reader "h"      "Header Level Shift")
;; extract-media

;; TODO for data-dir, output-dir and extract-media, a macro define-pandoc-dir-option might be useful.


;;; General writer options
(define-pandoc-switch        no-check-certificate  writer "N"         "Do Not Check Certificates")
(define-pandoc-list-option   request-headers       writer "R"  string "HTTP Request Header" "Request Header")
(define-pandoc-list-option   resource-path         writer "r"  string "Resource Path" "Resource Path")
(define-pandoc-list-option   include-after-body    writer "ia" file   "Include After Body" "File") ; allows URL since Pandoc 2.6.
(define-pandoc-list-option   include-before-body   writer "ib" file   "Include Before Body" "File") ; Also allows URL since Pandoc 2.6.
(define-pandoc-list-option   include-in-header     writer "ih" file   "Include Header" "File") ; Also allows URL since Pandoc 2.6.
(define-pandoc-list-option   syntax-definitions    writer "y" file    "Syntax Definition File" "File")
(define-pandoc-string-option syntax-highlighting   writer "h"         "Syntax Highlighting Type")
(define-pandoc-switch        strip-comments        writer "C"         "Strip comments")
(define-pandoc-switch        list-of-tables        writer "lt"        "List of Tables")
(define-pandoc-switch        list-of-figures       writer "lf"        "List of Figures")
(define-pandoc-number-option toc-depth             writer "D"         "TOC Depth")
(define-pandoc-switch        table-of-contents     writer "T"         "Table of Contents")
(define-pandoc-number-option columns               writer "c"         "Column Width")
(define-pandoc-choice-option wrap                  writer "w"         "Wrap"                ("auto" "none" "preserve"))
(define-pandoc-choice-option eol                   writer "e"         "Line Endings Style"  ("crlf" "lf" "native"))
(define-pandoc-number-option dpi                   writer "d"         "DPI")
(define-pandoc-alist-option  variable-json         writer "j"         "JSON Variables"      "JSON Variable")
(define-pandoc-alist-option  variable              writer "v"         "Variables"           "Variable")
(define-pandoc-file-option   template              writer "t"         "Template File")
(define-pandoc-switch        standalone            writer "s"         "Standalone")
;; print-default-template : not actually included
;; print-default-data-file : not actually included
;; print-highlight-style : not actually included


;;; Options affecting specific writers

;; general
(define-pandoc-switch        ascii              specific      "a"         "Use Only ASCII")
(define-pandoc-switch        link-images        specific      "I"         "Include Links to Images")
(define-pandoc-choice-option ipynb-output       specific      "p"         "Jupyter Output Cells" ("best" "all" "none") ("ipynb"))
(define-pandoc-list-option   pdf-engine-opts    specific      "o"  string "PDF Options" "PDF Option")
(define-pandoc-choice-option pdf-engine         specific      "e"         "PDF Engine"
  ("pdflatex" "lualatex" "xelatex" "tectonic" "latexmk" "wkhtmltopdf" "weasyprint" "prince" "pagedjs-cli" "context" "pdfroff"))
(define-pandoc-file-option   reference-doc      specific      "R"         "Reference Doc")
(define-pandoc-number-option slide-level        specific      "H"         "Slide Level Header")
(define-pandoc-switch        incremental        specific      "i"         "Incremental")
(define-pandoc-switch        number-sections    specific      "n"         "Number Sections")
(define-pandoc-switch        list-tables        specific      "L"         "Render tables as list tables")
(define-pandoc-choice-option markdown-headings  specific      "h"         "Markdown Headings" ("atx" "setext")
                             ("markdown" "markdown_github" "markdown_mmd" "markdown_phpextra" "markdown_strict"))
(define-pandoc-choice-option table-caption-position specific  "t"         "Position of table captions" ("above" "below"))
(define-pandoc-choice-option figure-caption-position specific "f"         "Position of figure captions" ("above" "below"))
(define-pandoc-switch        reference-links    specific      "r"         "Reference Links")
(define-pandoc-choice-option reference-location specific      "l"         "Reference Location" ("block" "section" "document")
                             ("markdown" "markdown_github" "markdown_mmd" "markdown_phpextra" "markdown_strict"))
(define-pandoc-choice-option top-level-division specific      "T"         "Top Level Division" ("section" "part" "chapter")
                             ("latex" "context" "docbook" "docbook5" "tei"))

;; html-based
(define-pandoc-list-option   css               html "c"  file  "CSS Style Sheet" "CSS File")
(define-pandoc-string-option title-prefix      html "t"        "Title Prefix")
(define-pandoc-string-option identifier-prefix html "i"        "ID Prefix")
(define-pandoc-choice-option email-obfuscation html "e"        "Email Obfuscation" ("none" "javascript" "references") ("html" "html5" "s5" "slidy" "slideous" "dzslides" "revealjs"))
(define-pandoc-switch        section-divs      html "d"        "Wrap Sections in <div> Tags")
(define-pandoc-list-option   number-offset     html "o" number "Number Offsets" "Offset")
(define-pandoc-switch        html-q-tags       html "Q"        "Use <q> Tags for Quotes in HTML")
(define-pandoc-switch        embed-resources   html "E"        "Embed All Resources")

;; epub
(define-pandoc-file-option   epub-subdirectory  epub "d"       "EPUB Subdirectory")
(define-pandoc-list-option   epub-fonts         epub "f"  file "EPUB Fonts" "Embed Font")
(define-pandoc-file-option   epub-metadata      epub "m"       "EPUB Metadata File")
(define-pandoc-switch        epub-title-page    epub "t"       "Add EPUB Title Page")
(define-pandoc-file-option   epub-cover-image   epub "i"       "EPUB Cover Image")
(define-pandoc-string-option chunk-template     epub "C"       "Template for Chunk Filenames")
(define-pandoc-number-option split-level        epub "l"       "Split at Heading Level")

;; obsolete
(define-pandoc-string-option highlight-style    obsolete "s" "Highlighting Style")
(define-pandoc-switch        no-highlight       obsolete "n" "No Highlighting")
(define-pandoc-switch        self-contained     obsolete "s" "Self-contained Document")
(define-pandoc-number-option epub-chapter-level obsolete "e" "EPUB Chapter Level")
(define-pandoc-switch        listings           obsolete "l" "Use LaTeX listings Package")
(define-pandoc-number-option base-header-level  obsolete "h" "Base Header Level")

;;; Citation rendering
(define-pandoc-choice-option cite-method            citations "m"       "Citation Method" '("natbib" "biblatex"))
(define-pandoc-file-option   citation-abbreviations citations "a"       "Citation Abbreviations File")
(define-pandoc-file-option   csl                    citations "C"       "CSL File")
(define-pandoc-list-option   bibliography           citations "B"  file "Bibliography Files" "Bibliography File")
(define-pandoc-switch        citeproc               citations "c"       "Process Citations")

;;; Math rendering in HTML

;; Note: `html-math-method' may look like an alist option, since its value
;; is an alist with keys `method' and `url', but the two keys cannot be set
;; independently.  Therefore we define it as a list option, even though
;; that's not a really good fit, either. That's why we also have a special
;; setter function, `pandoc-set-html-math-method'.
;;
;; TODO An object-oriented approach for the options would probably be
;; better. Perhaps subclassing transient classes, although I'm not sure if
;; that would still work with the menu-bar menus.
(define-pandoc-list-option html-math-method nil nil string "HTML Math Rendering" "")

(provide 'pandoc-mode-utils)

;;; pandoc-mode-utils.el ends here
