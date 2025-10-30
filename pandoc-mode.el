;;; pandoc-mode.el --- Minor mode for interacting with Pandoc  -*- lexical-binding: t -*-

;; Copyright (c) 2009-2025 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 31 Oct 2009
;; Version: 2.34
;; Keywords: text, pandoc
;; URL: http://joostkremers.github.io/pandoc-mode/
;; Package-Requires: ((emacs "30.1") (yaml "1.2.0"))

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
(require 'cl-lib)
(require 'thingatpt)
(require 'yaml)

(declare-function ebib "ext:ebib.el" (&optional file key))

;;; Defcustoms

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

;;; Global variables

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

;; (defun pandoc-add-extensions (format)
;;   "Add extensions for FORMAT."
;;   (with-temp-buffer
;;     (insert-file-contents "extensions.txt")
;;     (goto-char (point-min))
;;     (while (< (point) (point-max))
;;       (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
;;              (flag (cdr (assoc (substring line 0 1) '(("+" . 1) ("-" . 2)))))
;;              (extension (substring line 1))
;;              (entry (assoc extension pandoc--extensions-alist)))
;;         (when (null entry)
;;           (setq entry (car (push (list extension nil nil) pandoc--extensions-alist))))
;;         (push format (nth flag entry)))
;;       (forward-line 1)))
;;   (with-current-buffer "extensions.eld"
;;     (erase-buffer)
;;     (insert (format "%S" pandoc--extensions-alist))
;;     (save-buffer)))

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
  `((writer . "native")))
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
If the cdr of an entry is t, the option takes an optional URL.")

(defvar-local pandoc--local-settings nil "A buffer-local variable holding a file's pandoc options.")

(defvar-local pandoc--settings-modified-flag nil "Non-nil means the current settings were modified and not saved.")

(defvar-local pandoc--latest-run nil
  "The output format and the output file created in the most recent call to Pandoc.")

(defvar pandoc--output-buffer-name " *Pandoc output*")
(defvar pandoc--log-buffer-name " *Pandoc log*")
(defvar pandoc--viewer-buffer-name " *Pandoc viewer*")

(defvar pandoc--options-menu nil
  "Auxiliary variable for creating the options menu.")

;;; Utility functions

;; File names

(defun pandoc--read-file-name (prompt dir relative)
  "Read a file name using PROMPT.
DIR is the directory used for completing file names.  If RELATIVE
is non-nil, return the file path as a relative path starting from
DIR, otherwise return the full path."
  ;; We inhibit inserting the default directory, though not all completion
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

;; Logging

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

;; Pretty-printing

(defun pandoc--pp-switch (switch)
  "Return a pretty-printed representation of SWITCH."
  (if (pandoc--get switch)
      "yes"
    "no"))

(defun pandoc--pp-option (option)
  "Return a pretty-printed representation of OPTION."
  (or (pandoc--get option)
      ""))

;; Getter and setter functions

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
  (when (assq option pandoc--options) ; Check if the option is licit.
    (unless (assq option pandoc--local-settings) ; Add the option if it's not there.
      (push (list option) pandoc--local-settings))
    (cond
     ((memq option pandoc--alist-options)
      (pandoc--set-alist-option option value))
     ((memq option pandoc--list-options)
      (pandoc--set-list-option option value))
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

;; Extensions

;; Note: the extensions appear to be binary options, but they are not:
;; they're really (balanced) ternary options. They can be on or off, but
;; that doesn't tell us whether they're on or off because the user set them
;; that way or because that's the default setting for the relevant format.
;;
;; So essentially each extension can have one of three values: nil, meaning
;; default, ?-, meaning switched off by the user, or ?+, meaning switched
;; on by the user.

(defun pandoc--extension-active-marker (extension rw)
  "Return a marker indicating whether EXTENSION is active.
RW is either `reader' or `writer', indicating whether to take the
input or the output format."
  (if (pandoc--extension-active-p extension rw)
      pandoc-extension-active-marker
    pandoc-extension-inactive-marker))

(defun pandoc--extension-in-format (extension format)
  "Check if EXTENSION is supported for FORMAT.
Return value is either one of the keywords `:enabled' or `:disabled',
indicating that EXTENSION is supported for FORMAT and whether it is
enabled or disabled by default, or nil if EXTENSION is not supported for
FORMAT."
  (let ((formats (cdr (assoc extension pandoc--extensions-alist))))
    (cond
     ((member format (memq '| formats))
      :disabled)
     ;; We could just check `format' against the entire list here, because
     ;; it wasn't found after the pipe, but this is cleaner and avoids some
     ;; unnecessary comparisons if the format is not supported.
     ((member format (seq-take-while (lambda (e) (not (eq e '|))) formats))
      :enabled))))

(defun pandoc--extension-active-p (extension rw)
  "Return non-nil if EXTENSION is active in the current buffer.
RW is either `reader' or `writer', indicating whether to test for the
input or the output format.

An extension is active either if it is automatically enabled for a
format and hasn't been deactivated by the user, or if the user has
activated it."
  (let ((value (pandoc--get-extension extension rw)))
    (or (char-equal value ?+)
        (and (not value)
             (eq (pandoc--extension-in-format extension (pandoc--get rw)) :enabled)))))

(defun pandoc--split-format-and-extensions (full-format)
  "Split FULL-FORMAT into format and extensions.
Return a cons cell of the format and the list of extensions."
  (let* ((start 0)
         (pos 0)
         result)
    ;; We start searching at `(1+ start)', because `start' is set to `pos'
    ;; at the end of the loop, but `pos' is the position of the + or - we
    ;; found.  We need this position because the + or - needs to be
    ;; included in the substring. (Which is also why we can't use
    ;; `split-string'.)
    (while (setq pos (string-match-p "[-+]" full-format (1+ start)))
      (push (substring full-format start pos) result)
      (setq start pos))
    (nreverse (push (substring full-format start) result))))

(defun pandoc--set-extension (extension rw value)
  "Set the value of EXTENSION for RW to VALUE.
RW is either `reader' or `writer', indicating whether an input or output
format extension is to be set.  VALUE is a character, either ?+ or ?-.
VALUE can also be nil, in which case the extension is removed from the
input or output format.  (Note that this means that the extension falls
back to its default value, not that it is unset.)"
  (let* ((format+exts (pandoc--split-format-and-extensions (pandoc--get rw)))
         (format (car format+exts))
         (exts (seq-filter (lambda (ext)
                             (not (= (substring ext 1) extension)))
                           (cdr format+exts))))
    (when value
      (push (format "%c%s" value extension) exts))
    (pandoc--set rw (concat format (string-join exts)))))

(defun pandoc--get-extension (extension rw)
  "Return the value of EXTENSION for RW.
RW is either `reader' or `writer', indicating whether the read or write
extension is to be queried.  Return ?+ if the extension is explicitly
enabled, ?- it is explicitly disabled, or nil if it has its default
value."
  (and-let* ((format (pandoc--get rw))
             (pos (string-match-p (concat "\\b[-+]" extension "\\b") format))
             (state (aref format pos)))))

;;; Helper functions and macros for defining options

;; Transient variables
(defvar pandoc--reader-transient-list nil)
(defvar pandoc--writer-transient-list nil)
(defvar pandoc--specific-transient-list nil)
(defvar pandoc--html-transient-list nil)
(defvar pandoc--epub-transient-list nil)
(defvar pandoc--obsolete-transient-list nil)
(defvar pandoc--citations-transient-list nil)

;; Menu variables
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
                                  `(quote (member (pandoc--get 'writer) (quote ,output-formats)))
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
(define-pandoc-string-option reader        nil nil "Input Format")
(define-pandoc-string-option output-file   nil nil "Output File")
(define-pandoc-file-option   output-dir    nil nil "Output Directory")
(define-pandoc-file-option   defaults      nil nil "Defaults File")
(define-pandoc-switch        file-scope    nil nil "Use File Scope")
(define-pandoc-switch        sandbox       nil nil "Run In Sandbox")
(define-pandoc-file-option   data-dir      nil nil "Data Directory")
(define-pandoc-file-option   extract-media nil nil "Extract Media Files")
(define-pandoc-file-option   master-file   nil nil "Master File")
(define-pandoc-string-option verbosity     nil nil "Verbosity")

;; Note that `writer' is not defined here, because it has a default value
;; other than nil.

;; Reader options
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

;; General writer options
(define-pandoc-switch        no-check-certificate  writer "N"         "Do Not Check Certificates")
(define-pandoc-list-option   request-headers       writer "R"  string "HTTP Request Header" "Request Header")
(define-pandoc-list-option   resource-path         writer "r"  string "Resource Path" "Resource Path")
(define-pandoc-list-option   include-after-body    writer "ia" file   "Include After Body" "File")
(define-pandoc-list-option   include-before-body   writer "ib" file   "Include Before Body" "File")
(define-pandoc-list-option   include-in-header     writer "ih" file   "Include Header" "File")
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

;; General
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

;; HTML-based
(define-pandoc-list-option   css               html "c"  file  "CSS Style Sheet" "CSS File")
(define-pandoc-string-option title-prefix      html "t"        "Title Prefix")
(define-pandoc-string-option identifier-prefix html "i"        "ID Prefix")
(define-pandoc-choice-option email-obfuscation html "e"        "Email Obfuscation" ("none" "javascript" "references") ("html" "html5" "s5" "slidy" "slideous" "dzslides" "revealjs"))
(define-pandoc-switch        section-divs      html "d"        "Wrap Sections in <div> Tags")
(define-pandoc-list-option   number-offset     html "o" number "Number Offsets" "Offset")
(define-pandoc-switch        html-q-tags       html "Q"        "Use <q> Tags for Quotes in HTML")
(define-pandoc-switch        embed-resources   html "E"        "Embed All Resources")

;; EPUB
(define-pandoc-file-option   epub-subdirectory  epub "d"       "EPUB Subdirectory")
(define-pandoc-list-option   epub-fonts         epub "f"  file "EPUB Fonts" "Embed Font")
(define-pandoc-file-option   epub-metadata      epub "m"       "EPUB Metadata File")
(define-pandoc-switch        epub-title-page    epub "t"       "Add EPUB Title Page")
(define-pandoc-file-option   epub-cover-image   epub "i"       "EPUB Cover Image")
(define-pandoc-string-option chunk-template     epub "C"       "Template for Chunk Filenames")
(define-pandoc-number-option split-level        epub "l"       "Split at Heading Level")

;; Obsolete
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

;;; Main

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
             (file-exists-p (pandoc--create-defaults-filename 'local "default" (buffer-file-name))))
    (pandoc-mode 1)))

;;; Running Pandoc

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
   ((or (eq (pandoc--get 'output-file) t) ; If the user set the output file to t.
        (and (null (pandoc--get 'output-file)) ; or if the user set no output file but either
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
   ((stringp (pandoc--get 'output-file))     ; If the user set an output file,
    (format "%s/%s"               ; we combine it with the output directory
            (expand-file-name (or (pandoc--get 'output-dir)
                                  (file-name-directory input-file)))
            (if pdf                 ; and check if we're converting to pdf.
                (concat (file-name-sans-extension (pandoc--get 'output-file)) ".pdf")
              (pandoc--get 'output-file))))
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

;;; Saving settings

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

(defun pandoc--create-defaults-filename (type output-format &optional filename)
  "Create a defaults filename.
TYPE is the type of defaults file, either `local', `project', or
`global'.  FILENAME is name of the file for which the defaults file is
to be created (which is ignored if TYPE is `global'), OUTPUT-FORMAT the
output format of the defaults file, which is recorded in its name.  The
return value is an absolute filename."
  (when filename (setq filename (expand-file-name filename)))
  (cond
   ((eq type 'local)
    (concat (file-name-directory filename) (file-name-nondirectory filename) "_" output-format ".yaml"))
   ((eq type 'project)
    (concat (file-name-directory filename) "Project_" output-format ".yaml"))
   ((eq type 'global)
    (concat (file-name-as-directory pandoc-data-dir) output-format "defaults.yaml"))))

(defun pandoc-set-default-format ()
  "Set the current output format as default.
This is done by creating a symbolic link to the relevant defaults
files.  (Therefore, this function is not available on Windows.)"
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "This option is not available on MS Windows")
    (let ((current-defaults-file
           (file-name-nondirectory
            (pandoc--create-defaults-filename 'local (pandoc--get 'writer) (buffer-file-name))))
          (current-project-file
           (file-name-nondirectory
            (pandoc--create-defaults-filename 'project (pandoc--get 'writer) (buffer-file-name)))))
      (when (not (file-exists-p current-defaults-file))
        (pandoc--save-settings 'local (pandoc--get 'writer)))
      (make-symbolic-link current-defaults-file
                          (pandoc--create-defaults-filename 'local "default" (buffer-file-name)) t)
      (when (file-exists-p current-project-file)
        (make-symbolic-link current-project-file
                            (pandoc--create-defaults-filename 'project "default" (buffer-file-name)) t))
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
         (defaults-file (pandoc--create-defaults-filename
                         type
                         format
                         (unless (eq type 'global) filename))))
    (if (and (not no-confirm)
             (file-exists-p defaults-file)
             (not (y-or-n-p (format "%s defaults file `%s' already exists.  Overwrite? "
                                    (capitalize (symbol-name type))
                                    (file-name-nondirectory defaults-file)))))
        (message "%s defaults file not written." (capitalize (symbol-name type)))
      (with-temp-buffer
        (insert (format "# Emacs pandoc-mode %s settings file%s\n"
                        type
                        (if (eq type 'local)
                            (concat " for " (file-name-nondirectory filename))
                          ""))
                (format "# Saved on %s\n\n" (format-time-string "%Y.%m.%d %H:%M"))
                (yaml-encode pandoc--local-settings)
                "\n\n## pandoc-mode settings ##\n"
                (string-join (mapcar (lambda (str)
                                       (concat "# " str))
                                     (split-string (yaml-encode `((output-dir . ,pandoc-output-dir)
                                                                  (master-file . ,pandoc-master-file)))
                                                   "\n"))
                             "\n")
                "\n")
        (let ((make-backup-files nil))
          (write-region (point-min) (point-max) defaults-file))
        (message "%s settings file written to `%s'."
                 (capitalize (symbol-name type))
                 (file-name-nondirectory defaults-file)))
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
    ;; First try to read local settings:
    (when file
      (setq settings (cons 'local (pandoc--read-settings-from-file (pandoc--create-defaults-filename 'local format file)))))
    ;; If that fails, try project settings:
    (when (and file (not (cdr settings)))
      (setq settings (cons 'project (pandoc--read-settings-from-file (pandoc--create-defaults-filename 'project format file)))))
    ;; If that fails too, or if there is no file, try reading global settings:
    (unless (cdr settings)
      (setq settings (cons 'global (pandoc--read-settings-from-file (pandoc--create-defaults-filename 'global format)))))
    ;; Now set them:
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
        (list :yaml (yaml-parse-string (buffer-string) :object-type 'alist :null-object nil)
              :non-pandoc (pandoc--read-non-pandoc-settings)))))

(defun pandoc--read-non-pandoc-settings ()
  "Read non-Pandoc settings in the current buffer.
The buffer should be a (temporary) buffer holding a defaults file.  The
non-Pandoc settings are additional settings placed in a comment block at
the end of the defaults file."
  (goto-char (point-min))
  (let* ((start (re-search-forward "## pandoc-mode settings ##\n"))
         (end (re-search-forward "## end"))
         (lines (split-string (buffer-substring start end) "\n")))
    (yaml-parse-string (string-join (mapcar (lambda (l)
                                              (substring l 2))
                                            (take (1- (length lines)) lines))
                                    "\n")
                       :object-type 'alist
                       :null-object nil)))

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

;;; Setter functions

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

(defun pandoc-set-output (prefix)
  "Set the output file.
If called with the PREFIX argument `\\[universal-argument] -' (or `\\[negative-argument]', the output
file is unset.  If called with any other prefix argument, the output
file is created on the basis of the input file and the output format."
  (interactive "P")
  (pandoc--set 'output-file
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

;;; Menu-bar menu

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
                                   pandoc--extensions-alist))))

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
                                   pandoc--extensions-alist))))

    ("Files"
     ("Output File"
      ["Output To Stdout" (pandoc--set 'output-file nil) :active t
       :style radio :selected (null (pandoc--get 'output-file))]
      ["Create Output Filename" (pandoc--set 'output-file t) :active t
       :style radio :selected (eq (pandoc--get 'output-file) t)]
      ["Set Output File..." pandoc-set-output :active t
       :style radio :selected (stringp (pandoc--get 'output-file))])
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

;;; Transients

(transient-define-prefix pandoc-main-transient ()
  "Pandoc-mode main menu."
  ["Pandoc\n"
   ("I" pandoc-input-formats-transient
    :description (lambda ()
                   (format "Input format   [%s]" (or (pandoc--get 'reader) "none"))))
   ("O" pandoc-output-formats-transient
    :description (lambda ()
                   (format "Output format  [%s]" (or (pandoc--get 'writer) "none"))))]
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

;; Generate the main input & output format transients.
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

;; Generate the specific input & output format transients.
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
                                                             (not (eq (nth 3 elt) 'output-file)))
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
             (let ((num 0)
                   (format (pandoc--get 'reader)))
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
                        (seq-partition (seq-filter (lambda (ext)
                                                     (member format (cdr ext)))
                                                   pandoc--extensions-alist)
                                       26))
                       (list [("b" "Back" transient-quit-one)
                              ("q" "Quit" transient-quit-all)])))))])

(transient-define-prefix pandoc-write-exts-transient ()
  "Pandoc-mode writer extensions menu."
  [:class transient-columns
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'pandoc-write-exts-transient
             (let ((num 0)
                   (format (pandoc--get 'writer)))
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
                        (seq-partition (seq-filter (lambda (ext)
                                                     (member format (cdr ext)))
                                                   pandoc--extensions-alist)
                                       26))
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
                   (format "%-27s[%s]" "Output file" (pandoc--pp-option 'output-file))))
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

;;; Citation jumping

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

;;; Numbered example lists

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

(define-derived-mode pandoc-@-mode
  fundamental-mode "Pandoc-select"
  "Major mode for the Pandoc-select buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

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

(provide 'pandoc-mode)

;;; pandoc-mode.el ends here
