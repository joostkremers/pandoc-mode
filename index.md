Introduction
============

`pandoc-mode` is an Emacs mode for interacting with [Pandoc](http://pandoc.org/). Pandoc is a program (plus libraries) created by John MacFarlane that can convert a text written in one markup language into another markup language. `pandoc-mode` is implemented as a minor mode that can be activated alongside the major mode for any of Pandoc's supported input formats. It provides facilities to set the various options that Pandoc accepts and to run Pandoc on the input file. It is possible to create different output profiles for a single input file, so that you can, for example, write your text in Markdown and then translate it to HTML for online reading, PDF for offline reading and Texinfo for reading in Emacs, all with just a few keystrokes.

The current version of `pandoc-mode` is 2.20 and is compatible with Pandoc version 1.18.

Installation
============

The easiest way to install `pandoc-mode` is to use the [Melpa](http://melpa.org/) package repository. Alternatively, you can install it manually: download `pandoc-mode.el` and `pandoc-mode-utils.el`, put them into Emacs' loadpath (byte-compiling is recommended) and add the line `(load "pandoc-mode")` to your init file.

Either installation method allows Emacs to load `pandoc-mode`, but they do not activate it. In order to activate `pandoc-mode` in a buffer, you need to type `M-x pandoc-mode`. To start `pandoc-mode` automatically when you load e.g., a Markdown file, you can add a hook to your init file:

    (add-hook 'markdown-mode-hook 'pandoc-mode)

However, if you do not want to start `pandoc-mode` every time you work on a Markdown document, you can use a different function in `markdown-mode-hook`: instead of using `pandoc-mode`, you can use `conditionally-turn-on-pandoc`. This function checks if a default settings file exists for the file you're opening and only turns on `pandoc-mode` if it finds one. (For more info on the settings file, see the section ['Settings Files'](#settings-files).)

Additionally, if you want to automatically load the default `pandoc-mode` settings file for the file you're opening, you can add the following to your init file:

    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

The function `pandoc-load-default-settings` checks if a default settings file exists for the file being loaded and reads its settings if it finds one.

OS X
----

`pandoc-mode` expects that the `pandoc` binary can be found in your system’s `$PATH` and that the contents of `$PATH` is available to Emacs. Especially on OS X, this may not always be the case. To remedy this, you can set the user option `pandoc-binary` to the full path of `pandoc` on your system. A more elegant solution is to install the [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) package. This package makes sure that the contents of your system’s `$PATH` variable is visible to Emacs, allowing Emacs to find `pandoc`.

Usage
=====

`pandoc-mode` uses the [hydra](https://github.com/abo-abo/hydra) package to create a keyboard-driven menu interface to all options and settings. The main menu is called by pressing `C-c /`. After that, everything should be self-explanatory. From the main menu, you can run `pandoc` on the buffer, view the output buffer and the current settings, set the input and output formats, and you can go to the options menu.

Note that if `menu-bar-mode` is active, `pandoc-mode` also provides a menu in the menu bar. It has the same structure as the hydra menu and it has the advantage that options that do not apply to the current file (due to its input or output format), are generally greyed out. On the other hand, the hydra menu shows the value of the options and allows you to change more than one option without having to keep reopening the menu. The menu bar menu disappears when you select an option, the hydra menu (generally) does not. Instead, it can be dismissed with `q`. Below, I talk about the hydra menu specifically, but most of what is said applies to the menu bar menu as well.

In the options menu, you can set options for running `pandoc` on your input file. All Pandoc options can be set from the menu (except for one or two that do not make sense, e.g., `--print-default-template`). Note that when you set options, they only apply to the current file *and* the current output format. When you open another file, or when you change the output format, all settings are reset to their default values. (There are ways to make settings more permanent, of course, as discussed below.)

Input and output formats
------------------------

The most important settings are the input and output formats. The input format is set automatically by Emacs on the basis of the major mode of the input file, but you can change it if you need to. The output format defaults to "Native Haskell", so most likely you will want to set it to something else before you run Pandoc. The input and output format menus also provide access to a submenu with the Markdown extensions that Pandoc supports.

As already stated, you may wish to use different output formats for a single input file. Most likely, the options that you want to pass to Pandoc will be different for each output format. To make this easier, `pandoc-mode` has the ability to save the settings for a specific output format. The main menu has an option “Settings files” (`C-c / s`), which takes you to a submenu where you can save the current settings. Emacs saves these settings to a hidden file in the same directory as the file you're editing, under a name composed of the input file, appended with the name of the output format and the string `.pandoc`. So if your input file is called `mytext.md`, the `html` settings file will be called `.mytext.md.html.pandoc`. (See the section ['Settings Files'](#settings-files) for details.)

A single document can have a separate settings file for each output format that Pandoc supports. These can simply be created by setting all options the way you want them for the first output format, save them, then choose another output format, set the required options, save again, etc. Because the name of a settings file contains the output format for which it was created, the different settings files won't interfere with each other. When you switch the output format (with `C-c / O`), Emacs checks if a corresponding settings file exists and loads it if one is found.

On systems that have symbolic links, it is also possible to specify a default output format (`C-c / s d`). This is done by creating a symbolic link to the settings file of the current output format (a settings file is created if one doesn't exist yet) with the output format replaced by the string `"default"`. The file it points to is read by the function `pandoc-load-default-settings`, making it possible to automatically load a specific settings file when `pandoc-mode` is invoked, as described above.

Note that the current output format is always visible in the mode line: the "lighter" for `pandoc-mode` in the mode line has the form `Pandoc/<format>`, where `<format>` is the current output format.

The major modes for which `pandoc-mode` selects an input format automatically can be customised (user option `pandoc-major-modes`). You can add major modes or remove those that you don't use. Similarly, you can customise the file extensions for each output format (`pandoc-output-format-extensions`).

The options menu
----------------

The options menu has a number of submenus, each related to a specific type of options: file options, reader options, writer options (general and specific), citations and math rendering. The file options menu contains options for the output file, output directory, data directory, the directory to extract media files to, and the master file. Only two of these (the data directory and the extract media directory) correspond directly to a Pandoc option. The output file and output directory options are combined to form Pandoc’s `--output` option, while the master file option is only used by `pandoc-mode`. These options are discussed in the sections ['Setting an output file'](#setting-an-output-file) and [’Master file’](#master-file), respectively.

Note that the subdivision in the options menu is based on the subdivision in the Pandoc README and the user guide on [Pandoc.org](http://pandoc.org/README.html), which should make it easier to find the relevant options in the menus.

One nice thing about the hydra menus is that the value of an option is displayed beside it. Pandoc’s options come in several different kinds. Switches, (boolean options that do not take a value), are toggled when you select them, and their value is displayed as either “yes” or “no”. If you select another kind of option, you are asked to provide a value in the minibuffer. For template variables and metadata items, you are asked both a variable / metadata name and a value.

Unsetting an option can usually be done by prefixing the relevant key with a dash `-`. This is actually the negative prefix argument, which can be typed without the meta (alt) key when inside a hydra menu. So for example, if you’re in the files menu (`C-c / o f`), you can set an output file with `o`, and to unset the output file, you can type `- o`.

Many Pandoc options have file names as values. These are normally prompted for and stored as *relative* paths. File name completion is available, starting from the current directory. For some options, such as `--css`, relative paths make more sense because an absolute file would almost certainly be incorrect once the output html file is moved to the web server. Other options, such as `--template`, look in Pandoc's data directory and therefore also do not require an absolute path. Lastly, auxiliary files, such as `--include-in-header`, will usually be stored in the same directory as the source file or in a subdirectory, in which case a relative path is unproblematic.

However, if for some reason you need to store an absolute path for an option, you can do so by using the prefix argument `C-u`. So for example in the general writer options menu, accessible through `C-/ o w`, pressing `C-u H` asks for a file to include in the header and stores it as an absolute path. Note that absolute paths are not expanded, i.e., they may contain abbreviations such as `~` for one’s home directory. This makes it easier to share settings files between computers with different OSes (for example, Linux expands `~` to `/home/<user>`, while on OS X it becomes `/Users/<user>`).

File options may have a default value (although currently, this is only the case for `--epub-stylesheet`). Such options can be specified on the `pandoc` command line without naming a file. To select such a default value for a file option, use a numeric prefix argument, which in the hydra menu is obtained by pressing a number without the meta key. That is, to select the default EPUB style sheet, go to the EPUB options menu (`C-/ o s E`) and press `1 s`.

Options that are not files or numbers are “string options”, which include options that specify a URL. These may also have a default value, which can be set in the same way as with file options. Note, though, that this does not apply to options that only have a limited set of possible values (e.g., `--email-obfuscation`, `--latex-engine`). These can be set or unset, you cannot explicitly request their default value. (\`pandoc' uses their default values even if they are not specified on the command line, unlike string options.)

To get an overview of all the settings for the current file and output format, you can use the option “View current settings” in the main menu (`C-c / S`). This displays all settings in a `*Help*` buffer in a Lisp-like format. For example, the settings for TeXinfo output of this manual look like this:

    ((standalone . t)
     (read . "markdown")
     (write . "texinfo")
     (output . t)
     (include-before-body . "~/src/pandoc-mode/manual/texi-before-body"))

Template variables and metadata
-------------------------------

`pandoc-mode` allows you to set or change template variables through the menu. The variables are in the general writer options menu, the metadata in the reader options menu. Emacs will ask you for the name of a variable or metadata item and for a value for it. If you provide a name that already exists (TAB completion works), the new value replaces the old one.

Deleting a template variable or metadata item can be done by prefixing the menu key with `-`. Emacs will ask you for the variable name (TAB completion works here, too) and removes it from the list.

Running Pandoc
--------------

The first item in the menu is "Run Pandoc" (accessible with `C-c / r`), which, as the name suggests, runs Pandoc on the document, passing all options you have set. By default, Pandoc sends the output to stdout (except when the output format is "odt", "epub" or "docx", in which case output is always sent to a file.). Emacs captures this output and redirects it to the buffer `*Pandoc output*`. The output buffer is not normally shown, but you can make it visible through the menu or by typing `C-c / V`. Error messages from Pandoc are also displayed in this buffer.

When you run Pandoc, `pandoc-mode` also generates a few messages, which are logged in a buffer calles `*Pandoc log*`. You will rarely need to see this, since `pandoc-mode` displays a message telling you whether Pandoc finished successfully or not. In the latter case, the output buffer is displayed, so you can see the error that Pandoc reported.

Note that when you run Pandoc, Pandoc doesn't read the file on disk. Rather, Emacs feeds it the contents of the buffer through `stdin`. This means that you don't actually have to save your file before running Pandoc. Whatever is in your buffer, saved or not, is passed to Pandoc. Alternatively, if the region is active, only the region is sent to Pandoc.

If you call this command with a prefix argument `C-u` (so the key sequence becomes `C-/ C-u r`: `C-/` to open the menu and `C-u r` to run Pandoc), Emacs asks you for an output format to use. If there is a settings file for the format you specify, the settings in it will be passed to Pandoc instead of the settings in the current buffer. If there is no settings file, Pandoc will be called with just the output format and no other options.

Note that specifying an output format this way does not change the output format or any of the settings in the buffer, it just changes the output profile used for calling Pandoc. This can be useful if you use different output formats but don't want to keep switching between profiles when creating the different output files.

Setting an output file
----------------------

If you want to save the output to a file rather than have it appear in the output buffer, you can set an explicit output file. Note that setting an output *file* is not the same thing as setting an output *format* (though normally the output file has a suffix that indicates the format of the file).

In `pandoc-mode`, the output file setting has three options: the default is to send output to stdout, in which case it is redirected to the buffer `*Pandoc output*`. This option can be selected by typing `- o` in the file options menu. Alternatively, you can let Emacs create an output filename for you. In this case the output file will have the same base name as the input file but with the proper suffix for the output format. To select this option, prefix the output file key `o` with `C-u` in the file options menu. The third option is to specify an explicit output file. This can (obviously) be done by hitting just `o`.

Note that Pandoc does not allow output to be sent to stdout if the output format is an OpenOffice.org Document (ODT), EPUB or MS Word (docx) file. Therefore, Emacs will always create an output filename in those cases, unless of course you've explicitly set an output file yourself.

The output file you set is always just the base filename, it does not specify a directory. Which directory the output file is written to depends on the setting "Output Directory" (which is not actually a Pandoc option). Emacs creates an output destination out of the settings for the output directory and output file. If you don't specify any output directory, the output file will be written to the same directory that the input file is in.

Creating a pdf
--------------

The second item in the main menu is "Create PDF" (invoked with `C-c / p`). This option calls Pandoc with an output file with the extention `.pdf`, causing Pandoc to create a pdf file by first converting to `.tex` and then calling LaTeX on it. Like `C-c / r`, this command operates on the region if it is active.

If you choose this option, Emacs checks if your current output format is `latex`. If it is, Emacs calls Pandoc with the buffer's settings. If the output format is something other than `latex`, Emacs checks if you have a settings file for LaTeX output and uses those settings. This allows you to create a pdf without having to switch the output format to LaTeX. If your current output format is not LaTeX and no LaTeX settings file is found, Emacs calls Pandoc with only the input and output formats.

Connection Type
---------------

By default, Emacs starts `pandoc` as an asynchronous process using a tty. If this causes problems for some reason, you can try using a pipe instead by customising `pandoc-process-connection-type`. Alternatively, you can use a synchronous process by unsetting the user option `pandoc-use-async`.

Citation Jumping
----------------

`pandoc-mode` provides the function `pandoc-jump-to-reference` that locates a reference within external bibliography files indicated by the `bibliography` user option. Note that entries to the `bibliography` user option list must have an absolute path for this option to work properly (i.e. `"./Bibliography.bib"` rather than `"Bibliography.bib"`). This feature is not bound to any key by default, but may of course be bound to a key combination as follows:

    (define-key markdown-mode-map (kbd "C-c j") 'pandoc-jump-to-reference)

The jump behaviour can be customised by changing the option `pandoc-citation-jump-function`. Its default value is `pandoc-goto-citation-reference`, which opens the relevant BibTeX file in a new window and moves point to the entry. Two alternative functions have been defined: `pandoc-open-in-ebib`, which opens the relevant entry in Ebib, and `pandoc-show-entry-as-help`, which shows the entry in a `*Help*` buffer, but does not open the corresponding BibTeX file.

Alternatively, you may also define your own function, which should take two arguments: the key of the entry to be displayed and a list of BibTeX files.

Font lock
=========

`pandoc-mode` adds font lock keywords for citations and numbered example lists. The relevant faces can be customised in the customisation group `pandoc`.

Settings Files
==============

Apart from settings files for individual files (which are called *local settings files*), `pandoc-mode` supports two other types of settings files: project files and global files. Project files are settings files that apply to all input files in a given directory (except those files for which a local settings file exists). Global settings files, as the name implies, apply globally, to files for which no local or project file is found. Both types of files are specific to a particular output format, just like local settings files. Project files live in the directory they apply to and are called `Project.<format>.pandoc`. Global files live in the directory specified by the variable `pandoc-data-dir`, which defaults to `~/.emacs.d/pandoc-mode/`, but this can of course be changed in the customisation group `pandoc`.

Whenever `pandoc-mode` loads settings for an input file, it first checks if there is a local settings file. If none is found, it looks for a project file, and if that isn't found, it tries to load a global settings file. In this way, local settings override project settings and project settings override global settings. Note, however, that if a local settings file exists, *all* settings are read from this file. Any project file or global file for the relevant output format is ignored.

You can create a project or global settings file through the menu in the submenu "Settings Files". This simply saves all settings for the current buffer to a project or global settings file. (Any local settings file for the file in the current buffer will be kept. You'll need to delete it manually if you no longer need it.)

The name of a global settings file has the form `<format>.pandoc`, where `<format>` obviously specifies the output format. `<format>` can also be the string `"default“`, however, in which case it specifies a default settings file, which is loaded by `pandoc-load-default-settings` when no default local or project settings file is found. In this way, you can override the default output format used for new files.

Note that starting with version 2.5, `pandoc-mode` settings files are written in a Lisp format (as demonstrated above). Old-style settings files continue to be read, so there is no need to change anything, but if you change any settings and save them, the file is converted.

File-local variables
====================

`pandoc-mode` also allows options to be set as file-local variables, which gives you the ability to keep the settings for a file in the file itself. To specify an option in this way, use the long form of the option as a variable name, prefixed with `pandoc/` (note the slash; use `pandoc/read` and `pandoc/write` for the input and output formats, and `pandoc/table-of-contents` for the TOC).

For example, in order to set a bibliography file, add the following line to the local variable block:

    pandoc/bibliography: "~/path/to/mybib.bib"

The easiest way to add a file-local variable is to use the command `M-x add-file-local-variable`. This will put the variable at the end of the file and add the correct comment syntax. Note that the values are Lisp expressions, which means that strings need to be surrounded with double quotes. Symbols do not need to be quoted, however.

Settings specified as file-local variables are kept separate from other settings: they cannot be set through the menu, they are *never* saved to a settings file, and they are not shown when you call `pandoc-view-settings` (`C-c / S`). A source file can both have a settings file and specify settings in file-local variables. If this happens, the latter override the former

Note that it is also possible to specify the customisation option `pandoc-binary` as a file-local variable. It does not require the `pandoc/` prefix, but since its value is a string, it must be enclosed in quotes:

    pandoc-binary: "/path/to/alternate/pandoc“

Managing numbered examples
==========================

Pandoc provides a method for creating examples that are numbered sequentially throughout the document (see [Numbered example lists](http://pandoc.org/README.html#numbered-example-lists) in the Pandoc documentation). `pandoc-mode` makes it easier to manage such lists. First, by going to "Example Lists | Insert New Example" (`C-c / e i`), you can insert a new example list item with a numeric label: the first example you insert will be numbered `(@1)`, the second `(@2)`, and so on. Before inserting the first example item, Emacs will search the document for any existing definitions and number the new items sequentially, so that the numeric label will always be unique.

Pandoc allows you to refer to such labeled example items in the text by writing `(@1)` and `pandoc-mode` provides a facility to make this easier. If you select the menu item "Example Lists | Select And Insert Example Label" (`C-c / e s`) Emacs displays a list of all the `(@)`-definitions in your document. You can select one with the up or down keys (you can also use `j` and `k` or `n` and `p`) and then hit `return` to insert the label into your document. If you change your mind, you can leave the selection buffer with `q` without inserting anything into your document.

Using @@-directives
===================

`pandoc-mode` includes a facility to make specific, automatic changes to the text before sending it to Pandoc. This is done with so-called `@@`-directives, which trigger an Elisp function and are then replaced with the output of that function. A `@@`-directive takes the form `@@directive`, where `directive` can be any user-defined string (see [How to define directive strings](#defining--directives)). Before Pandoc is called, Emacs searches the text for these directives and replaces them with the output of the functions they call.

So suppose you define (e.g., in `~/.emacs.d/init`) a function `my-pandoc-current-date`:

    (defun my-pandoc-current-date (_)
      (format-time-string "%d %b %Y"))

Now you can define a directive `@@date` that calls this function. The effect is that every time you write `@@date` in your document, it is replaced with the current date.

Note that the function that the directive calls must have one argument, which is used to pass the output format to the function (as a string). This way you can have your directives do different things depending on the output format. This argument can be called anything you like. In the above example, it is called `_` (i.e., just an underscore), to indicate that the variable is not actually used in the function. If you do use it, you should probably choose a more meaningful name.

`@@`-directives can also take the form `@@directive{...}`. Here, the text between curly braces is an argument, which is passed to the function called by the directive as the second argument. Note that there should be *no* space between the directive and the left brace. If there is, Emacs won't see the argument and will treat it as normal text.

It is possible to define a directive that can take an optional argument. This is simply done by defining the argument that the directive's function takes as optional. Suppose you define `my-pandoc-current-date` as follows:

    (defun my-pandoc-current-date (_ &optional text)
      (format "%s%s" (if text (concat text ", ") "")
                     (format-time-string "%d %b %Y")))

This way, you could write `@@date` to get just the date, and `@@date{Cologne}` to get "Cologne, 02 Nov 2016".

Two directives have been predefined: `@@lisp` and `@@include`. Both of these take an argument. `@@lisp` can be used to include Elisp code in the document which is then executed and replaced by the result (which should be a string). For example, another way to put the current date in your document, without defining a special function for it, is to write the following:

    @@lisp{(format-time-string "%d %b %Y")}

Emacs takes the Elisp code between the curly braces, executes it, and replaces the directive with the result of the code. Note that the code can be anything, and there is no check to see if it is “safe”.

`@@include` can be used to include another file into the current document (which must of course have the same input format):

    @@include{copyright.text}

This directive reads the file `copyright.text` and replaces the `@@include` directive with its contents.

Processing `@@`-directives works everywhere in the document, including in code and code blocks, and also in the %-header block. So by putting the above `@@lisp` directive in the third line of the %-header block, the meta data for your documents will always show the date on which the file was created by Pandoc.

If it should ever happen that you need to write a literal `"@@lisp"` in your document, you can simply put a backslash \\ before the first `@`: `\@@lisp`. Emacs removes the backslash (which is necessary in case the string `\@@lisp` is contained in a code block) and then continues searching for the next directive.

After Emacs has processed a directive and inserted the text it produced in the buffer, processing of directives is resumed from the *start* of the inserted text. That means that if an `@@include` directive produces another `@@include` directive, the newly inserted `@@include` directive gets processed as well.

Master file
-----------

If you have a master file with one or more `@@include` directives and you’re editing one of the included files, running Pandoc from that buffer will not produce the desired result, because it runs Pandoc on the included file. To make working with included files easier, you can specify a master file for them, with the command `pandoc-set-master-file` (through the menu with `C-c / o f m`). When this option is set, Pandoc is run on the master file rather than on the file in the current buffer.

The settings used in this case are always the settings for the master file, not the settings for the included file. The only exception is the output format, which is taken from the buffer from which you run Pandoc. This makes it possible to change the output format while in a buffer visiting an included file and have `pandoc-mode` do the right thing.

One thing to keep in mind is that the master file setting is dependent on the output format. When you set a master file, it is only set for the output format that is active. This means that you need to set the output format *before* you set the master file.

Note that the master file menu also has an option “Use this file as master file” (`C-c / o f M`). When you select this option, the current file is set as master file and a project settings file is created for the current output format. This is a quick way to set the master file for all files in a directory, since the project settings will apply to all files in the directory.

Defining @@-directives
----------------------

Defining `@@`-directives yourself is done in two steps. First, you need to define the function that the directive will call. This function must take at least one argument to pass the output format and may take at most one additional argument. It should return a string, which is inserted into the buffer. The second step is to go to the customisation buffer with `M-x customize-group` `RET` `pandoc` `RET`. One of the options there is `pandoc-directives`. This variable contains a list of directives and the functions that they are linked with. You can add a directive by providing a name (without `@@`) and the function to call. Note that directive names may only consists of letters (`a-z`, `A-Z`) or numbers (`0-9`). Other characters are not allowed. Directive names are case sensitive, so `@@Date` is not the same as `@@date`.

Passing more than one argument to an `@@`-directive is not supported. However, if you really want to, you could use `split-string` to split the argument of the `@@`-directive and "fake" multiple arguments that way.

A final note: the function that processes the `@@`-directives is called `pandoc-process-directives` and can be called interactively. This may be useful if a directive is not producing the output that you expect. By running `pandoc-process-directives` interactively, you can see what exactly your directives produce before the resulting text is sent to pandoc. The changes can of course be undone with `M-x undo` (usually bound to `C-/`).

Directive hooks
---------------

There is another customisable variable related to `@@`-directives: `pandoc-directives-hook`. This is a list of functions that are executed *before* the directives are processed. These functions are not supposed to change anything in the buffer, they are intended for setting up things that the directive functions might need.

Disabling the hydra menu
========================

The hydra package provides a nice way to control `pandoc-mode` and to set all the options that Pandoc provides. However, if for some reason you prefer to use normal key bindings, you can disable the hydra menu by rebinding `C-c /`. To restore the original key bindings, put the following in your init file:

    (with-eval-after-load 'pandoc-mode
      (define-key 'pandoc-mode-map "C-c / r" #'pandoc-run-pandoc)
      (define-key 'pandoc-mode-map "C-c / p" #'pandoc-convert-to-pdf)
      (define-key 'pandoc-mode-map "C-c / s" #'pandoc-save-settings-file)
      (define-key 'pandoc-mode-map "C-c / w" #'pandoc-set-write)
      (define-key 'pandoc-mode-map "C-c / f" #'pandoc-set-master-file)
      (define-key 'pandoc-mode-map "C-c / m" #'pandoc-set-metadata)
      (define-key 'pandoc-mode-map "C-c / v" #'pandoc-set-variable)
      (define-key 'pandoc-mode-map "C-c / V" #'pandoc-view-output)
      (define-key 'pandoc-mode-map "C-c / S" #'pandoc-view-settings)
      (define-key 'pandoc-mode-map "C-c / c" #'pandoc-insert-@)
      (define-key 'pandoc-mode-map "C-c / C" #'pandoc-select-@))

It’s also possible to bind other commands to keys. The switches (i.e., the options that can only be on or off) can be toggled with the command `pandoc-toggle-interactive`. All other options (except `--read`) have dedicated functions to set them, called `pandoc-set-<option>`, where `<option>` corresponds to the long form of the option without the double dashes (use `write` rather than `to`, and `table-of-contents` rather than `toc`).
