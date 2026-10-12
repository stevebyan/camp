#lang scribble/manual

@(require "doc-util.rkt"
          racket/runtime-path
          ;uncomment if you end up using #:style below
          ;scribble/core
          ;scribble/html-properties
          (for-label camp
                     punct/doc
                     racket/base))

@title[#:tag "gui-app"]{Camp Computer: the GUI client for Camp}

@define-runtime-path[app-window]{app.png}

@image[app-window #:scale 0.4 #; #:style #; (style #f (list (attributes '((style . "float: right;")))))]

Camp comes with @onscreen{Camp Computer}, a simple app that hides all the site scaffolding and removes
friction from common operations when you're ready to write. (The app is @emph{not} intended to be an
IDE for designing and developing the site itself.)

@margin-note{Camp Computer is tested on Mac OS; it should work on other platforms, but there may be
bugs.}

It must be installed separately with @tt{raco pkg install camp-app}. On Mac OS, if Camp is installed
in user scope, the GUI app will be copied to your user’s @filepath{Applications} folder automatically.

@section{User interface}

@bold{Page management:} You can double click on individual sources in the right-side pane to open
them in the @seclink["builtin-editor"]{built-in editor}. (To use an external editor instead, select
one by clicking @onscreen{File} menu → @onscreen{Preferences…}.) You can also right-click a source
file to get a context menu with @onscreen{Edit}, @onscreen{Preview} and @onscreen{Delete} options.
Previewing will start the project server (if it is not already started) and open your browser to the
localhost URL for that page.

The @onscreen{Status} column shows @onscreen{Draft} for any page whose @racket['draft?] meta is
present and not @racket[#f]. Click the column header to sort the drafts together.

The toolbar buttons are pretty straightforward:

@itemlist[
 
@item{@onscreen{New page}: Prompts you for metadata for the new page. If the current folder’s
@tech{collection} has any taxonomies defined, you can specify those as well.}
                                                            
 @item{@onscreen{Build site}: Same effect as running @secref["cli-build"]. Hold @kbd{⌘} while
 clicking to do a @deftech{full rebuild}: the build runs in a separate process, guaranteeing that all
 modules are loaded fresh. Use this if you suspect a change isn't being picked up by a normal build.
 A full rebuild is also available from @onscreen{File} menu → @onscreen{Full Rebuild}
 (or @kbd{⌘}@kbd{⇧}@kbd{B}).}
 
 @item{@onscreen{Start/Stop Preview}: Starts/stops the local server for previewing the site. While
 the server is running, changes are picked up automatically: edits to page sources and static files
 trigger a rebuild or sync, as do edits to render modules, to @racket[_.rkt] modules in the site
 root, and to the site config itself. (If a config edit fails to load, the error is logged and the
 app keeps running with the previous config.) A new browser tab is opened to the localhost URL for
 the server every time the server is started.

 Two changes are @emph{not} picked up live: changing the site's output folder requires stopping and
 restarting the preview server, and edits to helper modules in subfolders of the site root do not
 trigger a rebuild by themselves (they are still reloaded as part of the next build).}
 
 @item{@onscreen{Publish}: Same effect as running @secref["cli-deploy"].}
 
 ]

@subsection[#:tag "builtin-editor"]{The built-in editor}

Double-clicking a source (or creating a new page) opens it in a simple editor window, one window per
file. The editor provides:

@itemlist[

 @item{Syntax highlighting for every kind of file in a Camp site — Punct pages, @racketmodname[camp/site]
 and @racketmodname[camp/book] configs, and plain Racket modules — chosen automatically from each
 file's @hash-lang[] line. Punct pages are highlighted as Markdown: headings, emphasis, code spans
 and fenced blocks, links, list and quote markers, and the metadata block each get their own color,
 while ordinary prose stays in the plain text color.}

 @item{Saving (@kbd{⌘}@kbd{S}) immediately rebuilds the site — or, while the preview server is
 running, lets its file watcher do so. The right end of the status bar reads @onscreen{Modified}
 while the buffer has unsaved changes; each save replaces it with a Vim-style report of what was
 written and when (e.g. @tt{Saved: 8L, 246B written • 14:22 Aug 22}).}

 @item{Autocomplete: press @kbd{ctrl}@kbd{.} to complete identifiers defined in or imported by the
 file. Completions are recomputed in the background as you edit; if the file doesn't currently
 expand, the editor falls back to words already present in the buffer.}

 @item{Find (@kbd{⌘}@kbd{F}, or @onscreen{Edit ▸ Find}) reveals a field in the status bar: typing
 highlights all matches, @kbd{return} jumps to the next one, and @kbd{esc} hides the field and
 returns to the text.}

 @item{Hard-wrapping: @kbd{⌘}@kbd{J} (@onscreen{Edit ▸ Re-wrap Paragraph}) re-wraps the paragraph around the cursor to the wrap column
 set in @onscreen{Preferences…} (like Vim's @tt{gqip}), and it is Markdown-aware: a bulleted or
 numbered list item is wrapped by itself, keeping its marker and giving continuation lines a
 hanging indent; blockquoted text keeps its @tt{>} prefix on every line; and headings, code
 fences and their contents, tables, and the metadata block are left alone. The same rules apply as
 you type: when a typed character carries the current line past the wrap column, that line is
 broken where @kbd{⌘}@kbd{J} would break it, and the words that overflow move to the start of the
 paragraph's next line, which passes its own overflow down in turn until a line fits. Lines are
 never joined upward, and a line that ends in a Markdown hard break (two spaces or a backslash)
 keeps its overflow on a line of its own; use @kbd{⌘}@kbd{J} to close up a paragraph after
 deleting from it. The window's soft wrap is measured in pixels, so in a proportional font it
 cannot stand in for a character count. Turn this off with @onscreen{Break lines at the wrap column
 while typing} in @onscreen{Preferences…}.}

 @item{A centered text block, as in iA Writer: the margins grow as the window widens, and never
 shrink below a comfortable minimum.}

 @item{Line numbers (toggle them with @onscreen{View ▸ Line Numbers}, @kbd{⌘}@kbd{⇧}@kbd{L}, or in
 @onscreen{Preferences…}; they sit in the left margin, so the text stays put either way),
 parenthesis matching, and language-aware indentation.}

 @item{Optional Vim keybindings, toggled in @onscreen{Preferences…}; the mode shows as a badge at the
 right end of the status bar, and @onscreen{Modified} at the left end. These are provided by the
 @tt{drracket-vim-tool} package, which camp-app installs as a dependency (as a side effect, a
 @onscreen{Vim Mode} also becomes available in DrRacket).}

]

If the file is changed on disk by another program while open in the editor, the editor offers to
reload it.

@subsection{Site Management}

Camp Computer can manage multiple sites. Click @onscreen{Add site} to add an already-created site to
the site switcher.

To remove a site from the site switcher, click @onscreen{File} menu → @onscreen{Remove this site}.
This does not affect the site’s files, it only eliminates it as an option within the site selector.