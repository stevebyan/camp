#lang scribble/manual

@(require "doc-util.rkt"
          (for-label camp))

@title[#:style 'quiet #:tag "cli"]{Command-Line Interface}

Camp provides several commands through Racket's @tt{raco} tool. All commands operate on the site
in the current directory (or the directory containing the current package).

@section[#:tag "cli-build"]{@tt{raco camp build}}

Builds the site by running the collect and build passes. Source documents are processed, rendered
through their collection's render function, and written to the output folder (default:
@filepath{publish}).

@terminal{
@rem{build ./site.rkt}
@:>{raco camp build}

@rem{build using named file}
@:>{raco camp build path/to/site.rkt}

@rem{build installed package "myblog"}
@:>{raco camp build myblog}
}

@itemlist[

@item{The @DFlag{fresh} flag clears the output folder before building. Without this flag, existing files
are overwritten but stale files from previous builds may remain.}

@item{The @DFlag{verbose} flag enables detailed output, including timing for the site loading phase and
information about individual file operations.}

@item{The @DFlag{drama} flag treats warnings as errors, causing the build to fail if any warnings are
logged. This is useful in continuous integration environments where you want strict validation.}

]

Build output shows timing for each phase (Collect, Build, Feeds, Static) and summarizes any
warnings at the end. A typical build looks like:

@terminal{
  Collect   42 pages                           23ms
  Build     42 pages                          156ms
  Feeds     1 feed                              8ms
  Static    12 files                           14ms
  ────────────────────────────────────────────────
  Total                                       201ms
}

@section[#:tag "cli-serve"]{@tt{raco camp serve}}

Starts a development server and optionally watches for file changes. By default, the server
listens on port 8000 and automatically rebuilds when source files change.

The @tt{[site]} argument can be a path to @filepath{site.rkt} or the name of an installed package.
If omitted, looks for @filepath{site.rkt} in the current directory.

The @DFlag{port} flag specifies an alternative port number.

The @DFlag{no-watch} flag disables file watching, running only the static server without automatic
rebuilds.

When watching is enabled, Camp monitors source documents, render modules, static files, and the
site configuration. Changes trigger appropriate actions: source document or render module changes
trigger a full rebuild, static file changes sync to the output folder, and configuration changes
reload the site and rebuild. Camp checks the modification times of these files twice a second, so
an edit, a new file or a deleted file is noticed within about half a second of being saved.

A rebuild renders every page, but reloads only the modules an edit affects: the edited module and
every source document or render module that depends on it, directly or through other modules of
the site. Each reload is logged as @tt{Reloaded} followed by the module's path. While watching,
the site's modules are compiled into a private bytecode cache (see @racket[load-site]); this does
not affect @tt{raco camp build}, which uses and updates the regular compiled bytecode.

The server provides directory listings for folders without an @filepath{index.html} and returns
a styled 404 page for missing files.

@section[#:tag "cli-deploy"]{@tt{raco camp deploy}}

Runs the deployment script specified in your site configuration. The script receives the output
folder path as its first argument.

The @tt{[site]} argument can be a path to @filepath{site.rkt} or the name of an installed package.
If omitted, looks for @filepath{site.rkt} in the current directory.

If no @tt{deploy-script} is configured in @filepath{site.rkt}, this command reports an error.

A typical deployment script might rsync files to a server, push to a Git repository, or upload
to a hosting service:

@filebox["deploy.sh"]{@verbatim{
#!/bin/bash
rsync -avz --delete "$1/" user@"@"server:/var/www/mysite/
}}

Configure it in your site:

@codeblock{
deploy-script = "./deploy.sh"
}

@section[#:tag "cli-new"]{@tt{raco camp new}}

Creates a new site from the built-in template. The @tt{<name>} argument specifies the directory
to create.

The generated site includes a minimal but functional structure: package metadata, site
configuration, a sample post, render functions, and basic CSS. It's designed as a starting point
for customization rather than a production-ready theme.

After creating a site, install it as a local package and build:

@terminal{
@:>{cd <name>}
@:>{raco pkg install}
@:>{raco camp serve}
}