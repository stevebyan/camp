#lang scribble/manual

@(require (for-label camp
                     racket/base)
          "doc-util.rkt")
                     

@title[#:tag "quickstart"]{Quick Start}

This guide gets you from zero to a working Camp site in a few minutes using the built-in site
template. If you’d rather understand each piece as you build it, skip ahead to
@secref["tutorial-blog"], which constructs a site from scratch.

@section[#:tag "qs-installation"]{Installation}

Camp requires Racket 8.13 or later. If you don't have Racket installed, download it from
@hyperlink["https://racket-lang.org"]{racket-lang.org}. The installation includes DrRacket (an IDE)
and the @tt{raco} command-line tool you'll use to interact with Camp.

Install Camp from the package server by running:

@terminal{@:>{raco pkg install camp}}

This installs Camp and its dependencies. The process may take a minute or two as Racket downloads
and compiles the packages.

@section[#:tag "qs-creating-site"]{Creating a Site}

Camp includes a @tt{new} command that generates a starter site with sensible defaults. Create a new
site called ``mysite'' by running:

@ensure-sandbox-state['gone]

@terminal{
@:>{raco camp new mysite}
@sandbox-raco{camp new mysite}}

This creates a @filepath{mysite} directory containing everything you need: a site configuration, a
sample blog post, a simple render module, basic CSS, and package metadata. Change into the new
directory to explore what was created:

@terminal{@:>{cd mysite}}

The key files are @filepath{site.rkt}, which configures your site's structure, and
@filepath{render.rkt}, which defines how pages are transformed into HTML. The @filepath{main.rkt}
file provides bindings that all source documents can access. The @filepath{blog} folder contains
source documents, and @filepath{static} holds assets like stylesheets that are copied to the output
unchanged.

@section[#:tag "qs-building"]{Building and Previewing}

As detailed later in @secref["basic-concepts"], a Camp site is a Racket package. Before you can build
the site, you must install it:

@terminal{@:>{raco pkg install}}

Build the site with:

@terminal{@:>{raco camp build}}

Camp reads your configuration, processes each source document through its designated render
function, and writes the results to the @filepath{publish} folder. You'll see output indicating
which phases completed and how long each took.

To preview your site locally, start the development server:

@terminal{@:>{raco camp serve}}

Open @hyperlink["http://localhost:8000"]{http://localhost:8000} in your browser to see your site.
The server watches for changes to your source files; when you edit a document or template, Camp
automatically rebuilds and you can refresh the browser to see your changes.

Press @kbd{Ctrl}@kbd{C} in the terminal to stop the server when you're done.

@section[#:tag "qs-package-integration"]{Package Integration}

Notice that source files in the generated site start with @tt{#lang punct mysite} rather than plain
@tt{#lang punct}. This tells Punct to import all bindings from @filepath{mysite/main.rkt}, giving
your source documents access to Camp's collection and cross-reference functions.

Once installed, you can also build your site by name from anywhere:

@terminal{@:>{raco camp build mysite}}

This is equivalent to running @tt{raco camp build} from within the site directory.

@section[#:tag "qs-next-steps"]{Next Steps}

You now have a working Camp site. The generated template is intentionally minimal---it's meant as a
starting point, not a finished design.

Continue to @secref["basic-concepts"] to learn the basic concepts behind Camp.
Then to understand how the pieces fit together and customize your site, continue with
@secref["tutorial-blog"], which walks through building a blog from scratch. You'll learn how to
configure collections, write render functions, and generate feeds. The @secref["reference"] section
provides complete documentation of Camp's modules and functions when you need to look something up.

@ensure-sandbox-state['gone]