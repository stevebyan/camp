#lang scribble/manual

@(require "doc-util.rkt"
          scribble/example
          hash-view/scribble
          (for-label (except-in racket/base date date?)
                     camp
                     camp/site
                     gregor
                     racket/contract
                     splitflap/constructs
                     toml/config/schema
                     (only-in xml xexpr?)))

@(define e (make-base-eval))
@(e '(require camp gregor camp/site))

@title[#:style 'quiet #:tag "mod-site"]{Site Configuration Language}

@defmodulelang[camp/site]

A Camp @deftech{site} is a single website, organized as a Racket package.

A site has one or more @deftech{collections}, which are named groups of pages with a sort order, a
mapping between source and output paths, and optional taxonomies for further organization.

@inline-note[#:type 'warning]{Note that the term @tech{collections} in Camp is different than
 Racket's own concept of @secref["Library_Collections" #:doc '(lib "scribblings/guide/guide.scrbl")].}

A @deftech{page} is a single source document.

A @deftech{feed} is an RSS or Atom feed which includes all @tech{pages} from a set of one or more
@tech{collections}. A @tech{site} may specify zero feeds, one feed, or multiple feeds.

The @hash-lang[] @racketmodname[camp/site] language provides a TOML-based configuration format for
defining Camp sites. Files written in this language are parsed and validated against the site
schema.

@codeblock|{
#lang camp/site

# Required values ------
title = "Site Title"
url = "https://example.com"
founded = 1912-07-04              # Note no quotes
authors = ["Me (me@example.com)"] # List of strings, must be in this format

# Optional values ------
sources = ".mypage"         # .md.rkt and .page.rkt are always recognized
static-folder = "res"       # default is "static"
output-folder = "public"    # default is "publish"
deploy-script = "deploy.sh"
default-render = "(camp-demo/render render-page)"
# → string datum: list of module and a function identifier
# function signature: Document, Context -> Xexpr

# Collections ----------

[[collections]]
name = "blog"
source = "blog/*"
output-paths = "blog/[yyyy]/[MM]/*/"
render-with = "(camp-demo/render render-post)" # same as default-render
order = "descending"
sort-key = "date"
taxonomies = ["tags", "series"]

[[collections]]
name = "pages"
source = "pages/*"
output-paths = "*/"
render-with = "(camp-demo/render render-page)"
sort-key = "title"
order = "ascending"

[[feeds]]
filename = "feed.atom" # Extension .atom or .rss sets format
collections = ["blog"] # List of collection names
render-with = "(camp-demo/feeds feed-content)" # same as default-render
}|

@section[#:tag "ref-site-required"]{Required Fields}

@tabular[#:style 'boxed
         #:pad '(1 0)
         #:row-properties '(bottom-border ())
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[title] @racket[string?] "Site title")
               (list @racket[url] @racket[valid-url-string?] "Base URL (must be valid)")
               (list @racket[founded] @racket[date?] "Founding date for tag URI generation")
               (list @racket[authors] @racket[(listof author-string?)] "Authors in \"Name (email)\" format"))]

@defproc[(author-string? [v any/c]) boolean?]{

 Returns @racket[#t] if @racket[_v] is a string in @racket["Name (email@example.com)"] format, where @tt{email}
 is a valid email address per @racket[email-address?] from @racketmodname[splitflap].

 @examples[
 #:eval e
 (author-string? "Me (me@example.com)")
 (author-string? "Me (me@1.com)")
 (author-string? " (me@example.com)")]

}

@section[#:tag "ref-site-optional"]{Optional Fields}

@tabular[#:style 'boxed
         #:pad '(1 0)
         #:row-properties '(bottom-border ())
         (list (list @bold{Field} @bold{Type} @bold{Default} @bold{Description})
               (list @racket[sources] @racket[non-rkt-file-extension?] @racket[".md.rkt"] "Source file extension")
               (list @racket[static-folder] @racket[path-string?] @racket["static"] "Static assets directory")
               (list @racket[output-folder] @racket[path-string?] @racket["publish"] @nonbreaking{Build output directory})
               (list @racket[deploy-script] @racket[path-string?] @racket[#f] @nonbreaking{Deployment script path})
               (list @nonbreaking[@racket[default-render]] @racket[render-spec?] @racket[#f] @nonbreaking{Default render function})
               (list @racket[timezone] @racket[timezone-name?] @racket[#f] @nonbreaking{Time zone for dates (see @secref["ref-site-dates"])}))]

@defproc[(timezone-name? [v any/c]) boolean?]{

 Returns @racket[#t] if @racket[_v] is a string naming a time zone in the IANA time zone database.

@examples[
 #:eval e
 (timezone-name? "America/Chicago")
 (timezone-name? "Etc/UTC")
 (timezone-name? "Central")]

}

@subsection[#:tag "ref-site-dates"]{Dates and time zones}

Camp reads the @tt{date} metadata of each page as a @racket[moment]. The value can be a string in
any format accepted by @racket[infer-moment]: a date such as @racket["2025-01-15"], optionally
followed by a time (after @litchar{T} or a space) and a UTC offset, as in
@racket["2025-01-15T09:30"] or @racket["2025-01-15 09:30-05:00"]. It can also be a gregor
@racket[date], @racket[datetime] or @racket[moment]. Any other value, including an empty string,
raises an exception when the site is collected.

A date without a time is taken to be the start of that day. If the site has no @racket[timezone]
setting, dates without an offset are in @racket[current-timezone] (normally the time zone of the
computer building the site), and dates with an offset keep it.

If the site sets @racket[timezone], dates without an offset are in that time zone, and dates with an
offset are converted to it. A converted date refers to the same instant (it is @racket[moment=?] to
the original), but its date and time fields may change: @racket["2025-01-15T23:30-08:00"] on a site
with @racket[timezone] set to @racket["Etc/UTC"] falls on January 16 in output paths and
@racket[~d].

@section[#:tag "ref-site-collections"]{Collections}

Each @tt{[[collections]]} entry defines a group of source documents:

@tabular[#:style 'boxed
         #:pad '(1 0)
         #:row-properties '(bottom-border ())
         (list (list @bold{Field} @bold{Type} @bold{Default} @bold{Description})
               (list @racket[name] @racket[string?] "" "Collection identifier")
               (list @racket[source] @nonbreaking[@racket[source-path-pattern?]] "" @nonbreaking{Location of sources})
               (list @nonbreaking[@racket[output-paths]] @racket[output-path-pattern?] "" "Defines output paths/URLs")
               (list @racket[render-with] @racket[render-spec?] "" @nonbreaking{Render function specification})
               (list @racket[taxonomies] @racket[(listof string?)] "" "(Optional) metadata keys")
               (list @racket[sort-key] @racket[string?] @racket{date} "Metadata sort key")
               (list @racket[order] @racket[(or/c "ascending" "descending")] @racket{descending} @nonbreaking{Sort order})
               )]

@defproc[(render-spec? [v any/c]) (or/c #f (listof module-path? symbol?))]{
                                                                           
 Validates that @racket[_v] is a string containing a two-element list, with the first element being a
 @racket[module-path?] and the second being an identifier. Returns the two-element list if validation
 succeeds, or @racket[#f] otherwise.
 
 In order to be valid, at site build time the identifier must be that of a function
 @racket[provide]d by the module, and the function must have the signature
 @racket[(-> document? context? xexpr?)]. This information is not checked by @racket[render-spec?],
 however.
 
 @examples[#:eval e
           (render-spec? "(my-module render-func)")
           (render-spec? "(\"mod.rkt\" func)")
           (render-spec? "(100)")
           ]
 
}

@section[#:tag "ref-site-feeds"]{Feeds}

Each @tt{[[feeds]]} entry defines an RSS or Atom feed:

@tabular[#:style 'boxed
         #:pad '(1 0)
         #:row-properties '(bottom-border ())
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[filename] @racket[feed-filename?] "Output filename (.atom or .rss)")
               (list @racket[collections] @racket[(listof string?)] "Collection names to include")
               (list @racket[render-with] @racket[render-spec?] "Feed content render function"))]

Each feed's @racket[_render-with] value should identify a function with the same signature as page
render functions:

@codeblock|{
(define (feed-content doc ctxt)
  ;; doc: the Punct document
  ;; ctxt: same context as page render functions (slug, url, collection, etc.)
  ;; Returns x-expression for the feed entry body
  `(article ,@(document-body doc)
            (p (a ((href ,(context-url ctxt))) "Read more..."))))
}|

The @racket[ctxt] argument is a @racket[context] whch provides access to the page's canonical URL,
enabling feed content to include links back to the original page on your site.

Every page in the feed's collections becomes an entry, except drafts and pages without a @tt{date}.
The page's @tt{date} is the entry's publication time. A page can also have an @tt{updated} value, in
any format accepted for @tt{date} (see @secref["ref-site-dates"]); it becomes the entry's update time,
which is otherwise the same as its @tt{date}. An @tt{updated} value earlier than the @tt{date} raises
an exception.

Entries are ordered by update time, most recent first, and the most recent update time is also the
update time of the feed itself. Adding or changing @tt{updated} on an older page therefore moves it
to the top of the feed. RSS items have only a publication date, so in RSS feeds @tt{updated} affects
only the order of items and the feed's @tt{lastBuildDate}.

@defproc[(feed-filename? [v any/c]) boolean?]{
                                              
 Returns @racket[#t] if @racket[_v] is a string ending in @filepath{.atom} or @filepath{.rss}.

@examples[
 #:eval e
 (feed-filename? "posts.atom")
 (feed-filename? "blog.rss")
 (feed-filename? "comments")]
         
}


@;------------------------------------------------
@section[#:tag "ref-path-mapping"]{Source/Output Path Mapping}

@declare-exporting[camp/main]

Camp uses path patterns to map source files to output locations.

A @deftech{source path pattern} specifies where to find source documents within a collection (e.g.,
@racket["blog/*"]). An @deftech{output path pattern} specifies the URL structure for rendered pages,
with support for slug substitution and date-based paths (e.g., @racket["blog/[yyyy]/[MM]/*/"]).

An @tech{output path pattern} specifies the folder/file structure (and thus the URL) for rendered
pages, with support for slug substitution, date-based paths, and meta value interpolation. In output
path patterns:

@itemlist[
 @item{Any folder name consisting only of @litchar{*} will be replaced by the source’s @tech{slug}.}
  
 @item{Any name inside a pair of brackets @litchar{[]} will first be looked up as a key in the
  source’s metadata. If a matching meta key is found, the bracket is replaced by that value. Otherwise,
  the name is interpreted as a CLDR date format code and formatted using the source’s @tt{date} meta.}
 
 @item{If the pattern ends in a trailing slash @litchar{/}, the output file will be named
  @filepath{index.html}. Otherwise the output is the name of the pattern’s final element with an
  added @filepath{.html} extension.}
 
 @item{A pattern must contain at least one @litchar{*} or @litchar{[]} element.}
 ]

@defproc[(source-path-pattern? [v any/c]) boolean?]{
 
 Returns @racket[#t] if @racket[_v] is a valid @tech{source path pattern}: a relative path string
 that does not contain @tt{.} or @tt{..} components, and whose final element is @litchar{*}.

 @examples[
 #:eval e
 (source-path-pattern? "writing/*")
 (source-path-pattern? "/writing/*")
 (source-path-pattern? "writing/")
 (source-path-pattern? "../writing/*")]
 
}

@defproc[(output-path-pattern? [v any/c]) boolean?]{
                                                    
 Returns @racket[#t] if @racket[_v] is a valid @tech{output path pattern}: a relative path string
 that does not contain @tt{.} or @tt{..} components, and contains at least one @tt{*} element or
 bracketed pattern.
 
 @examples[
 #:eval e
 (output-path-pattern? "posts/*/")
 (output-path-pattern? "posts/[YYYY]/*/")
 (output-path-pattern? "../posts/*/")
 (output-path-pattern? "posts/")]

}

@defproc[(format-output-path [pattern output-path-pattern?]
                             [slug string?]
                             [date (or/c date-provider? #f)]
                             [metas (or/c hash? #f)])
         path?]{
Applies an @tech{output path pattern} to produce an output file path. The @racket[_slug] replaces
@tt{*} in the pattern. Bracketed patterns are resolved by first checking @racket[_metas] for a
matching key; if no match is found, the pattern is interpreted as a CLDR date code and formatted
using @racket[_date].

@examples[#:eval e
(format-output-path "posts/*/" "hello-world" #f #f)
(format-output-path "blog/[yyyy]/[MM]/*/" "my-post" (date 2025 1 15) #f)
(format-output-path "newsletter/[issue]/*/" "my-post" #f (hasheq 'issue 42))
]

}

@defproc[(file-extension? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid file extension: a string or byte string starting with
@tt{.} and containing no directory separators.}

@defproc[(non-rkt-file-extension? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a valid file extension other than @racket[".rkt"].

@examples[
 #:eval e
 (non-rkt-file-extension? ".myformat.rkt")
 (non-rkt-file-extension? ".rkt")]

}

@; =============================================================================
@section[#:tag "ref-site-config"]{Site Configuration API}

@declare-exporting[camp]

The following data types from @racketmodname[camp] underlie the site configuration language. They
are represented as @deftech{hash-views}: hash tables with struct-like accessor functions. For more
information on hash-views, see @other-doc['(lib "hash-view/hash-view.scrbl")].

@defhashview[site ([title string?]
                   [url valid-url-string?]
                   [founded date-provider?]
                   [authors (listof string?)]
                   [sources string? #:default ".md.rkt"]
                   [static-folder string? #:default "static"]
                   [output-folder string? #:default "publish"]
                   [collections (listof collection?)]
                   [racket-collection (or/c string? #f) #:default #f]
                   [deploy-script (or/c string? #f) #:default #f]
                   [default-render (or/c list? #f) #:default #f]
                   [timezone (or/c timezone-name? #f) #:default #f]
                   [feeds (listof feed-config?) #:default '()])]{

A @tech{hash-view} representing a @tech{site} configuration. A site is most commonly defined using
@hash-lang[] @racketmodname[camp/site].

Required fields are @racket[title], @racket[url], @racket[founded], @racket[authors], and
@racket[collections].

The @racket[racket-collection] field is set automatically by @racket[load-site] from the package's
@filepath{info.rkt}. It contains the Racket collection name (e.g., @racket["myblog"]) and is
@racket[#f] if the site is not installed as a package.}

@defhashview[collection ([name string?]
                          [source source-path-pattern?]
                          [output-paths output-path-pattern?]
                          [render-with (or/c list? #f)]
                          [order string? #:default "descending"]
                          [sort-key string? #:default "date"]
                          [taxonomies (listof string?)])]{

A @tech{hash-view} representing a @tech{collection} configuration. Collections are most commonly
defined as part of a @hash-lang[] @racketmodname[camp/site] configuration module.

Required fields are @racket[name], @racket[source], and @racket[output-paths].}

@defhashview[feed-config ([filename string?]
                           [collections (listof string?)]
                           [render-with list?])]{

A @tech{hash-view} representing a @tech{feed} configuration. Feed configurations are most commonly
defined as part of a @hash-lang[] @racketmodname[camp/site] configuration module. All fields are
required.}

@defproc[(load-site [mod-path (or/c path-string? module-path?
                                     (and/c hash? (λ (h) (hash-has-key? h 'path))))]) site?]{
Loads a site configuration from a @hash-lang[] @racketmodname[camp/site] module. The
@racket[_mod-path] can be:
@itemlist[
  @item{A filesystem path to a @filepath{site.rkt} file}
  @item{A module path like @racket['my-site/site]}
  @item{A hash containing a @racket['path] key (such as a book configuration returned by
        @racket[load-book])---the site is discovered from the package's @filepath{info.rkt}}]
Returns the parsed site configuration as a hash-view with an additional @racket['root] key
containing the absolute path to the site's directory.

Calling @racket[load-site] again in the same process reflects any changes saved to the
configuration module in the meantime; the GUI app and @tt{raco camp serve} rely on this to
reload the site when its configuration changes.

@racket[load-site] marks the site's directory as the boundary of the site's compiled bytecode. In a
one-shot command like @tt{raco camp build}, Camp compiles any site module whose bytecode is missing
or older than its source before loading it, as @exec{raco make} would, so later builds load it
without expanding it again. Modules outside the boundary are never compiled by Camp.

In a live-reloading context (the GUI app, or @tt{raco camp serve} with watching enabled), the
boundary instead delimits a private bytecode cache (a @filepath{camp-live} subfolder of each
@filepath{compiled} folder). Bytecode produced by @tt{raco setup} prevents modules from being
reloaded after edits, so live sessions instead compile site modules into this cache without that
restriction, and reuse it across sessions: unchanged modules never recompile, though a module is
rebuilt when a library it links against gets recompiled (for example by @tt{raco setup} after a
Racket upgrade). Live sessions neither load nor disturb the regular bytecode, so one-shot commands
keep their own compile cache.}

@defproc[(resolve-site-spec [spec (or/c path? string? symbol?)]) (or/c path? #f)]{
Resolves a site specification to a path. The @racket[_spec] can be:
@itemlist[
  @item{A @racket[path?]: Returns the path if it exists as a file, @racket[#f] otherwise.}
  @item{A @racket[string?]: If it exists as a file, returns it as a path. Otherwise, treats it as
        a collection name and searches installed packages.}
  @item{A @racket[symbol?]: Treats the symbol as a collection name and searches installed packages.}]

When searching by collection name, finds packages with a @racket['camp-site] field in their
@filepath{info.rkt} whose @racket['collection] name matches.

@codeblock|{
(resolve-site-spec "site.rkt")       ; path if file exists
(resolve-site-spec "myblog")         ; finds myblog package
(resolve-site-spec 'myblog)          ; same, with symbol
}|}

@defproc[(file-path->site-path [file-path path-string?]) path?]{
Discovers the site configuration path for a file within a Camp package. Uses
@racket[path->pkg+subpath] to find the package root, then reads the @racket['camp-site] field
from the package's @filepath{info.rkt}.

Raises an error if the file is not in a package, no @filepath{info.rkt} exists, or the
@racket['camp-site] field is not defined.}

@defproc[(load-book [file-path path-string?]) book?]{
Loads a book configuration from a @hash-lang[] @racketmodname[camp/book] module. Returns the
parsed book configuration as a hash-view with an additional @racket['path] key containing the
absolute path to the book file.

To load the associated site for a book:

@codeblock|{
(define mybook (load-book "path/to/my.book.rkt"))
(define mysite (load-site book))  ; discovers site from package info.rkt
}|}
