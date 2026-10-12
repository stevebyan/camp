#lang scribble/manual

@(require "doc-util.rkt"
          hash-view/scribble
          (for-label (except-in racket/base date date?)
                     camp
                     camp/build
                     gregor
                     punct/doc))

@title[#:style 'quiet #:tag "ref-rendering"]{Rendering and Context}

@defmodule[camp]

A @deftech{render context} is a table containing information about a @tech{page} and its context
within the current site build. It is provided render functions and provides navigation helpers and
metadata.

@defhashview[context ([slug string?]
                       [url string?]
                       [collection string?]
                       [taxonomies hash?]
                       [source (or/c string? #f) #:default #f])]{

A @tech{hash-view} representing a @tech{render context}. The context is passed to render functions
and provides access to taxonomy data.

The @racket[url] field contains the canonical URL for the current page. The @racket[taxonomies]
field maps taxonomy keys to lists of normalized values for the current page. Use the @racket[prev]
and @racket[next] functions with a context to navigate between pages.

The @racket[source] field is the path of the page's source file, relative to the site root (the
folder containing the @tt{camp/site} file), with @racket["/"] as the separator on every platform:
for example, @racket["blog/hello-world.md.rkt"]. It does not depend on the page's slug, so it is
suitable for building links to the file in a version control web interface. Every page generated
by a paginated @tt{camp/page} document has the same @racket[source]. Contexts constructed by
other code may omit it, in which case @racket[context-source] returns @racket[#f].}

@section[#:tag "ref-html-rendering"]{HTML Rendering}

@defproc[(camp-doc->html-xexpr [doc document?]
                               [fallback (or/c (-> symbol? list? list? (or/c any/c #f)) #f) #f])
         (listof any/c)]{

Renders a Punct document to HTML x-expressions with cross-references resolved. This is the primary
function for converting document bodies in render functions.

For @hash-lang[] @racketmodname[camp/page] documents, calls the body thunk to produce the content.
For Punct documents, renders the document content with:

@itemlist[
  @item{@racket[term] elements resolved to hyperlinks}
  @item{@racket[term-definition] elements rendered as @tt{<dfn>} with anchors}
  @item{@racket[page-ref] elements resolved to page links}]

Returns a list of x-expressions (the body content without an outer wrapper).

The optional @racket[_fallback] procedure handles custom elements. It receives the tag name,
attributes, and rendered child elements. Return an x-expression to override rendering, or
@racket[#f] to use the default (passthrough) behavior.

@codeblock|{
(define (my-fallback tag attrs elems)
  (if (eq? tag 'callout)
      `(aside ((class "callout")) ,@elems)
      #f))

(define body (camp-doc->html-xexpr doc my-fallback))
}|}

@section[#:tag "ref-collection-retrieval"]{Collection Retrieval}

@defstruct[page-link ([url string?]
                      [title string?]
                      [metas hash?])
                     #:transparent]{
A lightweight reference to a page, used for navigation and collection retrieval. Contains the
page's URL, title, and full metadata hash.}

@defproc[(page-link-doc [pl page-link?]) document?]{
Retrieves the full Punct document for a @racket[page-link]. This allows paginated pages to display
full post content rather than just titles and metadata.

Must be called within a build context (i.e., when @racket[current-site-info] is set).

@codeblock|{
(paginate "blog" #:per-page 5
  (λ (items pagination)
    `(main
      ,@(for/list ([p items])
          `(article
            (h2 ,(page-link-title p))
            ,(camp-doc->html-xexpr (page-link-doc p))))
      ,(pagination-nav pagination))))
}|}

@defproc[(get-collection [name string?]
                         [#:limit limit (or/c #f exact-positive-integer?) #f]
                         [#:full-docs? full-docs? boolean? #f]
                         [#:include-drafts? include-drafts? any/c #f])
         list?]{
Retrieves pages from a named collection. Returns a list of @racket[page-link] structs by default,
or full Punct documents if @racket[_full-docs?] is @racket[#t]. The optional @racket[_limit]
restricts the number of results returned.

Pages whose metadata contains a @tt{draft?} key with any non-@racket[#f] value are excluded by
default. Pass a true value for @racket[_include-drafts?] to include them. The @racket[_limit] is
applied after draft filtering.}

@section[#:tag "ref-taxonomy-functions"]{Taxonomy Functions}

@defproc[(get-taxonomy-terms [collection-name string?]
                             [taxonomy-key string?])
         (listof string?)]{
Returns all distinct values for a taxonomy within a collection, ordered by first appearance in the
collection's sort order.}

@defproc[(get-taxonomy-pages [collection-name string?]
                             [taxonomy-key string?]
                             [term (or/c string? #f) #f])
         (or/c (listof page-link?) hash?)]{
With two arguments, returns a hash mapping each taxonomy term to its list of page-links.
With three arguments (including a specific @racket[_term]), returns the list of page-links
for that term.}

@section[#:tag "ref-page-navigation"]{Page Navigation}

@defproc[(prev [ctxt context?]
               [taxonomy-key string? #f]
               [term string? #f])
         (or/c page-link? #f)]{

Returns the previous page relative to the current page, or @racket[#f] if there is no previous page.

With only @racket[_ctxt], navigates within the current page's collection in sort order.

With @racket[_taxonomy-key], navigates within the pages sharing the current page's first value for
that taxonomy. For example, if the current page has @tt{tags: emacs, lisp}, then
@racket[(prev ctxt "tags")] navigates within pages tagged @racket["emacs"].

With both @racket[_taxonomy-key] and @racket[_term], navigates within pages having that specific
taxonomy term.

@codeblock|{
(prev ctxt)                  ; previous in collection
(prev ctxt "tags")           ; previous in first tag
(prev ctxt "tags" "emacs")   ; previous in "emacs" tag
}|}

@defproc[(next [ctxt context?]
               [taxonomy-key string? #f]
               [term string? #f])
         (or/c page-link? #f)]{

Returns the next page relative to the current page, or @racket[#f] if there is no next page.

Calling conventions are the same as @racket[prev]: with only @racket[_ctxt], navigates within the
collection; with @racket[_taxonomy-key], navigates within pages sharing the current page's first
value for that taxonomy; with both @racket[_taxonomy-key] and @racket[_term], navigates within
pages having that specific term.

@codeblock|{
(next ctxt)                  ; next in collection
(next ctxt "tags")           ; next in first tag
(next ctxt "tags" "emacs")   ; next in "emacs" tag
}|}

@section[#:tag "ref-page-filtering"]{Page Filtering}

@defproc[(filter-pages [pages (listof page-link?)]
                       [#:date-from date-from (or/c date-provider? #f) #f]
                       [#:date-to date-to (or/c date-provider? #f) #f]
                       [#:date-key date-key symbol? 'date]
                       [#:taxonomies taxonomies hash? #hasheq()])
         (listof page-link?)]{
Filters a list of page-links by date range and/or taxonomy values. All filters are combined with
AND logic---pages must pass all specified filters to be included.

@itemlist[
  @item{@racket[#:date-from] and @racket[#:date-to]: Inclusive date bounds, compared by calendar
        date only. Pages without a date are excluded when date filtering is active, and an
        unparseable date raises an exception (see @secref["ref-site-dates"]).}
  @item{@racket[#:date-key]: The metadata key to use for date comparison (default: @racket['date]).}
  @item{@racket[#:taxonomies]: A hash where keys are taxonomy names (strings or symbols) and values
        are either a single string (exact match) or a list of strings (match any).}]

This function is useful for building custom page sets or for book parts that filter collections:

@codeblock|{
(require gregor)  ; for date constructor

;; Get 2024 posts tagged "featured"
(filter-pages (get-collection "blog")
              #:date-from (date 2024 1 1)
              #:date-to (date 2024 12 31)
              #:taxonomies (hasheq 'tags "featured"))

;; Get posts in any of these categories
(filter-pages posts
              #:taxonomies (hasheq 'category '("tech" "science")))
}|

Taxonomy values in page metadata can be comma-separated strings or lists; both formats are
normalized before matching.}

@section[#:tag "ref-output-files"]{Additional Output Files}

@defparam[current-output-dir dir (or/c path? #f)]{
A parameter containing the complete path of the site's output folder. Set by @racket[build!] and
@racket[collect/call-with-page] for the duration of the render; @racket[#f] otherwise.

Render functions can use this to write files that accompany a page, such as generated images.
A build leaves such files in place, with two exceptions: @filepath{.html} files that the build did
not itself produce are deleted at the end of each build, and a clean build empties the whole output
folder first. Files that are expensive to produce are best kept in a cache folder elsewhere and
copied into place when missing:

@codeblock|{
(define-runtime-path card-cache "card-cache")

(define (ensure-card! doc ctx)
  (define name (~a (context-slug ctx) ".png"))
  (define cached (build-path card-cache name))
  (define published (build-path (current-output-dir) "cards" name))
  (unless (file-exists? cached)
    (make-parent-directory* cached)
    (draw-card! doc cached))
  (unless (file-exists? published)
    (make-parent-directory* published)
    (copy-file cached published)))
}|}

@section[#:tag "ref-date-formatting"]{Date Formatting}

@defproc[(~t [t date-provider?] [pattern string?]) string?]{
Formats a date using CLDR (Unicode Common Locale Data Repository) patterns. This function is
re-exported from the @racketmodname[gregor] library.

Common pattern elements:
@tabular[#:sep @hspace[2]
         (list (list @bold{Pattern} @bold{Output} @bold{Example})
               (list @tt{yyyy} "4-digit year" "2025")
               (list @tt{yy} "2-digit year" "25")
               (list @tt{MMMM} "Full month name" "January")
               (list @tt{MMM} "Abbreviated month" "Jan")
               (list @tt{MM} "2-digit month" "01")
               (list @tt{M} "1 or 2-digit month" "1")
               (list @tt{dd} "2-digit day" "05")
               (list @tt{d} "1 or 2-digit day" "5")
               (list @tt{EEEE} "Full weekday name" "Wednesday")
               (list @tt{EEE} "Abbreviated weekday" "Wed"))]

Patterns can be combined with literal text:
@codeblock|{
(~t my-date "MMMM d, yyyy")    ; "January 15, 2025"
(~t my-date "d MMM yyyy")      ; "15 Jan 2025"
(~t my-date "yyyy-MM-dd")      ; "2025-01-15"
(~t my-date "EEE, MMM d")      ; "Wed, Jan 15"
}|

See the @hyperlink["http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table"]{CLDR
documentation} for the complete list of pattern symbols.}

@defproc[(~d [pattern string?] [v (or/c string? date-provider?)]) string?]{
Formats a date using CLDR patterns, automatically converting date strings. This is the
recommended function for formatting dates from page metadata, since Punct documents provide dates
as strings. The pattern comes first, matching Racket's @racket[format] convention.

@codeblock|{
;; In a render function:
(define date (meta-ref doc 'date))  ; might be "2025-01-15" or a date object

(~d "d MMM yyyy" date)      ; "15 Jan 2025" - works either way
(~d "yyyy-MM-dd" date)      ; "2025-01-15" - for datetime attributes
(~d "h:mm a" "2025-01-15T09:30")  ; "9:30 AM"
}|

If @racket[_v] is already a @racket[date-provider?], it is passed directly to @racket[~t]. If
@racket[_v] is a string, it is first parsed as a @racket[moment] in the same way as @tt{date}
metadata (see @secref["ref-site-dates"]).}


