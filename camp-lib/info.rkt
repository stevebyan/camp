#lang info
(define collection "camp")
(define deps '("mime-type-lib"
               "base"
               "gregor-lib"
               "hash-view-lib"
               "html-printer-lib"
               "punct-lib"
               ["splitflap-lib" #:version "1.4.1"]
               "toml-config-lib"
               "tzinfo"
               "web-server-lib"))
(define pkg-desc "Implementation part of Camp")
(define version "1.0")
(define pkg-authors '("Joel Dueck"))
(define license 'LicenseRef-CreatorCxn-1.0)
(define build-deps '())

;; Register raco camp command
(define raco-commands '(("camp" camp/private/cli "Camp static site generator" #f)))
