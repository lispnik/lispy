
(defpackage #:lispy
  (:use #:common-lisp)
  (:export #:*lispy-map-url*
           #:*lispy-pathname*
           #:*lispy-installation-pathname*
           #:*lispy-asdf-config-pathname*
           #:*lispy-distfiles-pathname*
           #:*lispy-log-stream*
           #:initialize
           #:install
           #:uninstall
           #:module-by-name
           #:install-by-name))
