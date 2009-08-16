
(defpackage #:lispy
  (:use #:common-lisp)
  (:export #:*lispy-map-urls*
           #:*lispy-pathname*
           #:*lispy-installation-pathname*
           #:*lispy-asdf-config-pathname*
           #:*lispy-distfiles-pathname*
           #:*lispy-log-stream*
	   #:*lisp-offline*
           #:+lispy-default-map-url+
           #:initialize
           #:install
           #:uninstall
           #:module-by-name
           #:install-by-name
           #:list-map
           #:list-installation
	   #:module
           #:name
           #:homepage
           #:description
           #:versions
	   #:version
           #:latest-version
           #:our-version
           #:version
           #:source
           #:md5sum
           #:root
           #:asdf-paths
           #:dependencies
           #:read-asdf-config
           #:write-asdf-config
           #:read-installation
           #:write-installation
           #:read-map
           #:upgradable-p
           #:list-upgrades
           #:upgrade
           #:upgrade-all
           #:search-map)
  (:documentation "Lispy is Common Lisp package management in Common Lisp."))
