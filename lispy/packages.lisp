
(defpackage #:lispy
  (:use #:common-lisp)
  (:export #:*lispy-map-urls*
           #:*lispy-pathname*
           #:*lispy-installation-pathname*
           #:*lispy-asdf-config-pathname*
           #:*lispy-distfiles-pathname*
           #:*lispy-log-stream*
           #:initialize
           #:install
           #:uninstall
           #:module-by-name
           #:install-by-name
           #:list-map
           #:list-installation
           #:name
           #:homepage
           #:description
           #:versions
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
           #:upgrade-all))
