(in-package #:lispy)

(defvar *lispy-map-url* (puri:parse-uri "http://localhost/~mkennedy/lispy/map.lisp-expr"))
(defvar *lispy-pathname* (merge-pathnames "lispy/" (user-homedir-pathname)))
(defvar *lispy-installation-pathname* (merge-pathnames #p"installation.lisp-expr" *lispy-pathname*))
(defvar *lispy-asdf-config-pathname* (merge-pathnames #p"asdf-config.lisp" *lispy-pathname*))
(defvar *lispy-distfiles-pathname* (merge-pathnames #p"distfiles/" *lispy-pathname*))
(defvar *lispy-log-stream* t)

