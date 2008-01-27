(in-package #:lispy)

(define-constant +lispy-default-map-url+
    (puri:parse-uri "http://common-lisp.net/project/lispy/repository/map.lisp-expr")
  "The URL to the official Lispy map.")

(defvar *lispy-map-urls* (list +lispy-default-map-url+)
  "A list of map URLs Lispy should read and merge.")

(defvar *lispy-pathname*
  (let ((path (make-pathname :name nil :type nil :version nil :defaults (parse-namestring *load-truename*))))
    (make-pathname :directory (butlast (pathname-directory path)) :defaults path))
  "Where Lispy is installed.  You should never need to modify this.")

(defvar *lispy-installation-pathname* (merge-pathnames #p"installation.lisp-expr" *lispy-pathname*)
  "The path to the installation list file.")

(defvar *lispy-asdf-config-pathname* (merge-pathnames #p"asdf-config.lisp" *lispy-pathname*)
  "The path to the Lispy ASDF configuration file.")

(defvar *lispy-distfiles-pathname* (merge-pathnames #p"distfiles/" *lispy-pathname*)
  "The path where source packages should be downloaded to.")

(defvar *lispy-log-stream* t
  "A stream Lispy should use to write log messages.")

