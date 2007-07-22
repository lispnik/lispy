(in-package #:lispy)

(defvar *lispy-map-urls*
  (list (puri:parse-uri "http://common-lisp.net/project/lispy/repository/map.lisp-expr")))

(defvar *lispy-pathname*
  (let ((path (make-pathname :name nil :type nil :version nil :defaults (parse-namestring *load-truename*))))
    (make-pathname :directory (butlast (pathname-directory path)) :defaults path)))

(defvar *lispy-installation-pathname* (merge-pathnames #p"installation.lisp-expr" *lispy-pathname*))

(defvar *lispy-asdf-config-pathname* (merge-pathnames #p"asdf-config.lisp" *lispy-pathname*))

(defvar *lispy-distfiles-pathname* (merge-pathnames #p"distfiles/" *lispy-pathname*))

(defvar *lispy-log-stream* t)

