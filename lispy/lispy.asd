
(defpackage #:lispy-system
  (:use #:common-lisp
        #:asdf))

(in-package #:lispy-system)

(defsystem #:lispy
  :components ((:file "packages")
               (:file "specials" :depends-on ("packages"))
               (:file "utils" :depends-on ("specials"))
               (:file "lispy" :depends-on ("utils")))
  :depends-on (#:drakma
               #:puri
               #:gzip-stream
               #:archive
               #:ironclad
               #:cl-fad))

(defmethod perform :after ((o load-op) (c (eql (find-system 'lispy))))
  (funcall (intern "INITIALIZE" (find-package "LISPY"))))
