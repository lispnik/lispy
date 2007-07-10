
(defpackage #:lispy-system
  (:use #:common-lisp
        #:asdf))

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