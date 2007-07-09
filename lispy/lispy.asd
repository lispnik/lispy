
(defpackage #:lispy-system
  (:use #:common-lisp
        #:asdf))

(defsystem #:lispy
  :components ((:file "packages")
               (:file "specials")
               (:file "utils" :depends-on ("packages" "specials"))
               (:file "lispy" :depends-on ("utils")))
  :depends-on (#:drakma
               #:puri
               #:gzip-stream
               #:archive
               #:ironclad
               #:cl-fad))