
(defpackage #:lispy-system
  (:use #:common-lisp
        #:asdf))

(in-package #:lispy-system)

(defsystem #:lispy
  :description "Common Lisp library management in Common Lisp"
  :author "Matthew Kennedy"
  :licence "GPL"
  :components ((:file "packages")
	       (:file "logging")
               (:file "utils")
               (:file "specials")
               (:file "lispy"))
  :serial t
  :depends-on (#:drakma
               #:puri
               #:gzip-stream
               #:archive
               #:ironclad
               #:cl-fad
 	       #:log5
	       #:trivial-shell))

(defmethod perform :after ((o load-op) (c (eql (find-system 'lispy))))
  (let ((lispy-config (merge-pathnames #p".lispy.lisp"(user-homedir-pathname))))
    (if (probe-file lispy-config)
        (load lispy-config)
        (warn "Lispy configuration not found at ~A" lispy-config)))
  (funcall (intern "INITIALIZE" (find-package "LISPY"))))
