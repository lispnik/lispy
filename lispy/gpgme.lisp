
(in-package #:lispy)

;;; FIXME: Even though gpgme-data-t is an alias for :string, the
;;; specializer does not seem to dispatch on it.

(defmethod gpgme::translate-to-foreign (value (type (eql 'gpgme::gpgme-data-t)))
  (cond
    (value value)
    (t (cffi:null-pointer))))

(defun verify-signature (signature-pathname plain-pathname)
  (with-open-file (plain plain-pathname)
    (with-open-file (signature signature-pathname)
      (gpgme:with-context (ctx)
	(gpgme:op-verify ctx signature plain :detached t)))))
