
(in-package #:lispy)

#+(and lispy-gnupg (not sbcl))
(defun verify-signature (signature-pathname plain-pathname)
  (multiple-value-bind (output error status)
      (trivial-shell:shell-command (format nil "gpg --verify ~A ~A" signature-pathname plain-pathname))
    (declare (ignore output))
    (values (zerop status)
	    error)))

#+(and lispy-gnupg sbcl)
(defun verify-signature (signature-pathname plain-pathname)
  (let ((process (sb-ext:run-program "gpg" `("--verify" ,(namestring signature-pathname) ,(namestring plain-pathname))
				     :wait t
				     :error :stream
				     :search t)))

    (unwind-protect
	 (let ((status (zerop (sb-ext:process-exit-code process)))
	       (error-text (with-output-to-string (output)
			     (with-open-stream (stream (sb-ext:process-error process))
			       (do ((line (read-line stream nil nil) (read-line stream nil nil)))
				   ((null line))
				 (write-line line output))))))
	   (write-string error-text *standard-output*)
	   (values status error-text))
      (sb-ext:process-close process))))

#+lispy-gnupg
(defun verify-map (map-signature map map-signature-url)
  (multiple-value-bind (success error)
      (verify-signature map-signature map)
    (if success
	(log5:log-for verify "GPG validation success ~A" (uri-to-string map-signature-url))
	(error "GPG verification of map ~A with signature ~A failed: ~S"
	       map
	       map-signature
	       error)))
  (values))

;;; FIXME: Even though gpgme-data-t is an alias for :string, the
;;; specializer does not seem to dispatch on it.

#+lispy-gpgme
(defmethod gpgme::translate-to-foreign (value (type (eql 'gpgme::gpgme-data-t)))
  (cond
    (value value)
    (t (cffi:null-pointer))))

#+lispy-gpgme
(defun verify-signature (signature-pathname plain-pathname)
  (with-open-file (plain plain-pathname)
    (with-open-file (signature signature-pathname)
      (gpgme:with-context (ctx)
	(gpgme:op-verify ctx signature plain :detached t)))))

#+lispy-gpgme
(defun verify-map (map-signature map map-signature-url)
  (let ((result (verify-signature map-signature map)))
    (dolist (signature (getf (cadr result) :signatures))
      (if (member :green (getf signature :summary))
	  (log5:log-for verify "GPG validation success ~A" (uri-to-string map-signature-url))
	  (error "GPG verification of map ~A with signature ~A failed: ~S"
		 map
		 map-signature
		 signature))))
  (values))

#+(or lispy-insecure (not (or lispy-gnupg lispy-gpgme)))
(defun verify-map (map-signature map map-signature-url)
  (log5:log-for verify "WARNING: GPG verification of map ~A with signature ~A has will be bypassed." map map-signature)
  (values))
