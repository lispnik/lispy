
(in-package #:lispy)

(defun compare-to-md5sum (pathname md5sum)
  (string= (ironclad:byte-array-to-hex-string (ironclad:digest-file :md5 pathname))
           md5sum))

(defun extract-entry (entry target-directory-pathname)
  (let ((input (archive:entry-stream entry)))
    (unwind-protect
         (let ((pathname (merge-pathnames (parse-namestring (archive:name entry)) target-directory-pathname)))
           (ensure-directories-exist pathname)
           (with-open-file (output pathname
                                   :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
             (copy-stream input output)))
      (close input))))

(defun extract-archive (archive-pathname target-directory-pathname)
  (gzip-stream:with-open-gzip-file (stream archive-pathname)
    (let ((archive (archive:open-archive 'archive:tar-archive stream)))
      (unwind-protect
           (archive:do-archive-entries (entry archive)
	     (log5:log-for extract "Extracting ~A" (archive:name entry))
             (when (archive:entry-regular-file-p entry)
               (extract-entry entry target-directory-pathname)))
        (close stream)))))


(defun copy-stream (input output &optional (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
  (do ((count #1=(read-sequence buffer input) #1#))
      ((zerop count))
    (write-sequence buffer output :start 0 :end count)))

(defun uri-to-string (uri)
  (with-output-to-string (stream)
    (puri:render-uri uri stream)))

(defun hash-to-list (hash)
  (let ((result '()))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v result))
             hash)
    result))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun read-stream (stream &rest args)
  (let ((*read-eval* nil))
    (apply #'read stream args)))

(defun download-file (url destination-pathname)
  (ensure-directories-exist destination-pathname)
  (multiple-value-bind (stream status-code headers uri http-stream must-close)
      (drakma:http-request url :want-stream t)
    (declare (ignore status-code headers uri http-stream must-close))
    (unwind-protect
	 (with-open-file (output-stream destination-pathname
					:direction :output
					:element-type '(unsigned-byte 8)
					:if-exists :supersede)
	   (copy-stream stream output-stream))
      (close stream))))

(defun verify-signature (text-pathname signature-pathname)
  (multiple-value-bind (output error status)
       (trivial-shell:shell-command (format nil "gpg --verify ~A ~A" signature-pathname text-pathname))
    (declare (ignore output))
    (values (zerop status)
	    error)))
