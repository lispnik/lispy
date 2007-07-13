
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

(defun log-message (name control-string &rest format-arguments)
  (when *lispy-log-stream*
    (format *lispy-log-stream* "~&~A ~A: ~A~%"
            (get-universal-time)
            (string-upcase name)
            (apply #'format nil (cons control-string format-arguments)))))

(defun hash-to-list (hash)
  (let ((result '()))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v result))
             hash)
    result))