
(in-package #:lispy)

(defclass module ()
  ((name :initarg :name :reader name)
   (homepage :initarg :homepage :reader homepage)
   (description :initarg :description :reader description)
   (versions :initarg :versions :reader versions)))

(defgeneric latest-version (module))

(defmethod latest-version ((module module))
  (let* ((versions (versions module))
         (latest-version (first versions))
         (latest-our-version (our-version latest-version)))
    (dolist (version (rest versions))
      (when (> (our-version version) latest-our-version)
        (setf latest-version version
              latest-our-version (our-version version))))
    latest-version))

(defgeneric dependency-list (module))

(defmethod dependency-list ((module module))
  (let ((dependencies '()))
    (labels ((dependencies-of (name)
               (push (module-by-name name) dependencies)
               (let ((m (module-by-name name)))
                 (if m
                     (dolist (d (dependencies (latest-version m)))
                       (dependencies-of d))
                     (error "No such module ~A found in map." name)))))
      (dependencies-of (name module)))
    dependencies))

(defclass version ()
  ((name :initarg :name :reader name)
   (our-version :initarg :our-version :reader our-version)
   (version :initarg :version :reader version)
   (source :initarg :source :reader source)
   (md5sum :initarg :md5sum :reader md5sum)
   (root :initarg :root :reader root)
   (asdf-paths :initarg :asdf-paths :reader asdf-paths)
   (dependencies :initarg :dependencies :reader dependencies)))

(defmethod print-object ((m module) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (princ (name m) stream)))

(defmethod print-object ((v version) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~A ~A" (name v) (version v))))

(defvar *lispy-map* (make-hash-table :test 'eq))

(defun module-by-name (name)
  (gethash name *lispy-map*))

(defun read-map (&optional (map-url *lispy-map-url*))
  (log-message "read-map" "Reading ~A" (uri-to-string *lispy-map-url*))
  (multiple-value-bind (stream status-code headers uri http-stream must-close)
      (drakma:http-request map-url :want-stream t)
    (declare (ignore status-code headers uri http-stream must-close))
    (unwind-protect
         (dolist (module (mapcar #'parse-module (read stream)))
           (setf (gethash (name module) *lispy-map*)
                 module))
      (close stream)))
  (log-message "read-map" "Map contains ~A entr~:@p" (hash-table-count *lispy-map*))
  *lispy-map*)

(defun parse-module (module)
  (destructuring-bind (&key name homepage description versions)
      module
    (make-instance 'module
                   :name name
                   :homepage homepage
                   :description description
                   :versions (mapcar #'(lambda (v)
                                         (parse-version name v))
                                     versions))))

(defun parse-version (name version)
  (destructuring-bind (&key our-version version source md5sum root asdf-paths dependencies)
      version
    (make-instance 'version
                   :name name
                   :our-version our-version
                   :version version
                   :source source
                   :md5sum md5sum
                   :root root
                   :asdf-paths (or asdf-paths (list root))
                   :dependencies dependencies)))

(defclass install ()
  ((name :initarg :name :reader name)
   (our-version :initarg :our-version :reader our-version)
   (version :initarg :version :reader version)
   (root :initarg :root :reader root)
   (asdf-paths :initarg :asdf-paths :reader asdf-paths)))

(defmethod print-object ((i install) stream)
  (print-unreadable-object (i stream :type t :identity t)
    (princ (root i) stream)))

(defvar *lispy-installation* (make-hash-table :test 'eq))

(defun read-installation ()
  (log-message "read-installation" "Reading ~A" (namestring *lispy-installation-pathname*))
  (with-open-file (stream *lispy-installation-pathname*
                          :direction :input
                          :if-does-not-exist :create)
    (dolist (install (mapcar #'parse-install (read stream nil nil)))
      (setf (gethash (name install) *lispy-installation*)
            install)))
  (log-message "read-installation" "Map contains ~A entr~:@p" (hash-table-count *lispy-installation*))
  *lispy-installation*)

(defun write-installation ()
  (log-message "write-installation" "Writing ~A" (namestring *lispy-installation-pathname*))
  (with-open-file (stream *lispy-installation-pathname*
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (print (let ((installation '()))
             (maphash #'(lambda (name install)
                          (push `(:name ,name
                                        :our-version ,(our-version install)
                                        :version ,(version install)
                                        :root ,(root install)
                                        :asdf-paths ,(asdf-paths install))
                                installation))
                      *lispy-installation*)
             installation)
           stream)))

(defun parse-install (install)
  (destructuring-bind (&key name our-version version root asdf-paths)
      install
    (make-instance 'install
                   :name name
                   :our-version our-version
                   :version version
                   :root root
                   :asdf-paths asdf-paths)))

(defun read-asdf-config ()
  (log-message "read-asdf-config" "Loading ~A" (namestring *lispy-asdf-config-pathname*))
  (load *lispy-asdf-config-pathname*))

(defun write-asdf-config ()
  (log-message "write-asdf-config" "Writing ~A" (namestring *lispy-asdf-config-pathname*))
  (with-open-file (stream *lispy-asdf-config-pathname*
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((paths '()))
      (maphash #'(lambda (name install)
                   (declare (ignore name))
                   (if (null (asdf-paths install))
                       (push (root install)
                             paths)
                       (dolist (path (asdf-paths install))
                         (push path
                               paths))))
               *lispy-installation*)
      #+nil (print `(let ((root (make-pathname :directory (pathname-directory *load-truename*))))
		      (dolist (path ',paths)
			(pushnew (merge-pathnames path root) asdf:*central-registry* :test 'equal)))
		   stream)
      (format stream
"(let ((root (make-pathname :directory (pathname-directory *load-truename*))))
   (dolist (path '(~{~S~}))
     (pushnew (merge-pathnames path root)
              asdf:*central-registry* 
              :test 'equal)))"
              paths))))

(defgeneric fetch (module))

(defmethod fetch ((module module))
  (log-message "fetch" "Fetching ~A" (name module))
  (fetch (latest-version module)))

(defmethod fetch ((version version))
  (log-message "fetch" "Fetching ~A" (uri-to-string (make-fetch-url (source version))))
  (ensure-directories-exist *lispy-distfiles-pathname*)
  (let ((pathname (merge-pathnames (source version) *lispy-distfiles-pathname*)))
    (if (and (probe-file pathname )
             (compare-to-md5sum pathname (md5sum version)))
        (log-message "fetch" "~A already exists and matches the version ~A MD5 checksum."
                     pathname
                     (version version))
        (progn
          (multiple-value-bind (stream status-code headers uri http-stream must-close)
              (drakma:http-request (make-fetch-url (source version))
                                   :force-binary t
                                   :want-stream t)
            (declare (ignore status-code headers uri http-stream must-close))
            (unwind-protect
                 (with-open-file (output pathname
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :element-type (stream-element-type stream))
                   (copy-stream stream output))
              (close stream)))
          (unless (compare-to-md5sum pathname (md5sum version))
            (error "MD5 checksum for ~S failed" (source version)))))))

(defun make-fetch-url (source)
  (let ((parsed-path (append (butlast (puri:uri-parsed-path *lispy-map-url*))
                             (list "distfiles" source))))
    (let ((result (puri:copy-uri *lispy-map-url*)))
      (setf (puri:uri-parsed-path result)
            parsed-path)
      result)))

(defgeneric install (module))

(defmethod install ((module module))
  (log-message "install" "Installing ~A" (name module))
  (dolist (module (remove-duplicates (dependency-list module)))
    (install (latest-version module))))

(defmethod install ((version version))
  (if (and (install-by-name (name version))
           (= (our-version (install-by-name (name version)))
              (our-version version)))
      (log-message "install" "Already installed ~A ~A." (name version) (version version))
      (progn
        (log-message "install" "Installing ~A ~A" (name version) (version version))
        (fetch version)
        (extract version)
        (setf (gethash (name version) *lispy-installation*)
              (make-instance 'install
                             :name (name version)
                             :our-version (our-version version)
                             :version (version version)
                             :root (root version)
                             :asdf-paths (asdf-paths version)))
        (write-installation)
        (write-asdf-config)
        (read-asdf-config))))

(defgeneric extract (module))

(defmethod extract ((module module))
  (log-message "extract" "Extracting ~A" (name module))
  (extract (latest-version module)))

(defmethod extract ((version version))
  (log-message "extract" "Extracting ~A ~A" (name version) (version version))
  (let ((pathname (merge-pathnames (source version) *lispy-distfiles-pathname*)))
    (extract-archive pathname *lispy-pathname*)))

(defun install-by-name (name)
  (gethash name *lispy-installation*))

(defgeneric uninstall (install))

(defmethod uninstall ((install install))
  (cl-fad:delete-directory-and-files (merge-pathnames (root install) *lispy-pathname*) :if-does-not-exist :ignore)
  (remhash (name install) *lispy-installation*)
  (write-installation))

(defun list-map () (hash-to-list *lispy-map*))
(defun list-installation () (hash-to-list *lispy-installation*))

(defun initialize ()
  (setf *lispy-installation* (make-hash-table :test 'eq)
        *lispy-map* (make-hash-table :test 'eq))
  (log-message "initialize" "Initializing Lispy system on ~A ~A" (lisp-implementation-type) (lisp-implementation-version))
  (read-map)
  (read-installation)
  (write-asdf-config)
  (read-asdf-config)
  (values))

(defgeneric upgradable-p (install module))

(defmethod upgradable-p ((install install) (module module))
  (let ((latest-version (latest-version module)))
    (> (our-version latest-version)
       (our-version install))))

;; (initialize)
;; (install (module-by-name 'drakma))
;; (install (module-by-name 'cl-plus))
;; (uninstall (install-by-name 'chunga))



;;; Lispy bootstrap code (remove installation.lisp-expr, distfiles and
;;; all source directories)
#+nil
(dolist (name '(drakma puri gzip-stream archive ironclad cl-fad asdf lispy))
  (install (module-by-name name)))
#+nil
(initialize)
