
(in-package #:lispy)

(defun log-message (name control-string &rest format-arguments)
  (when *lispy-log-stream*
    (format *lispy-log-stream* "~&~A ~A: ~A~%"
            (get-universal-time)
            (string-upcase name)
            (apply #'format nil (cons control-string format-arguments)))))

(defclass module ()
  ((name :initarg :name
	 :reader name
	 :documentation "The name of the Lisp package.")
   (homepage :initarg :homepage
	     :reader homepage
	     :documentation "From where the Lisp package originated.")
   (description :initarg :description
		:reader description
		:documentation "A brief description of the Lisp package to include in rendered views of maps.")
   (versions :initarg :versions
	     :reader versions
	     :documentation "A list of versions of the Lisp package.")
   (map-url :initarg :map-url
	    :reader map-url
	    :documentation "The URL to the map the MODULE was defined in."))
  (:documentation "A MODULE represents a Lisp package from a Lispy map."))

(defgeneric latest-version (module)
  (:documentation "Determine the latest version of the Lisp package described by MODULE.  Returns an instance of VERSION."))

(defmethod latest-version ((module module))
  (let* ((versions (versions module))
         (latest-version (first versions))
         (latest-our-version (our-version latest-version)))
    (dolist (version (rest versions))
      (when (> (our-version version) latest-our-version)
        (setf latest-version version
              latest-our-version (our-version version))))
    latest-version))

(defgeneric dependency-list (module)
  (:documentation "Compute the dependencies of the latest version of a Lispy package described by MODULE.  Returns a list of MODULE instances."))

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
  ((name :initarg :name
	 :reader name
	 :documentation "The name of the module to which a VERSION belongs.")
   (map-url :initarg :map-url
	    :reader map-url
	    :documentation "The URL to the map the VERSION was defined in.")
   (our-version :initarg :our-version
		:reader our-version
		:documentation "A synthetic version number which is used in VERSION comparisons.")
   (version :initarg :version
	    :reader version
	    :documentation "The upstream author's version number or a date of the form YYYY-MM-DD if no version is available or the version is from version control.")
   (source :initarg :source
	   :reader source
	   :documentation "The Lisp package's source file name.")
   (md5sum :initarg :md5sum
	   :reader md5sum
	   :documentation "The MD5 checksum of the Lisp package's source archive.")
   (root :initarg :root
	 :reader root
	 :documentation "The directory where the Lisp package's source archive unpacks to.")
   (asdf-paths :initarg :asdf-paths
	       :reader asdf-paths
	       :documentation "A list of ASDF paths to be added to the ASDF configuration for Lispy packages which include more than one ASDF file in more than one location.  If not specified, then ROOT is used.")
   (dependencies :initarg :dependencies
		 :reader dependencies
		 :documentation "A list of MODULE names which the VERSION depends on."))
  (:documentation "A VERSION represents a specific version of a MODULE."))

(defmethod print-object ((m module) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (princ (name m) stream)))

(defmethod print-object ((v version) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~A ~A" (name v) (version v))))

(defvar *lispy-map* (make-hash-table :test 'eq))

(defun module-by-name (name)
  "Returns the instance of MODULE described by NAME."
  (gethash name *lispy-map*))

(defun read-map (map-url)
  "Read the map at MAP-URL and merge the modules into *LISPY-MAP*."
  (log-message "read-map" "Reading ~A" (uri-to-string map-url))
  (multiple-value-bind (stream status-code headers uri http-stream must-close)
      (drakma:http-request map-url :want-stream t)
    (declare (ignore status-code headers uri http-stream must-close))
    (unwind-protect
         (dolist (module (mapcar #'(lambda (m)
                                     (parse-module m map-url))
                                 (read-stream stream)))
           (setf (gethash (name module) *lispy-map*)
                 module))
      (close stream))))

(defun read-maps (&optional (map-urls *lispy-map-urls*))
  "Read all maps in the list MAP-URLS, merging each map into *LISPY-MAPS*.
Returns the mutated *LISPY-MAPS*."
  (dolist (map-url map-urls)
    (read-map map-url))
  (log-message "read-maps" "Map contains ~A entr~:@p"
               (hash-table-count *lispy-map*))
  *lispy-map*)

(defun parse-module (module map-url)
  (destructuring-bind (&key name homepage description versions)
      module
    (make-instance 'module
                   :name name
                   :homepage homepage
                   :description description
                   :versions (mapcar #'(lambda (v)
                                         (parse-version name v map-url))
                                     versions)
                   :map-url map-url)))

(defun parse-version (name version map-url)
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
                   :dependencies dependencies
                   :map-url map-url)))

(defclass install ()
  ((name :initarg :name
	 :reader name
	 :documentation "The MODULE name the INSTALL references.")
   (our-version :initarg :our-version
		:reader our-version
		:documentation "The synthetic version number.")
   (version :initarg :version
	    :reader version
	    :documentation "The upstream author's version number.")
   (root :initarg :root
	 :reader root
	 :documentation "The directory the install is located in.")
   (asdf-paths :initarg :asdf-paths
	       :reader asdf-paths
	       :documentation "The list of ASDF paths the installation references."))
  (:documentation "An INSTALL represents a Lisp package that has been installed by Lispy."))

(defmethod print-object ((i install) stream)
  (print-unreadable-object (i stream :type t :identity t)
    (princ (root i) stream)))

(defvar *lispy-installation* (make-hash-table :test 'eq))

(defun read-installation ()
  "Read the installation file into *LISPY-INSTALLATION*"
  (log-message "read-installation" "Reading ~A" (namestring *lispy-installation-pathname*))
  (with-open-file (stream *lispy-installation-pathname*
                          :direction :input
                          :if-does-not-exist :create)
    (dolist (install (mapcar #'parse-install (read-stream stream nil nil)))
      (setf (gethash (name install) *lispy-installation*)
            install)))
  (log-message "read-installation" "Map contains ~A entr~:@p" (hash-table-count *lispy-installation*))
  *lispy-installation*)

(defun write-installation ()
  "Write *LISPY-INSTALLATION* to the installtaion file."
  (log-message "write-installation" "Writing ~A" (namestring *lispy-installation-pathname*))
  (with-open-file (stream *lispy-installation-pathname*
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((*print-readably* t))
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
             stream))))

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
  "Load the Lispy ASDF configuration, which in turn adds paths to
ASDF:*CENTRAL-REGISTRY*."
  (log-message "read-asdf-config" "Loading ~A" (namestring *lispy-asdf-config-pathname*))
  (load *lispy-asdf-config-pathname*))

(defun write-asdf-config ()
  "Write a Lispy ASDF configuration file which can be loaded
indepedent of Lispy."
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
      (let ((*print-readably* t))
        (print `(let ((cl-user::root (make-pathname :directory (pathname-directory *load-truename*))))
                  (dolist (cl-user::path ',paths)
                    (pushnew (merge-pathnames cl-user::path cl-user::root) asdf:*central-registry* :test 'equal)))
               stream)))))

(defgeneric fetch (module)
  (:documentation "Download Lisp package source."))

(defmethod fetch ((module module))
  (log-message "fetch" "Fetching ~A" (name module))
  (fetch (latest-version module)))

(defmethod fetch ((version version))
  (log-message "fetch" "Fetching ~A" (uri-to-string (make-fetch-url (source version) (map-url version))))
  (ensure-directories-exist *lispy-distfiles-pathname*)
  (let ((pathname (merge-pathnames (source version) *lispy-distfiles-pathname*)))
    (if (and (probe-file pathname )
             (compare-to-md5sum pathname (md5sum version)))
        (log-message "fetch" "~A already exists and matches the version ~A MD5 checksum."
                     pathname
                     (version version))
        (progn
          (multiple-value-bind (stream status-code headers uri http-stream must-close)
              (drakma:http-request (make-fetch-url (source version) (map-url version))
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

(defun make-fetch-url (source map-url)
  (let ((parsed-path (append (butlast (puri:uri-parsed-path map-url))
                             (list "distfiles" source))))
    (let ((result (puri:copy-uri map-url)))
      (setf (puri:uri-parsed-path result)
            parsed-path)
      result)))

(defgeneric install (module)
  (:documentation "Install Lisp package source."))

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

(defgeneric extract (module)
  (:documentation "Extract Lisp package source."))

(defmethod extract ((module module))
  (log-message "extract" "Extracting ~A" (name module))
  (extract (latest-version module)))

(defmethod extract ((version version))
  (log-message "extract" "Extracting ~A ~A" (name version) (version version))
  (let ((pathname (merge-pathnames (source version) *lispy-distfiles-pathname*)))
    (extract-archive pathname *lispy-pathname*)))

(defun install-by-name (name)
  "Return the instance of INSTALL given by NAME."
  (gethash name *lispy-installation*))

(defgeneric uninstall (install)
  (:documentation "Uninstall Lisp package source."))

(defmethod uninstall ((install install))
  (cl-fad:delete-directory-and-files (merge-pathnames (root install) *lispy-pathname*) :if-does-not-exist :ignore)
  (remhash (name install) *lispy-installation*)
  (write-installation)
  (values))

(defun list-map () 
  "Return the merged list of modules."
  (hash-to-list *lispy-map*))

(defun list-installation ()
  "Return the list of installed modules versions."
  (hash-to-list *lispy-installation*))

(defun initialize ()
  "Initialize Lispy.  This reads and merges all maps in
*LISPY-MAP-URLS*, reads the installation, writes a fresh ASDF
configuration file and updates ASDF:*CENTRAL-REGISTRY*."
  (setf *lispy-installation* (make-hash-table :test 'eq)
        *lispy-map* (make-hash-table :test 'eq))
  (log-message "initialize" "Initializing Lispy system on ~A ~A" (lisp-implementation-type) (lisp-implementation-version))
  (read-maps)
  (read-installation)
  (write-asdf-config)
  (read-asdf-config)
  (values))

(defgeneric upgradable-p (install module)
  (:documentation "Non-NIL if the Lisp package source can be upgraded."))

(defmethod upgradable-p ((install install) (module module))
  (let ((latest-version (latest-version module)))
    (> (our-version latest-version)
       (our-version install))))

(defun list-upgrades ()
  "Returns a list of VERSION instances for all modules that can be upgraded."
  (let ((result '()))
    (dolist (i (list-installation))
      (let ((module (module-by-name (name i))))
        (when (and module
                   (upgradable-p i module))
          (push (list i (latest-version module)) result))))
    result))

(defgeneric upgrade (install)
  (:documentation "Upgrade a Lisp source package installation."))

(defmethod upgrade ((install install))
  (let ((module (module-by-name (name install))))
    (if (upgradable-p install module)
        (let ((latest-version (latest-version module)))
          (log-message "upgrade" "Upgrading ~A from ~A to ~A"
                       (name install)
                       (version install)
                       (version latest-version))
          ;; FIXME: this needs to be transactional
	  (uninstall install)
	  (install latest-version))
        (log-message "upgrade" "~A ~A is already the latest version."
                     (name install)
                     (version install)))))

(defun upgrade-all ()
  "Upgrade all upgradable Lisp source packages."
  (dolist (upgrade (list-upgrades))
    (destructuring-bind (install version)
        upgrade
      (declare (ignore version))
      (upgrade install))))

;;; Lispy bootstrap code (remove installation.lisp-expr, distfiles and
;;; all source directories)
#+nil
(dolist (name '(:drakma :puri :gzip-stream :archive :ironclad :cl-fad :asdf :lispy :cl+ssl :flexi-streams :trivial-gray-streams :chunga :salza :cffi :split-sequence :cl-base64))
  (install (module-by-name name)))
