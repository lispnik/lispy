
(in-package #:lispy)

(log5:defcategory install)
(log5:defcategory uninstall)
(log5:defcategory upgrade)
(log5:defcategory extract)
(log5:defcategory map)
(log5:defcategory installation)
(log5:defcategory asdf)
(log5:defcategory fetch)

(log5:defcategory all-categories (install uninstall upgrade extract map installation asdf fetch))

(log5:defoutput newline (format nil "~%"))

(log5:defoutput time-hms
    (multiple-value-bind (second minute hour day month year)
	(decode-universal-time (get-universal-time))
      (format nil "~D:~2,'0D:~2,'0D" hour minute second)))

(log5:start-sender 'debug
	      (log5:stream-sender :location *error-output*)
	      :category-spec '(all-categories log5:error+)
	      :output-spec '(time-hms log5:message newline))
