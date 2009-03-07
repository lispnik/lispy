;;;; libgpg-error-parse.lisp

;;; Copyright (C) 2008 Matthew Kennedy <mkennedy@common-lisp.net>
;;;
;;; This file is NOT part of libgpg-error.
;;;
;;; libgpg-error is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1 of
;;; the License, or (at your option) any later version.
;;;
;;; libgpg-error is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with libgpg-error; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

(in-package #:gpg-error)

(defun parse-err-codes (&optional (pathname #.(merge-pathnames #p"err-codes.h.in.1.6" *compile-file-pathname*)))
  (with-open-file (stream pathname)
    (do ((line #1=(read-line stream nil nil) #1#)
	 err-codes)
	((null line) (reverse err-codes))
      (cl-ppcre:register-groups-bind
	  ((#'parse-integer code)
	   (#'(lambda (symbol) (intern (substitute #\- #\_ symbol)
				       (find-package "KEYWORD"))) symbol)
	   description)
	  ("^(\\d+)\\s+(GPG_\\w+)\\s+(\\w.*)\\s*$" line)
	(push (list code symbol description)
	      err-codes)))))

(defun parse-errnos (&optional (pathname #.(merge-pathnames #p"errnos.in.1.6" *compile-file-pathname*)))
  (with-open-file (stream pathname)
    (do ((line #1=(read-line stream nil nil) #1#)
	 errnos)
	((null line) (reverse errnos))
      (cl-ppcre:register-groups-bind
	  ((#'(lambda (code) `(logior +gpg-err-system-error+ ,(parse-integer code)))
	      code)
	   (#'(lambda (symbol) (intern (concatenate 'string "GPG-ERR-" symbol)
				       (find-package "KEYWORD")))
	      symbol))
	  ("^(\\d+)\\s+(E\\w+)\\s*$" line)
	(push (list code symbol)
	      errnos)))))

(defun parse-all () (append (parse-err-codes) (parse-errnos)))

(defmacro define-errors ()
  `(cffi:defcenum gpg-err-code-t
     ,@(mapcar #'(lambda (err)
		   (list (second err)
			 (eval (first err))))
	       `,(parse-all))))
