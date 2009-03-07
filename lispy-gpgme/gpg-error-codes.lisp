;;; Copyright (C) 2006 g10 Code GmbH
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

(in-package :gpg-error)

;;; The error code type gpg-err-code-t.

;;; This is used for system error codes.
(defconstant +gpg-err-system-error+ (ash 1 15))

;;; This is one more than the largest allowed entry.
(defconstant +gpg-err-code-dim+ 65536)

(define-errors)
