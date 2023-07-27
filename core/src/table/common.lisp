;;; common.lisp --- Table Basics

;; Copyright (C) 2023 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of OTF.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:



;;; Code:

(in-package :net.didierverna.otf)
(in-readtable :net.didierverna.otf)

;; ==========================================================================
;; Table records
;; ==========================================================================

(defstruct table-record
  "The TABLE-RECORD structure."
  tag checksum offset length)

(defun read-table-record (&aux (record (make-table-record)))
  "Read a table record from *STREAM*."
  ;; #### TODO: see about convenient restarts here (discard-table, ...)
  (setf (table-record-tag record) (read-tag))
  (setf (table-record-checksum record) (read-u32))
  (setf (table-record-offset record) (read-u32))
  (setf (table-record-length record) (read-u32))
  record)



;; ==========================================================================
;; Tables
;; ==========================================================================

(define-condition unsupported-table (otf-usage-warning)
  ((name
    :documentation "The table's name."
    :initarg :name
    :reader name))
  (:report (lambda (unsupported-table stream)
	     (report stream "Table '~A' is unsupported."
		     (name unsupported-table))))
  (:documentation "The Unsupported Table usage warning.
It signals that an OTF table is unsupported."))

(defgeneric read-table (font name record)
  (:documentation "Read a new table into FONT.
NAME is the table's name as a Lisp keyword.
RECORD is the corresponding table record from the tables directory.")
  (:method (font name record)
    "Skip unsupported table. This is the default method."
    (warn 'unsupported-table :name (table-record-tag record))))

;;; common.lisp ends here
