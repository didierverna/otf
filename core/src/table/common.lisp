;;; common.lisp --- Table Basics

;; Copyright (C) 2023, 2026 Didier Verna

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

(defclass table-record ()
  ((tag
    :documentation "The table's tag.
It is normally a keyword, but may also be an uninterned symbol (see
`read-table-record')."
    :initarg :tag :reader tag)
   (checksum
    :documentation "The table's checksum."
    :initarg :checksum :reader checksum)
   (offset
    :documentation "The table's offset."
    :initarg :offset :reader offset)
   (length
    :documentation "The table's length."
    :initarg :length :reader table-length))
  (:documentation "The TABLE-RECORD class."))

(defun read-table-record ()
  "Read a table record from *STREAM*."
  (make-instance 'table-record
    :tag (read-tag) :checksum (read-u32) :offset (read-u32):length (read-u32)))




;; ==========================================================================
;; Table Commonalities
;; ==========================================================================

(defclass table-context (context)
  ((tag :documentation "The table's tag." :initarg :tag :reader tag))
  (:documentation "The Table Context class."))

(defmethod context-string ((context table-context))
  "Return the Header Table CONTEXT string."
  (format nil "while reading the '~A' table" (tag context)))




;; ==========================================================================
;; Table Reading
;; ==========================================================================

(define-condition unsupported-table (otf-usage-warning)
  ((tag
    :documentation "The table's tag."
    :initarg :tag :reader tag))
  (:documentation "The Unsupported Table usage warning.
It signals that an OTF table is unsupported."))

(define-condition-report (condition unsupported-table)
    "table '~A' is not supported yet" (tag condition))


(define-condition spurious-table-byte (otf-compliance-error)
  ((tag
    :documentation "The next table's tag."
    :initarg :tag :reader tag)
   (spurious-byte-position
    :documentation "The position of the spurious byte."
    :initarg :spurious-byte-position :reader spurious-byte-position)
   (start
    :documentation "The next table's start position."
    :initarg :start :reader start))
  (:documentation "The Spurious Table Byte compliance error.
It signals that a non-zero byte was encountered in a table padding."))

(define-condition-report (condition spurious-table-byte)
    "spurious non-zero byte in table padding, ~
     at position ~A, before the start of table '~A' at position ~A"
  (spurious-byte-position condition)
  (tag condition)
  (start condition))


(defgeneric read-table (tag record font)
  (:documentation "Read a new table from *STREAM* into FONT.
TAG is the table's tag as a Lisp keyword.
RECORD is the corresponding table record from the tables directory.
The file position in *STREAM* must be before, or exactly at the beginning of
the table to be read, and strictly after the contents of the previous table,
if any.")
  (:method :before (tag record font)
    "Skip potential padding before the beginning of the table.
If a non-zero byte is found while skipping, signal a SPURIOUS-TABLE-BYTE
error. This error is immediately restartable with IGNORE-SPURIOUS-TABLE-BYTE."
    (assert (<= (file-position *stream*) (offset record)))
    (dotimes (i (- (offset record) (file-position *stream*)))
      (unless (= (read-byte *stream*) #x00)
	(with-simple-restart
	    (ignore-spurious-table-byte "Ignore spurious table byte.")
	  (error 'spurious-table-byte
	    :tag (tag record)
	    :spurious-byte-position (1- (file-position *stream*))
	    :start (offset record))))))
  (:method (tag record font)
    "Signal an UNSUPPORTED-TABLE warning, and skip table.
This is the default method."
    (warn 'unsupported-table :tag (tag record))
    (dotimes (i (table-length record)) (read-byte *stream*))))

;;; common.lisp ends here
