;;; file.lisp --- Parsing and decoding

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
;; Table Directory
;; ==========================================================================

(define-condition invalid-header (otf-compliance-error)
  ((header
    :documentation "The header."
    :initarg :header
    :accessor header))
  (:report (lambda (invalid-header stream)
	     (report stream "0x~X is not a valid OTF header.
Must be one of 0x00010000, 0x4f54544f ('OTTO'), or 0x74746366 ('ttcf')."
	       (header invalid-header))))
  (:documentation "The Invalid Header compliance error.
It signals that a data header is not a valid OTF one."))


(define-constant +file-extensions+
  '((#x00010000 "otf" "ttf")
    (#x4f54544f "otf")
    (#x74746366 "otc" "ttc"))
  "The list of OTF valid file extensions for each data header.")

(define-condition invalid-file-extension (otf-compliance-warning)
  ((header
    :documentation "The data header."
    :initarg :header
    :accessor header)
   (extension
    :documentation "The file extension."
    :initarg :extension
    :accessor extension))
  (:report
   (lambda (invalid-file-extension stream
	    &aux (extensions (cdr (find (header invalid-file-extension)
				      +file-extensions+
				    :key #'car))))
     (report stream "'~A' is not a valid extension for this file.
Should be~:[~; one of~]~{ '~A'~}."
	     (extension invalid-file-extension)
	     (cdr extensions)
	     extensions)))
  (:documentation "The Invalid File Extension compliance warning.
It signals that an OTF file's extension is not compliant with its alleged
data."))



;; ==========================================================================
;; Font Files
;; ==========================================================================

(define-condition invalid-value (otf-compliance-error)
  ((kind
    :documentation "The kind of invalid value."
    :initarg :kind
    :reader kind)
   (provided
    :documentation "The provided invalid value."
    :initarg :provided
    :reader provided)
   (inferred
    :documentation "The inferred correct value."
    :initarg :inferred
    :reader inferred))
  (:report (lambda (invalid-value stream)
	     (report stream "Invalid '~A' value: ~S. Should be ~S."
		     (kind invalid-value)
		     (provided invalid-value)
		     (inferred invalid-value))))
  (:documentation "The Invalid Value compliance error.
It signals that a provided value in OTF data is invalid."))

(define-condition invalid-table-records-order (otf-compliance-error)
  ()
  (:report (lambda (invalid-table-records-order stream)
	     (declare (ignore invalid-table-records-order))
	     (report stream "Invalid table records order.
The records should be sorted by ascending tag order.")))
  (:documentation "The Invalid Table Records Order compliance error.
It signals that the entries in the records in the table records array are not
ordered by ascending tag."))

(defun load-font-data
    (header
     &aux (font (make-font (ecase header
			     (#x00010000 :true-type)
			     (#x4f54544f :compact-font-format))
			   :file (when (typep *stream* 'file-stream)
				   (pathname *stream*)))))
  "Load OTF font data from *STREAM*.

If any of the searchRange, entrySelector, or rangeShift values are invalid,
signal an INVALID-VALUE error. This error is immediately restartable with
FIX.
If the table records are not properly ordered, signal an
INVALID-TABLE-RECORDS-ORDER error. This error is continuable."
  (setf (tables-number font) (read-u16))
  (setf (search-range font) (read-u16))
  (let ((inferred (* 16 (expt 2 (floor (log (tables-number font) 2))))))
    (unless (= (search-range font) inferred)
      (restart-case (error 'invalid-value
			   :kind "searchRange"
			   :provided (search-range font)
			   :inferred inferred)
	(fix () :report "Update to the correct value."
	  (setf (search-range font) inferred)))))
  (setf (entry-selector font) (read-u16))
  (let ((inferred (floor (log (tables-number font) 2))))
    (unless (= (entry-selector font) inferred)
      (restart-case (error 'invalid-value
			   :kind "entrySelector"
			   :provided (entry-selector font)
			   :inferred inferred)
	(fix () :report "Update to the correct value."
	  (setf (entry-selector font) inferred)))))
  (setf (range-shift font) (read-u16))
  (let ((inferred (- (* (tables-number font) 16) (search-range font))))
    (unless (= (range-shift font) inferred)
      (restart-case (error 'invalid-value
			   :kind "rangeShift"
			   :provided (range-shift font)
			   :inferred inferred)
	(fix () :report "Update to the correct value."
	  (setf (range-shift font) inferred)))))
  (let ((records (list)))
    (dotimes (i (tables-number font)) (push (read-table-record) records))
    (let ((sorted-records (sort records #'string> :key #'table-record-tag)))
      (unless (equal sorted-records records)
	(cerror "Continue anyway." 'invalid-table-records-order)))
    ;; #### TODO: check for unicity of the standardized tables.
    ;; #### TODO: check for existence of required tables.
    (setq records (sort records #'< :key #'table-record-offset))
    (dolist (record records)
      (read-table font (intern (table-record-tag record) :keyword) record)))
  font)



;; ==========================================================================
;; Entry Point
;; ==========================================================================

(defun load-data (&aux (from-file-p (typep *stream* 'file-stream))
		       (extension (when from-file-p
				    (pathname-type (pathname *stream*)))))
  "Load OTF data from *STREAM*.

If the data header is not recognized, signal an INVALID-HEADER error.
If the data comes from a file with a wrong extension,
signal an INVALID-FILE-EXTENSION warning.

While loading OTF data, any signalled condition is restartable with
CANCEL-LOADING, in which case this function simply returns NIL."
  (with-simple-restart (cancel-loading "Cancel loading.")
    (let* ((header (read-u32))
	   (extensions (cdr (find header +file-extensions+ :key #'car))))
      (unless extensions (error 'invalid-header :header header))
      (unless (and from-file-p (member extension extensions :test #'string=))
	(warn 'invalid-file-extension :header header :extension extension))
      (if (= header #x74746366) ;; ttcf
	(load-font-collection-data)
	(load-font-data header)))))

(defun load-file (file)
  "Load OTF FILE."
  (with-open-file
      (*stream* file :direction :input :element-type '(unsigned-byte 8))
    (load-data)))

;;; file.lisp ends here
