;;; file.lisp --- Parsing and decoding

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
;; Stream Loading
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
  (:documentation "The Invalid Value compliance error.
It signals that a provided value in OTF data is invalid."))

(define-condition-report (condition invalid-value)
    "invalid '~A' value: ~S. Should be ~S"
  (kind condition)
  (provided condition)
  (inferred condition))


(define-condition invalid-table-records-order (otf-compliance-error)
  ()
  (:documentation "The Invalid Table Records Order compliance error.
It signals that the entries in the table records array are not ordered by
ascending tag."))

(define-condition-report (condition invalid-table-records-order)
    "invalid table records order.
The records should be sorted by ascending tag order")


(defun load-stream (font)
  "Parse *STREAM* into FONT, and return it.

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
  (let (records)
    (dotimes (i (tables-number font)) (push (read-table-record) records))
    (let ((sorted-records (sort records #'string> :key #'table-record-tag)))
      (unless (equal sorted-records records)
	(cerror "Continue anyway." 'invalid-table-records-order)))
    ;; #### TODO: check for unicity of the standardized tables.
    ;; #### TODO: check for existence of required tables.
    (setq records (sort records #'< :key #'table-record-offset))
    (dolist (record records)
      (read-table (intern (table-record-tag record) :keyword) record font)))
  font)




;; ==========================================================================
;; Entry Point
;; ==========================================================================

(define-condition invalid-custom-name (otf-usage-error)
  ((name
    :documentation "The invalid custom name."
    :initarg :name
    :reader name))
  (:documentation "The Invalid Custom Name usage error.
It signals that a custom name is not a non-empty string."))

(define-condition-report (condition invalid-custom-name)
    "custom name ~S is invalid (should be a non-empty string)"
  (name condition))


(define-condition invalid-header (otf-compliance-error)
  ((header
    :documentation "The header."
    :initarg :header
    :accessor header))
  (:documentation "The Invalid Header compliance error.
It signals that a header is not a valid OTF one."))

(define-condition-report (condition invalid-header)
    "0x~X is not a valid OTF header.
Must be one of 0x00010000 (TT), 0x4f54544f (CFF), or 0x74746366 (TTCF)"
  (header condition))


(define-constant +file-extensions+
  '((#x00010000 "otf" "OTF" "ttf" "TTF")
    (#x4f54544f "otf" "OTF")
    (#x74746366 "otc" "OTC" "ttc" "TTC"))
  "The list of OTF valid file extensions for each header.")

(define-condition invalid-file-extension (otf-compliance-warning)
  ((header
    :documentation "The data header."
    :initarg :header
    :accessor header)
   (extension
    :documentation "The file extension."
    :initarg :extension
    :accessor extension))
  (:documentation "The Invalid File Extension compliance warning.
It signals that an OTF file's extension is not compliant with its alleged
data."))

;; #### FIXME: DEFINE-CONDITION-REPORT should be extended to enable local
;; variables.
(define-condition-report (condition invalid-file-extension)
    "'~A' is not a valid extension for this file.
Should be~:[~; one of~]~{ '~A'~}"
  (extension condition)
  (cddr (find (header condition) +file-extensions+ :key #'car))
  (cdr (find (header condition) +file-extensions+ :key #'car)))


(define-condition unsupported-format (otf-warning)
  ((fmt
    :documentation "The unsupported format.
Possible values include :TT, :CFF, and :TTCF."
    :initarg :fmt
    :reader fmt)
   ;; #### NOTE: this slot is here for completeness, but since context-aware
   ;; condition reporting already mentions the file we're reading from, it's
   ;; not used in the condition reporting code below.
   (file
    :documentation "The unsupported font's file name."
    :initarg :file
    :reader file))
  (:documentation "The Unsupported Format warning.
It signals that a file contains data in a currently unsupported format."))

(define-condition-report (condition unsupported-format)
    "~A is not supported yet"
  (ecase (fmt condition)
    (:tt "True Type")
    (:cff "Compact Font Format")
    (:ttcf "Font Collection")))


(defun load-font (file &rest keys &key name &aux font)
  "Load FILE into a new font, and return it.
- FILE must be a pathname designator.
- If NAME is not NIL, use it as the font's name instead of FILE's base name,
  in which case it must be a non-empty string. Otherwise, signal an
  INVALID-CUSTOM-NAME error. This error is immediately restartable with
  USE-FILE-BASE-NAME.

If FILE's header is not recognized, signal an INVALID-HEADER error.

If FILE's extension is incorrect, signal an INVALID-FILE-EXTENSION warning.

If FILE contains a font collection, signal an UNSUPPORTED-FORMAT warning, and
return NIL.

Any condition signalled while FILE is being loaded is restartable with
CANCEL-LOADING, in which case this function simply returns NIL."
  (when name
    (unless (and (stringp name) (not (zerop (length name))))
      (restart-case (error 'invalid-custom-name :name name)
	(use-file-base-name () :report "Use the font file's base name."
	  (setq keys (remove-keys keys :name))))))
  (with-open-file
      (*stream* file :direction :input :element-type '(unsigned-byte 8))
    (with-simple-restart (cancel-loading "Cancel loading.")
      (let* ((header (read-u32))
	     (extensions (cdr (find header +file-extensions+ :key #'car)))
	     (extension (pathname-type file)))
	(unless extensions (error 'invalid-header :header header))
	(unless (member extension extensions :test #'string=)
	  (warn 'invalid-file-extension :header header :extension extension))
	(if (= header #x74746366)
	  (warn 'unsupported-format :fmt :ttcf :file file)
	  (setq font (load-stream
		      (apply #'make-instance 'font
			     :file file
			     :outline-type (ecase header
					     (#x00010000 :tt)
					     (#x4f54544f :cff))
			     keys)))))))
  font)

;;; file.lisp ends here
