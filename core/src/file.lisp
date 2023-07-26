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
;; Header Processing
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
;; Entry Point
;; ==========================================================================

(defun load-tt-data
  (&key (file (when (typep *stream* 'file-stream) (pathname *stream*)))
   &aux (font (make-font :file file)))
  "Load OTF TrueType outlines data from *STREAM* into a new font and return it."
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
      (cond ((= header #x00010000)
	     (load-tt-data))
	    ((= header #x4f54544f) ;; OTTO
	     (load-cff-data))
	    ((= header #x74746366) ;; ttcf
	     (load-collection-data))))))

(defun load-file (file)
  "Load OTF FILE."
  (with-open-file
      (*stream* file :direction :input :element-type '(unsigned-byte 8))
    (load-data)))

;;; file.lisp ends here
