;;; util.lisp --- General Utilities

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
;; Error Ontology
;; ==========================================================================

(define-condition otf ()
  ()
  (:documentation "The OTF root condition."))


(define-condition otf-warning (otf warning)
  ()
  (:documentation "The OTF warnings root condition."))

(define-condition otf-error (otf error)
  ()
  (:documentation "The OTF errors root condition."))


(define-condition otf-compliance (otf)
  ()
  (:documentation "The OTF Compliance root condition.
This is the mixin for conditions related to OTF compliance."))

(define-condition otf-compliance-warning (otf-warning otf-compliance)
  ()
  (:documentation "The OTF compliance warnings root condition.
This is the root condition for warnings related to OTF compliance."))

(define-condition otf-compliance-error (otf-error otf-compliance)
  ()
  (:documentation "The OTF compliance errors root condition.
This is the root condition for errors related to OTF compliance."))



;; ==========================================================================
;; Stream Reading
;; ==========================================================================

(defvar *stream* nil "The stream being read.")

#i(report 2)
(defun report (stream format-string &rest format-arguments)
  "Like FORMAT, but if *STREAM* is bound, report that we're reading from it."
  (if *stream*
    (format stream "While reading ~A, "
      (or (when (typep *stream* 'file-stream) (pathname *stream*))
	  *stream*))
    (when (alpha-char-p (aref format-string 0))
      (setf (aref format-string 0) (char-upcase (aref format-string 0)))))
  (apply #'format stream format-string format-arguments))


;; ----------------
;; Numerical values
;; ----------------

(defun read-u32 ()
  "Read an unsigned 32 bits Big Endian integer from *STREAM*."
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-byte *stream*)
	  (ldb (byte 8 16) u32) (read-byte *stream*)
	  (ldb (byte 8  8) u32) (read-byte *stream*)
	  (ldb (byte 8  0) u32) (read-byte *stream*))
    u32))



;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defmacro define-constant (name value &optional documentation)
  "Like DEFCONSTANT, but reuse existing value if any."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))

;;; util.lisp ends here
