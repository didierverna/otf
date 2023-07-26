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

(defun read-u16 ()
  "Read an unsigned 16 bits Big Endian integer from *STREAM*."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-byte *stream*)
	  (ldb (byte 8 0) u16) (read-byte *stream*))
    u16))


;; ------------
;; Other values
;; ------------

(define-condition invalid-tag (otf-compliance-error)
  ()
  (:documentation "The Invalid Tag compliance error.
It signal that an OTF tag is ill-formed."))

(define-condition invalid-tag-byte (invalid-tag)
  ((tag-byte
    :documentation "The invalid tag byte."
    :initarg :tag-byte
    :reader tag-byte)
   (byte-number
    :documentation "The byte number in the tag."
    :initarg :byte-number
    :reader byte-number))
  (:report (lambda (invalid-tag-byte stream)
	     (report stream "Invalid byte 0x~X at tag position ~A.
Should be within the range 0x20 - 0x7E."
		     (tag-byte invalid-tag-byte)
		     (byte-number invalid-tag-byte))))
  (:documentation "The Invalid Tag Byte compliance error.
It signals that an OTF tag byte is not within the range of printable ASCII
characters."))

(define-condition spurious-tag-byte (invalid-tag-byte)
  ()
  (:report (lambda (spurious-tag-byte stream)
	     (report stream "Spurious non-space byte 0x~X at tag position ~A.
Should be 0x20."
		     (tag-byte spurious-tag-byte)
		     (byte-number spurious-tag-byte))))
  (:documentation "The Spurious Tag Byte compliance error.
It signals that a non-space OTF tag byte follows a space one."))

(define-condition blank-tag (invalid-tag)
  ()
  (:report (lambda (blank-tag stream)
	     (declare (ignore blank-tag))
	     (report stream "Tag doesn't have any non-space characters.")))
  (:documentation "The Blank Tag compliance error.
It signals that a tag doesn't have any non-space characters."))

(defun read-tag ()
  "Read a tag from *STREAM*.
The tag is returned as a string of up to four characters, potential padding
spaces being discarded.

If one of the original tag bytes is not in the printable ASCII character
range, signal an INVALID-TAG-BYTE error.
If a non-space character follows a space character, signal a SPURIOUS-TAG-BYTE
error. This error is immediately restartable with DISCARD-TAG-BYTE.
Finally, if the tag contains only space characters, signal a BLANK-TAG error."
  (coerce
   (mapcar #'code-char
     (or (loop :with padding
	       :for i :from 0 :to 3
	       :for byte := (read-byte *stream*)
	       :unless (<= #x20 byte #x7e)
		 :do (error 'invalid-tag-byte :tag-byte byte :byte-number i)
	       :when (and padding (not (= byte #x20)))
		 :do (with-simple-restart
			 (discard-tag-byte
			  "Discard spurious tag byte (continue padding).")
		       (error 'spurious-tag-byte :tag-byte byte :byte-number i))
	       :if (= byte #x20)
		 :do (setq padding t)
	       :else :collect byte)
	 (error 'blank-tag)))
   'string))



;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defmacro define-constant (name value &optional documentation)
  "Like DEFCONSTANT, but reuse existing value if any."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))

;;; util.lisp ends here
