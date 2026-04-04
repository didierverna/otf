;;; util.lisp --- General Utilities

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


(defvar *stream* nil "The stream being read.")


;; ==========================================================================
;; Context-Aware Condition Reporting and Handling
;; ==========================================================================

(defclass context ()
  ()
  (:documentation "The CONTEXT class.
This is the base class for classes representing contexts in which
conditions are signalled."))

(defgeneric context-string (context)
  (:documentation "Return CONTEXT'string."))

(defun context-format
    (stream context format-string &rest arguments &aux (upcase t))
  "Like FORMAT, but *STREAM* and CONTEXT-aware.
- When *STREAM*, report that we're reading from it to STREAM.
- When CONTEXT, report the context string to STREAM.
- Finally, print FORMAT-STRING with ARGUMENTS to STREAM."
  (when *stream*
    (format stream "While reading ~A,~%"
      (if (typep *stream* 'file-stream) (pathname *stream*) *stream*))
    (setq upcase nil))
  (when context
    (format stream "~:[~A~;~@(~A~)~],~%" upcase (context-string context))
    (setq upcase nil))
  (apply #'format stream "~:[~@?~;~@(~@?~)~]." upcase format-string arguments))

#i(define-condition-report 2)
(defmacro define-condition-report
    ((condition type) format-string &rest arguments)
  "Define a context-aware report function for a CONDITION of TYPE.
The reporting is ultimately done by calling FORMAT on FORMAT-STRING with
ARGUMENTS."
  (let ((the-stream (gensym "stream")))
    `(defmethod print-object ((,condition ,type) ,the-stream)
       (if *print-escape*
	 (call-next-method)
	 (context-format ,the-stream (context ,condition)
	   ,format-string ,@arguments)))))

(defmacro with-condition-context
    ((condition-type context-type &rest initargs) &body body)
  "Execute BODY within a particular condition signalling context.
While BODY is executing, conditions of type CONDITION-TYPE (not evaluated) are
caught and augmented with an instance of CONTEXT-TYPE (not evaluated)
initialized with INITARGS."
  `(handler-bind ((,condition-type
		    (lambda (condition)
		      (setf (slot-value condition 'context)
			    (make-instance ',context-type ,@initargs)))))
     ,@body))




;; ==========================================================================
;; Error Ontology
;; ==========================================================================

(define-condition otf ()
  ((context
    :documentation "The context in which the condition was signalled."
    :initform nil
    :reader context))
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


(define-condition invalid-value (otf-compliance-error)
  ((kind
    :documentation "The kind of invalid value."
    :initarg :kind
    :reader kind)
   (actual
    :documentation "The actual value."
    :initarg :actual
    :reader actual)
   (expected
    :documentation "The expected value."
    :initarg :expected
    :reader expected))
  (:documentation "The Invalid Value compliance error.
It signals that a provided value in OTF data is invalid."))

(define-condition-report (condition invalid-value)
    "invalid ~A value: ~A. Should be ~A"
  (kind condition)
  (actual condition)
  (expected condition))


(define-condition otf-usage (otf)
  ()
  (:documentation "The OTF Usage root condition.
This is the mixin for conditions related to the use of the library."))

(define-condition otf-usage-warning (otf-warning otf-usage)
  ()
  (:documentation "The OTF usage warnings root condition.
This is the root condition for warnings related to the use of the library."))

(define-condition otf-usage-error (otf-error otf-usage)
  ()
  (:documentation "The OTF usage errors root condition.
This is the root condition for errors related to the use of the library."))




;; ==========================================================================
;; Numbers
;; ==========================================================================

(defun read-u16 ()
  "Read an unsigned 16 bits Big Endian integer from *STREAM*."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-byte *stream*)
	  (ldb (byte 8 0) u16) (read-byte *stream*))
    u16))

(defun read-s16 (&aux (u16 (read-u16)))
  "Read an signed 16 bits Big Endian integer from *STREAM*."
  (logior u16 (- (mask-field (byte 1 15) u16))))


;; #### FIXME: I'm not sure yet how to actually read this (cons, 32bits value,
;; etc.).
(defun read-fixed ()
  "Read a fixed (32bits signed fixed point number 16.16) from *STREAM*.
Return a cons of the two 16 bits components."
  (cons (read-s16) (read-u16)))


(defun read-u32 ()
  "Read an unsigned 32 bits Big Endian integer from *STREAM*."
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-byte *stream*)
	  (ldb (byte 8 16) u32) (read-byte *stream*)
	  (ldb (byte 8  8) u32) (read-byte *stream*)
	  (ldb (byte 8  0) u32) (read-byte *stream*))
    u32))


(defun read-u64 ()
  "Read an unsigned 32 bits Big Endian integer from *STREAM*."
  (let ((u64 0))
    (setf (ldb (byte 8 56) u64) (read-byte *stream*)
	  (ldb (byte 8 48) u64) (read-byte *stream*)
	  (ldb (byte 8 40) u64) (read-byte *stream*)
	  (ldb (byte 8 32) u64) (read-byte *stream*)
	  (ldb (byte 8 24) u64) (read-byte *stream*)
	  (ldb (byte 8 16) u64) (read-byte *stream*)
	  (ldb (byte 8  8) u64) (read-byte *stream*)
	  (ldb (byte 8  0) u64) (read-byte *stream*))
    u64))

(defun read-s64 (&aux (u64 (read-u64)))
  "Read an signed 64 bits Big Endian integer from *STREAM*."
  (logior u64 (- (mask-field (byte 1 63) u64))))




;; ==========================================================================
;; Tags
;; ==========================================================================

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
  (:documentation "The Invalid Tag Byte compliance error.
It signals that an OTF tag byte is not within the range of printable ASCII
characters."))

(define-condition-report (condition invalid-tag-byte)
    "invalid byte 0x~X at tag position ~A.
Should be within the range 0x20 - 0x7E"
  (tag-byte condition)
  (byte-number condition))


(define-condition spurious-tag-byte (invalid-tag-byte)
  ()
  (:documentation "The Spurious Tag Byte compliance error.
It signals that a non-space OTF tag byte follows a space one."))

(define-condition-report (condition spurious-tag-byte)
    "spurious non-space byte 0x~X at tag position ~A.
Should be 0x20"
  (tag-byte condition)
  (byte-number condition))


(define-condition blank-tag (invalid-tag)
  ()
  (:documentation "The Blank Tag compliance error.
It signals that a tag doesn't have any non-space characters."))

(define-condition-report (condition blank-tag)
    "tag doesn't have any non-space characters")


(defun read-tag (&aux bytes)
  "Read a tag from *STREAM*.
The tag is returned as a case-sensitive keyword with trailing spaces removed.
- If one of the original tag bytes is not in the printable ASCII character
  range, signal an INVALID-TAG-BYTE error. This error is immediately
  restartable with KEEP-TAG-BYTE.
- If a non-space character follows a space character, signal a
  SPURIOUS-TAG-BYTE error. This error is immediately restartable with
  DISCARD-TAG-BYTE.
- If the tag contains only space characters, signal a BLANK-TAG error. This
  error is immediately restartable with USE-GENSYMED-TAG."
  (let (byte padding)
    (dotimes (i 4)
      (setq byte (read-byte *stream*))
      (cond ((= byte #x20)
	     (unless padding (setq padding t)))
	    (padding
	     (with-simple-restart
		 (discard-tag-byte
		  "Discard spurious tag byte (continue padding).")
	       (error 'spurious-tag-byte :tag-byte byte :byte-number i)))
	    (t
	     (if (and (< #x20 byte) (<= byte #x7e))
	       (push byte bytes)
	       (restart-case
		   (error 'invalid-tag-byte :tag-byte byte :byte-number i)
		 (keep-tag-byte ()
		   :report "Keep tag byte anyway."
		   (push byte bytes))))))))
  (if (null bytes)
    (restart-case (error 'blank-tag)
      (use-gensymed-tag ()
	:report "Use a gensym'ed tag."
	(gensym "TAG")))
    (intern (coerce (mapcar #'code-char (nreverse bytes)) 'string) :keyword)))




;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defmacro define-constant (name value &optional documentation)
  "Like DEFCONSTANT, but reuse existing value if any."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))

#i(remove-keys 1)
(defun remove-keys (keys &rest removed)
  "Return a new property list from KEYS without REMOVED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:unless (member key removed)
	  :nconc (list key val)))

;;; util.lisp ends here
