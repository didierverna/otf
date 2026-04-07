;;; head.lisp --- Head Table

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

;; #### TODO: more checks need to be implemented, that extend beyond this
;; table only. See the bottom of this page:
;; https://learn.microsoft.com/en-us/typography/opentype/spec/head for
;; constraints on flags / lsb (left sidebearings) of glyphs in particular.


;;; Code:

(in-package :net.didierverna.otf)
(in-readtable :net.didierverna.otf)


(defclass head-table ()
  ((version
    :documentation
    "The header table's version as (majorVersion . minorVersion)."
    :initarg :version :reader table-version)
   (font-revision
    :documentation "The font's revision as (major . minor)."
    :initarg :font-revision :reader font-revision)
   (checksum-adjustment
    :initarg :checksum-adjustment :reader checksum-adjustment)
   (magic-number
    :initarg :magic-number :reader magic-number)
   (flags ;; #### TODO: provide flag bit accessors.
    :initarg :flags :reader flags)
   (units-per-em
    :initarg :units-per-em :reader units-per-em)
   (dates
    :documentation "The font's dates as (created . modified)."
    :initarg :dates :reader dates)
   (bounds
    :documentation "The font's bounds as (xMin yMin xMax yMax)."
    :initarg :bounds :reader bounds)
   ;; #### TODO: provide flag bit accessors, and check consistency with the
   ;; fsSelection bits in the OS/2 table.
   (mac-style
    :initarg :mac-style :reader mac-style)
   (lowest-rec-ppem
    :initarg :lowest-rec-ppem :reader lowest-rec-ppem)
   (font-direction-hint
    :initarg :font-direction-hint :reader font-direction-hint)
   (index-to-loc-format
    :initarg :index-to-loc-format :reader index-to-loc-format)
   (glyph-data-format
    :initarg :glyph-data-format :reader glyph-data-format))
  (:documentation "The header ('head') table class."))


(defmethod read-table ((tag (eql :|head|)) record font &aux head)
  "Read the header ('head') table from *STREAM* into FONT.
If the table version is not 1.0, signal an INVALID-VALUE error. This error is
immediately restartable with FIX or CONTINUE."
  (with-condition-context (otf table :tag tag)
    (setq head
	  (make-instance 'head-table
	    :version (cons (read-u16) (read-u16))
	    :font-revision (read-fixed)
	    :checksum-adjustment (read-u32)
	    :magic-number (read-u32)
	    :flags (read-u16)
	    :units-per-em (read-u16)
	    :dates (cons (read-s64) (read-s64))
	    :bounds (list (read-s16) (read-s16) (read-s16) (read-s16))
	    :mac-style (read-u16)
	    :lowest-rec-ppem (read-u16)
	    :font-direction-hint (read-s16)
	    :index-to-loc-format (read-s16)
	    :glyph-data-format (read-s16)))
    (unless (equal (table-version head) '(1 . 0))
      (restart-case (error 'invalid-value
		      :kind "table version"
		      :actual (table-version head)
		      :expected '(1 . 0))
	(continue () :report "Continue anyway.")
	(fix () :report "Set to 1.0."
	  (setf (slot-value head 'version) '(1 . 0)))))
    (unless (= (magic-number head) #x5F0F3CF5)
      (restart-case (error 'invalid-value
		      :kind "magic number"
		      :actual (magic-number head)
		      :expected #x5F0F3CF5)
	(fix () :report "Set to 0x5F0F3CF5."
	  (setf (slot-value head 'magic-number) #x5F0F3CF5))
	(continue () :report "Continue anyway.")))
    (unless (zerop (ldb (byte 5 6) (flags head)))
      (restart-case (error 'invalid-value
		      :kind "bits 6-10 in flags"
		      :actual (flags head)
		      :expected "cleared")
	(fix () :report "Clear bits 6-10."
	  (setf (ldb (byte 5 6) (flags head)) 0))
	(continue () :report "Continue anyway.")))
    (unless (zerop (ldb (byte 1 15) (flags head)))
      (restart-case (error 'invalid-value
		      :kind "bit 15 in flags"
		      :actual (flags head)
		      :expected "cleared")
	(fix () :report "Clear bit 15."
	  (setf (ldb (byte 1 15) (flags head)) 0))
	(continue () :report "Continue anyway.")))
    (unless (zerop (ldb (byte 9 7) (mac-style head)))
      (restart-case (error 'invalid-value
		      :kind "bits 7-15 in macStyle"
		      :actual (mac-style head)
		      :expected "cleared")
	(fix () :report "Clear bits 7-15."
	  (setf (ldb (byte 9 7) (mac-style head)) 0))
	(continue () :report "Continue anyway.")))
    (unless (= (font-direction-hint head) 2)
      (warn 'deprecated-value
	:kind "fontDirectionHint"
	:actual (font-direction-hint head)
	:expected 2))
    (unless (zerop (glyph-data-format head))
      (restart-case (error 'invalid-value
		      :kind "glyphDataFormat"
		      :actual (glyph-data-format head)
		      :expected 0)
	(fix () :report "Set to 0."
	  (setf (slot-value head 'glyph-data-format) 0))
	(continue () :report "Continue anyway."))))
  (setf (slot-value font '|head|) head)
  head)

;;; common.lisp ends here
