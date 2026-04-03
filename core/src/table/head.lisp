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
   (flags
    :initarg :flags :reader flags)
   (units-per-em
    :initarg :units-per-em :reader units-per-em)
   (dates
    :documentation "The font's dates as (created . modified)."
    :initarg :dates :reader dates)
   (bounds
    :documentation "The font's bounds as (xMin yMin xMax yMax)."
    :initarg :bounds :reader bounds)
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


(defclass header-table-context (context)
  ()
  (:documentation "The Header Table Context class."))

(defmethod context-string ((context header-table-context))
  "Return the Header Table CONTEXT string."
  "while reading the header ('head') table")


(defmethod read-table ((name (eql :|head|)) record font &aux head)
  "Read the header ('head') table from *STREAM* into FONT."
  (with-condition-context (otf header-table-context)
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
	    :glyph-data-format (read-s16))))
  (setf (slot-value font '|head|) head)
  head)

;;; common.lisp ends here
