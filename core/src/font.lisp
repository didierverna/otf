;;; font.lisp --- Font Information

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
;; Base Font
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass font ()
  ((file
    :documentation "The font's file."
    :initarg :file
    :reader file)
   (name
    :documentation "The font's name."
    :initform nil
    :initarg :name
    :reader name)
   ;; #### TODO: see later if this shouldn't be made into 2 different font
   ;; classes. This will also condition the existence of a constructor
   ;; function.
   (outline-type
    :documentation "The font's outline type.
Possible values are :tt (True Type) or :cff (Compact Font Format)."
    :initarg :outline-type
    :reader outline-type)
   (tables-number
    :documentation "This font's number of tables."
    :accessor tables-number)
   (search-range
    :documentation "This font's search range."
    :accessor search-range)
   (entry-selector
    :documentation "This font's entry selector."
    :accessor entry-selector)
   (range-shift
    :documentation "This font's range shift."
    :accessor range-shift)
   (|head|
    :documentation "This font's header table."
    :accessor |head|))
  (:documentation "The Open Type Font class.
This class represents decoded font information. Within the context of this
library, the term \"font\" denotes an instance of this class, or of one of its
subclasses."))


(defmethod print-object ((font font) stream)
  "Print FONT unreadably with its name to STREAM."
  (print-unreadable-object (font stream :type t)
    (format stream "~A (~A)" (name font) (outline-type font))))


;; #### NOTE: we're not currently so pedantic as to check that the font's file
;; has a non-empty base name.
(defmethod initialize-instance :after ((font font) &key)
  "Handle FONT's name initialization.
Unless a custom name has been provided already, initialize FONT's name to the
font file's base name."
  (with-slots (file name) font
    ;; #### NOTE: the validity of a custom name has already been checked by
    ;; LOAD-FONT.
    (unless name (setq name (pathname-name file)))))

;;; font.lisp ends here
