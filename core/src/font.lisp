;;; font.lisp --- Font Information

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
;; Base Font
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass font ()
  ((file
    :documentation "The file from which the font was loaded, or NIL."
    :initform nil
    :initarg :file
    :accessor file)
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
    :accessor range-shift))
  (:documentation "The Open Type Font class.
This class represents decoded font information. Within the context of this
library, the term \"font\" denotes an instance of this class, or of one of its
subclasses."))

(defun make-font (&rest initargs)
  "Make a new FONT instance, and return it.
If INITARGS are provided, pass them as-is to MAKE-INSTANCE."
  (apply #'make-instance 'font initargs))

;;; font.lisp ends here
