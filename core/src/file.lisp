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
;; Header
;; ==========================================================================

(define-condition invalid-file-header (otf-compliance-error)
  ((header
    :documentation "The file header."
    :initarg :header
    :accessor header))
  (:report (lambda (invalid-file-header stream)
	     (report stream "0x~X is not a valid OTF file header.
Must be one of 0x00010000, 0x4f54544f ('OTTO'), or 0x74746366 ('ttcf')."
	       (header invalid-file-header))))
  (:documentation "The Invalid File Header compliance error.
It signals that a file header is not a valid OTF one."))



;; ==========================================================================
;; Entry Point
;; ==========================================================================

(defun load-font (file)
  "Load FILE into a new font, and return it.
If FILE's header is not recognized, signal an INVALID-FILE-HEADER error.

While loading OTF data, any signalled condition is restartable with
CANCEL-LOADING, in which case this function simply returns NIL."
  (with-open-file
      (*stream* file :direction :input :element-type '(unsigned-byte 8))
    (with-simple-restart (cancel-loading "Cancel loading this font.")
      (let ((header (read-u32)))
	(cond ((= header #x00010000)
	       (load-tt-font))
	      ((= header #x4f54544f) ;; OTTO
	       (load-cff-font))
	      ((= header #x74746366) ;; ttcf
	       (load-font-collection))
	      (t
	       (error 'invalid-file-header :header header)))))))

;;; file.lisp ends here
