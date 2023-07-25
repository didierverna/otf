;;; package.lisp --- OTF package definition

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

(in-package :cl-user)


(defpackage :net.didierverna.otf
  (:documentation "The Open Type Font.")
  (:use :cl :net.didierverna.otf.setup)
  (:export

    ;; From the :net.didierverna.otf.setup package:
    :*copyright-years*
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version

    ;; From package.lisp (this file):
    :nickname-package

    ;; From src/util.lisp:
    :otf :otf-warning :otf-error :otf-compliance
    :otf-compliance-warning :otf-compliance-error

    ;; From src/file.lisp:
    :load-font :cancel-loading))


(in-package :net.didierverna.otf)

(defun nickname-package (&optional (nickname :otf))
  "Add NICKNAME (:OTF by default) to the :NET.DIDIERVERNA.OTF package."
  (rename-package :net.didierverna.otf
		  (package-name :net.didierverna.otf)
		  (adjoin nickname (package-nicknames :net.didierverna.otf)
			  :test #'string-equal)))

;;; package.lisp ends here
