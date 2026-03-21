;;; package.lisp --- OTF package definition

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

(in-package :cl-user)


(defpackage :net.didierverna.otf
  (:documentation "The Open Type Font package.")
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
    :context :context-string
    :otf :context :otf-warning :otf-error
    :otf-compliance :otf-compliance-warning :otf-compliance-error
    :otf-usage :otf-usage-warning :otf-usage-error
    :invalid-tag
    :invalid-tag-byte :tag-byte :byte-number
    :spurious-tag-byte :tag-byte :byte-number
    :blank-tag
    :discard-tag-byte

    ;; From src/font.lisp:
    :font :file :name :outline-type
    :tables-number :search-range :entry-selector :range-shift
    :head

    ;; From src/table/common.lisp:
    :unsupported-table :name
    :spurious-table-byte :table-name :spurious-byte-position :table-position
    :ignore-spurious-byte

    ;; From src/file.lisp:
    :invalid-value :kind :provided :inferred
    :invalid-table-records-order
    :fix
    :invalid-custom-name :name :use-base-file-name
    :invalid-header :header
    :invalid-file-extension :header :extension
    :unsupported-format :ftm :file
    :cancel-loading
    :load-font))


(in-package :net.didierverna.otf)

(defun nickname-package (&optional (nickname :otf))
  "Add NICKNAME (:OTF by default) to the :NET.DIDIERVERNA.OTF package."
  (rename-package :net.didierverna.otf
		  (package-name :net.didierverna.otf)
		  (adjoin nickname (package-nicknames :net.didierverna.otf)
			  :test #'string-equal)))

;;; package.lisp ends here
