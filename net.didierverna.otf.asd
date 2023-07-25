;;; net.didierverna.otf.asd --- ASDF system definition

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

(defsystem :net.didierverna.otf
  :long-name "Open Type Font"
  :description "A Common Lisp interface to the Open Type Font format"
  :long-description "\
OTF (for Open Type Font) is a cross-platform font file format developed
jointly by Microsoft and Adobe. The OTF library parses and decodes OTF files
into an abstract data structure, providing easy access to the corresponding
font information in Common Lisp."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#otf"
  :source-control "https://github.com/didierverna/otf"
  :license "BSD"
  :version (:read-file-line #p"make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :depends-on (:net.didierverna.otf.core))

;;; net.didierverna.otf.asd ends here
