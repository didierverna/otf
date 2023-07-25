# OTF
OTF (for Open Type Font) is a cross-platform font file format developed
jointly by Microsoft and Adobe. The OTF library parses and decodes OTF files
into an abstract data structure, providing easy access to the corresponding
font information in Common Lisp.

## Quick Start
In your favorite Lisp REPL, type something like this:
```
(asdf:load-system :net.didierverna.otf)
(net.didierverna.otf:nickname-package)
(defvar *lmroman10-regular* (otf:load-font #p"/path/to/lmroman10-regular.otf"))
```
You will end up with a `font` object, containing the decoded font information,
and stored in an easily accessible way. Inspect the object in question to
familiarize yourself with its contents.

## More information
OTF comes with both a
[user manual](https://www.lrde.epita.fr/~didier/software/lisp/otf/user/)
and a
[reference manual](https://www.lrde.epita.fr/~didier/software/lisp/otf/reference/).
Please see the projet's
[homepage](https://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#otf)
for more information.
