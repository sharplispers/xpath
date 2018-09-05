;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defsystem "xpath"
  :name "plexippus-xpath"
  :description "An implementation of the XML Path Language (XPath) Version 1.0"
  :license "2 clause BSD" ; TODO maybe X11-style? check homepage(s)
  :author ("Ivan Shvedunov"
           "David Lichteblau")
  :maintainer "Sharp Lispers <sharplispers@googlegroups.com>"

  :version "0.1"
  :depends-on ("cxml" "parse-number" "cl-ppcre" "yacc")

  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "pipes")
               (:file "protocol")
               #+sbcl (:file "xnum-ieee")
               #-sbcl (:file "xnum")
               (:file "types")
               (:file "extensions")
               (:file "environment")
               (:file "axes")
               (:file "node-tests")
               (:file "xpath")
               (:file "functions")
               (:file "lexer")
               (:file "parser")
               (:file "api")
               (:file "plx")
               (:file "xmls-compat")
               (:file "patterns")
               (:file "profile"))

  :in-order-to ((test-op (test-op "xpath/test"))))

(defsystem "xpath/test"
  :depends-on ("xpath")
  :serial t
  :components ((:file "test")
               #+sbcl (:file "xnum-ieee-test")
               #-sbcl (:file "xnum-test")
               (:file "parser-test")
               (:file "xpath-test"))
  :perform (test-op (o c)
                    (uiop:symbol-call :xpath-test '#:run-all-tests)))
