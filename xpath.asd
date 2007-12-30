;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:xpath.system
  (:use #:cl #:asdf))

(in-package :xpath.system)

(defsystem #:xpath
    :name "plexippus-xpath"
    :author "Ivan Shvedunov, David Lichteblau"
    :version "0.1"
    :depends-on (:cxml :parse-number :cl-ppcre :yacc)
    :components ((:module xpath
                          :pathname ""
                          :components
                          ((:file "package")
                           (:file "utils" :depends-on ("package"))
                           (:file "protocol" :depends-on ("package" "pipes"))
                           (:file "test" :depends-on ("utils"))
                           (:file "pipes" :depends-on ("utils"))
                           (:file "regex" :depends-on ("utils" "test"))
                           (:file "xml-utils" :depends-on ("regex" "utils" "pipes"))
                           (:file "xnum" :depends-on ("utils" "test"))
                           (:file "types" :depends-on ("utils" "xnum" "pipes" "xml-utils"))
                           (:file "axes" :depends-on ("utils" "xnum" "pipes" "xml-utils"))
                           (:file "node-tests" :depends-on ("utils" "xnum" "pipes" "xml-utils"))
                           (:file "xpath" :depends-on ("axes" "node-tests"))
                           (:file "functions" :depends-on ("xpath"))
                           (:file "lexer" :depends-on ("package"))
                           (:file "parser" :depends-on ("lexer"))
                           (:file "api" :depends-on ("xpath" "parser"))
			   (:file "xpath-test" :depends-on ("xpath" "functions" "types" "api"))))))
