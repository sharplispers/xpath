;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:xpath.system
  (:use #:cl #:asdf))

(in-package :xpath.system)

(defsystem #:xpath
    :name "cl-xpath"
    :author "Ivan Shvedunov"
    :version "0.1"
    :description "cl-xpath"
    :depends-on (:cxml :parse-number :cl-ppcre :yacc)
    :components ((:module xpath
                          :pathname ""
                          :components
                          ((:file "package")
                           (:file "utils" :depends-on ("package"))
                           (:file "test" :depends-on ("utils"))
                           (:file "pipes" :depends-on ("utils"))
                           (:file "regex" :depends-on ("utils" "test"))
                           (:file "xml-utils" :depends-on ("regex" "utils"))
                           (:file "xnum" :depends-on ("utils" "test"))
                           (:file "types" :depends-on ("utils" "xnum" "pipes" "xml-utils"))
                           (:file "axes" :depends-on ("utils" "xnum" "pipes" "xml-utils"))
                           (:file "node-tests" :depends-on ("utils" "xnum" "pipes" "xml-utils"))
                           (:file "xpath" :depends-on ("axes" "node-tests"))
                           (:file "functions" :depends-on ("xpath"))
                           (:file "xpath-test" :depends-on ("xpath" "functions"))
			   (:file "lexer" :depends-on ("package"))))))
