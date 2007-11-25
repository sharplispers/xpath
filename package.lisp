(in-package :cl-user)

(defpackage :xpath
  (:use cl)
  (:export #:compile-xpath
	   #:parse-xpath

	   #:boolean-value
	   #:string-value

	   #:environment-find-namespace
	   #:environment-validate-variable

	   #:context-variable-value))
