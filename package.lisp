;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :xpath-protocol
  (:use)
  (:export #:child-pipe
	   #:attribute-pipe
	   #:namespace-pipe

           #:node-p
	   #:parent-node
	   #:local-name
	   #:qualified-name
	   #:namespace-prefix
	   #:namespace-uri
	   #:string-value
	   #:processing-instruction-target

	   #:node-type-p

	   #:base-uri

	   #:get-element-by-id)
  (:documentation
   "XPATH-PROTOCOL package contains generic functions that are used to support
    multiple representations of XML documents for XPath."))

(defpackage :xpath
  (:use cl)
  (:export #:compile-xpath
	   #:parse-xpath

	   #:boolean-value
	   #:string-value
           #:number-value

	   #:with-namespaces
	   #:with-variables
	   #:evaluate
	   #:evaluate-compiled
	   #:xpath
	   #:first-node
	   #:all-nodes
           #:map-node-set
	   #:map-node-set->list
           #:do-node-set
           #:make-node-set-iterator
           #:node-set-iterator-end-p
           #:node-set-iterator-next
           #:node-set-iterator-current
           #:node-set-p
           #:node-set-empty-p
	   #:node-set

	   #:context
	   #:make-context
	   #:context-node

	   #:environment-find-namespace
	   #:environment-find-variable
	   #:environment-find-function

	   #:find-xpath-function
	   #:define-extension
	   #:define-xpath-function/lazy
	   #:define-xpath-function/eager
	   #:define-xpath-function/single-type

	   #:xpath-error)
  (:documentation
   "Plexippus XPath is an XPath implementation for Common Lisp.
    TBD: document here:
    environments/variables, XPath sexpr-based syntax, representation of XPath
    types (incl. xnums), samples"))
