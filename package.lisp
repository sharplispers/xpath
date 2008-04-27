;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

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
           #:node-equal
           #:hash-key
           #:parent-node
           #:local-name
           #:qualified-name
           #:namespace-prefix
           #:namespace-uri
           #:node-text
           #:processing-instruction-target
           #:node-type-p
           #:base-uri
           #:get-element-by-id
	   #:unparsed-entity-uri

           #:define-default-method

           #:child-pipe-using-navigator
           #:attribute-pipe-using-navigator
           #:namespace-pipe-using-navigator
           #:node-p-using-navigator
           #:hash-key-using-navigator
           #:node-equal-using-navigator
           #:parent-node-using-navigator
           #:local-name-using-navigator
           #:qualified-name-using-navigator
           #:namespace-prefix-using-navigator
           #:namespace-uri-using-navigator
           #:node-text-using-navigator
           #:processing-instruction-target-using-navigator
           #:node-type-p-using-navigator
           #:base-uri-using-navigator
           #:get-element-by-id-using-navigator
	   #:unparsed-entity-uri-using-navigator)
  (:documentation
   "XPATH-PROTOCOL package contains generic functions that are used to support
    multiple representations of XML documents for XPath."))

(defpackage :xpattern
  (:use)
  (:export #:pattern
           #:pattern-value
           #:pattern-priority
           #:matching-values
           #:matching-value
           #:parse-pattern-expression
           #:make-pattern-matcher*
           #:make-pattern-matcher
           #:compute-patterns
           #:node-matches-p
           #:pattern-case
           #:pattern-ecase
           #:*allow-variables-in-patterns*)
  (:documentation
   "The XPATTERN package implements pattern matching compatible with XSLT 1.0.

    @begin[Using pattern matchers]{section}
    @aboutfun{node-matches-p}
    @aboutmacro{pattern-case}
    @aboutmacro{pattern-ecase}
    @end{section}
    @begin[Compiling pattern matchers dynamically]{section}
    Pattern are represented as objects:

    @aboutclass{pattern}
    @aboutfun{pattern-value}
    @aboutfun{pattern-priority}
    Use @code{compute-patterns} to parse a pattern expression into
    pattern objects:

    @aboutfun{compute-patterns}
    @code{make-pattern-matcher} builds a matcher functions from multiple
    pattern objects.  The matcher will find the highest-priority match
    among them.

    @aboutfun{make-pattern-matcher}
    @aboutfun{make-pattern-matcher*}
    To invoke a matcher, use @code{matching-value} or @code{matching-values}:

    @aboutfun{matching-value}
    @aboutfun{matching-values}
    @end{section}"))

(defpackage :xpath
  (:use cl :xpattern)
  (:import-from :xpath-protocol #:define-default-method)
  (:intern #:make-node-set
           #:make-pipe
           #:pipe-head
           #:pipe-tail
           #:pipe-of
           #:get-node-id
           #:environment-find-namespace
           #:environment-find-variable
           #:environment-find-function
           #:find-xpath-function
           #:define-extension
           #:define-xpath-function/lazy
           #:define-xpath-function/eager
           #:define-xpath-function/single-type)
  (:export #:compile-xpath
           #:parse-xpath

           #:boolean-value
           #:string-value
           #:number-value
           #:node-set-value

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
           #:list->node-set
           #:sort-node-set

           #:context
           #:make-context
           #:context-node
           #:context-starting-node
           #:context-position
           #:context-size

           #:with-plx-extensions

           #:*navigator*

           #:xpath-error)
  (:documentation
   "Plexippus XPath is an XPath implementation for Common Lisp.

    TBD: document here:
    environments/variables, XPath sexpr-based syntax, representation of XPath
    types (incl. xnums), extensions, samples

    @begin[Using XPath]{section}
      TBD
    @end{section}
    @begin[Compiling XPath dynamically]{section}
      TBD
    @end{section}
    @begin[The dynamic environment]{section}
      TBD
    @end{section} "))

(defpackage :xpath-sys
  (:import-from :xpath-protocol
                #:define-default-method)
  (:import-from :xpath
                #:make-node-set
                #:make-pipe
                #:pipe-head
                #:pipe-tail
                #:pipe-of
                #:get-node-id
                #:environment-find-namespace
                #:environment-find-variable
                #:environment-find-function
                #:find-xpath-function
                #:define-extension
                #:define-xpath-function/lazy
                #:define-xpath-function/eager
                #:define-xpath-function/single-type)
  (:export #:make-node-set
           #:make-pipe
           #:pipe-head
           #:pipe-tail
           #:pipe-of
           #:get-node-id
           #:environment-find-namespace
           #:environment-find-variable
           #:environment-find-function
           #:find-xpath-function
           #:define-extension
           #:define-xpath-function/lazy
           #:define-xpath-function/eager
           #:define-xpath-function/single-type)
  (:documentation
   "XPATH-SYS package contains API for extending Plexippus XPath."))
