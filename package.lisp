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

    @begin[Utilities powered by pattern matchers]{section}
    The following convenience functions and macros use patterns.  They
    are implemented using the lower-level functions listed below.

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
    @end{section}
    @begin[Applying pattern matchers]{section}
    To invoke a matcher created by @code{make-pattern-matcher}, use
    @code{matching-value} or @code{matching-values}:

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
           #:define-xpath-function/single-type
           #:enable-profiling
           #:disable-profiling
           #:report)
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

;;;     TBD: document here:
;;;     environments/variables, XPath sexpr-based syntax, representation of XPath
;;;     types (incl. xnums), extensions, samples

  (:documentation
   "Plexippus XPath is an XPath 1.0 implementation for Common Lisp.

    @begin[Using XPath]{section}
      Almost all uses of XPath involve the @code{evaluate} function, which
      can parse, compile, and invoke XPath expressions.

      @aboutfun{evaluate}
      @aboutmacro{xpath}
    @end{section}
    @begin[Compiling XPath dynamically]{section}
      @code{compile-xpath} allows the compilation of XPath into a closure
      ahead of time, so that @code{evaluate} only needs to invoke that closure
      rather than having to re-compile it first.

      Although @code{evaluate} itself already performs caching of compiled
      closures, explicit precompilation can aid optimizations if one
      call site uses multiple XPath expressions.

      Explicit compilation using @code{compile-xpath} is also required when
      using custom environment classes, since @code{evaluate} compiles
      expressions using the dynamic environment only.

      @code{parse-xpath} can be used to translate the standard string
      representation of XPath into a Plexippus-specific sexp representation.
      Both @code{compile-xpath} and @code{evaluate} accept sexps instead of
      strings.

      @aboutfun{parse-xpath}
      @aboutfun{compile-xpath}
    @end{section}
    @begin[Type coercion]{section}
      These correspond to the XPath functions boolean(),
      string(), and number().  In addition, @code{node-set-value}
      is provided, which turns nodes into node sets.

      @aboutfun{boolean-value}
      @aboutfun{string-value}
      @aboutfun{number-value}
      @aboutfun{node-set-value}
    @end{section}
    @begin[The dynamic environment]{section}
      The default enviroment used by @code{evaluate} is the dynamic
      environment, backed by information bound in dynamic variables.
      The following macros are used to bind these variables.  They have
      dynamic scope.  (The dynamic environment is currently not capable
      of dynamic declarations for variables, but can be used with extension
      functions that are declared globally.)

      (The XPATH-SYS defined an @a[xpath-sys.html]{environment protocol}
      for user-defined environment classes.)

      @aboutmacro{with-namespaces}
      @aboutmacro{with-variables}
    @end{section}
    @begin[The run-time context]{section}
      Instead of passing a node to @code{evaluate}, user code can construct
      a full context object.

      The context object specifies values to be returned by position(),
      current(), and last().

      @aboutclass{context}
      @aboutfun{make-context}
      @aboutfun{context-node}
      @aboutfun{context-starting-node}
      @aboutfun{context-position}
      @aboutfun{context-size}
    @end{section}
    @begin[Node sets]{section}
      Node sets are the XPath data type used to represent the results of
      evaluations that select multiple nodes.  As sets, they never contain
      duplicates.

      In addition to the high-level functions defined here, the XPATH-SYS
      package defined several @a[xpath-sys.html]{low-level node set functions}.
      Please also refer to the description there for details on node set order.

      @aboutfun{first-node}
      @aboutfun{all-nodes}
      @aboutfun{map-node-set}
      @aboutfun{map-node-set->list}
      @aboutmacro{do-node-set}
      @aboutfun{make-node-set-iterator}
      @aboutfun{node-set-iterator-end-p}
      @aboutfun{node-set-iterator-next}
      @aboutfun{node-set-iterator-current}
      @aboutfun{node-set-p}
      @aboutfun{node-set-empty-p}
      @aboutfun{node-set}
      @aboutfun{list->node-set}
      @aboutfun{sort-node-set}
    @end{section}
    @begin[Miscellaneous]{section}
      Other useful functions, variables, and classes:

      @aboutmacro{with-plx-extensions}
      @aboutvar{*navigator*}
      @aboutclass{xpath-error}
    @end{section}"))

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
                #:define-xpath-function/single-type
                #:enable-profiling
                #:disable-profiling
                #:report)
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
           #:define-xpath-function/single-type
           #:enable-profiling
           #:disable-profiling
           #:report)
  (:documentation
   "The XPATH-SYS package provides an API for extensions to Plexippus XPath.

   @begin[Pipes]{section}
   Pipes are lazy lists, inspired by their implementation in
   Norvig's 'Paradigms of Artificial Intelligence Programming'.

   @aboutmacro{make-pipe}
   @aboutfun{pipe-head}
   @aboutfun{pipe-tail}
   @end{section}
   @begin[Node sets]{section}
   Node sets are the XPath data type used to represent the results of
   evaluations that select multiple nodes.  As sets, they never contain
   duplicates. Conceptually, they are unordered, with the most important
   order defined on them being the document order.

   As a data structure though, node sets are backed by a pipe, and the order
   of elements in that pipe is well-documented: By default, the pipe of
   returned node sets is sorted into document order.  When unordered
   results are requested, the order is usually not specified, but in some
   cases, are already sorted according to the axis being queried, which
   is usually sorted either in document order,or in reverse document order.
   See @fun{xpath:evaluate} for the @code{unordered} argument.

   @aboutclass{node-set}
   @aboutfun{make-node-set}
   @aboutfun{pipe-of}
   @end{section}
   @begin[Implementing environments]{section}
   Environments provide compilation-time configuration for XPath.
   An environment is a CLOS object, which is queried by the compiler using
   generic functions that users can implement on their own subclasses
   of @class{xpath::environment}.

   The default environment class implements a `dynamic' environment, backed
   by information bound in dynamic variables, so that typical uses of XPath
   work without special environment classes.

   @aboutfun{environment-find-namespace}
   @aboutfun{environment-find-variable}
   @aboutfun{environment-find-function}
   @end{section}
   @begin[Defining extension functions]{section}
   XPath defines built-in functions in the empty namespace.  Using
   the extension API, user code can implement XPath functions addressed
   using other namespaces.

   @aboutmacro{define-extension}
   @aboutmacro{define-xpath-function/lazy}
   @aboutmacro{define-xpath-function/eager}
   @aboutmacro{define-xpath-function/single-type}
   @aboutfun{find-xpath-function}
   @end{section}
   @begin[Profiling support]{section}
   The profiling facility records the run time of XPath evaluations
   and pattern matching.

   @aboutfun{enable-profiling}
   @aboutfun{disable-profiling}
   @aboutfun{report}
   @end{section}
   @begin[Miscellaneous functions]{section}
   Other useful functions:

   @aboutfun{get-node-id}
   @end{section}"))
