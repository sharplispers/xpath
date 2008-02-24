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


(in-package :xpath)

;; public evaluation API

(defun first-node (node-set)
  "@arg[node-set]{a @class{node-set}}
   @return{a @class{node-set} or nil}
   Returns the first node in the @code{node-set} or nil if it's empty."
  (pipe-head (pipe-of node-set)))

(defun all-nodes (node-set)
  "@arg[node-set]{a @class{node-set}}
   @return{a list of nodes}
   Returns all nodes of the @code{node-set} as a list."
  (force (pipe-of node-set)))

(defun map-node-set (func node-set)
  "@arg[func]{a function}
   @arg[node-set]{a @class{node-set}}
   @return{nil}
   @short{Calls @code{func} for each node in @code{node-set}}

   The operation is performed lazily, i.e. if it's terminated via
   a non-local exit it doesn't necessarily cause the XPath engine to find
   out all nodes in the @class{node-set} internally."
  (enumerate (pipe-of node-set) :key func :result nil))

(defun map-node-set->list (func node-set)
  "@arg[func]{a function}
   @arg[node-set]{a @class{node-set}}
   @return{a list}
   @short{Calls @code{func} for each node in @code{node-set} and conses up
   a list of its return values}

   The operation is performed lazily, i.e. if it's terminated via
   a non-local exit it doesn't necessarily cause the XPath engine to find
   out all nodes in the @class{node-set} internally."
  (loop for pipe = (pipe-of node-set) then (pipe-tail pipe)
        while (not (pipe-empty-p pipe))
        collect (funcall func (pipe-head pipe))))

(defmacro do-node-set ((var node-set &optional result) &body body)
  "@arg[var]{symbol, a variable name}
   @arg[node-set]{a @class{node-set}}
   @arg[result]{a form}
   @return{the result of evaluating @code{result}}
   @short{Executes @code{body} with @code{var} bound to successive nodes
     in @code{node-set}}

   The operation is performed lazily, i.e. if it's terminated via
   a non-local exit it doesn't necessarily cause the XPath engine to find
   out all nodes in the @class{node-set} internally.

   Returns nil if @code{result} form isn't specified."
  (check-type var symbol)
  `(block nil
     (map-node-set #'(lambda (,var) ,@body) ,node-set)
     ,result))

(defstruct (node-set-iterator
            (:constructor
             %make-node-set-iterator (pipe)))
  pipe)

(defun make-node-set-iterator (node-set)
  "@arg[node-set]{a @class{node-set}}
   @return{a node-set iterator}
   @short{Creates a node set iterator for @code{node-set}}

  Node set iterators can be used to iterate over node-sets.
  This can be done without causing the XPath engine to find out
  all their nodes and using non-local exits."
  (%make-node-set-iterator (pipe-of node-set)))

(defun node-set-iterator-end-p (iterator)
  "@arg[iterator]{a node-set iterator returned by @see{make-node-set-iterator}}
   @return{a generalized boolean}
   Returns true if @code{iterator} points to the end of its node set"
  (pipe-empty-p (node-set-iterator-pipe iterator)))

(defun node-set-iterator-next (iterator)
  "@arg[iterator]{a node-set iterator returned by @see{make-node-set-iterator}}
   @return{the value of @code{iterator}}
   Advances @code{iterator} if it's not at the end of its node set,
   does nothing otherwise."
  (unless (node-set-iterator-end-p iterator)
    (setf (node-set-iterator-pipe iterator)
          (pipe-tail (node-set-iterator-pipe iterator))))
  iterator)

(defun node-set-iterator-current (iterator)
  "@arg[iterator]{a node-set iterator returned by @see{make-node-set-iterator}}
   @return{a node or nil}
   Returns current node of @code{iterator} or nil if it's at the end
   of its node set."
  (if (node-set-iterator-end-p iterator)
      nil
      (pipe-head (node-set-iterator-pipe iterator))))

(defmacro xpath (form)
  "@arg[form]{a sexpr-based XPath form}
   @return{a list consisting of symbol XPATH and the @code{form}}
   This macro is used to specify sexpr-based XPath expression for @see{evaluate}"
  `(list 'xpath ',form))

(deftype xpath-expr ()
  '(or string function
    (cons (eql xpath) (cons t null))))

(defun compile-xpath (xpath &optional (environment (make-dynamic-environment *dynamic-namespaces*)))
  "@arg[xpath]{an XPath expression}
   @return{a compiled XPath expression}
   @short{Compiles an XPath expression}

   The @code{xpath} expression is compiled using current environment if it isn't
   compiled yet. @code{xpath} can be a string, a sexpr-based XPath epression or
   a compiled expression. In the latter case @code{xpath} argument value itself
   is returned."
  (unless (typep xpath 'xpath-expr)
    (xpath-error "invalid xpath designator: ~A" xpath))
  (if (functionp xpath)
      xpath
      (compile-xpath/sexpr (if (stringp xpath)
			       (parse-xpath xpath)
			       (second xpath))
			   environment)))

(defun evaluate-compiled (compiled-xpath context)
  "@arg[compiled-xpath]{a compiled XPath expression}
   @arg[context]{an XPath context}
   @return{the result of evaluating @code{compiled-xpath} within the @code{context}}
   @short{Evaluates a compiled XPath expression returned by @see{compile-xpath}}

   The @code{context} can be obtained using @see{make-context}. As an alternative,
   a node can be specifed."
  ;; FIXME: Should this perhaps compute position and size based on
  ;; the node's siblings instead?
  (funcall compiled-xpath
           (if (typep context 'context) context (make-context context))))

(defun same-expr-p (prev-expr xpath cur-bindings)
  (and (equal xpath (first prev-expr))
       (loop for (key . value) in (rest prev-expr)
	     when (not (equal value (cdr (assoc key cur-bindings :test #'equal))))
	       do (return nil)
	     finally (return t))))

(defmacro evaluate (xpath context)
  "@arg[xpath]{an XPath expression}
   @arg[context]{an XPath context}
   @return{the result of evaluating @code{xpath} within the @code{context}}
   @short{Evaluates an XPath expression}

   @code{xpath} can be a string, a sexpr-based XPath epression or
   a compiled expression. The @code{context} can be obtained using @see{make-context}.
   As an alternative, a node can be specifed."
  (with-gensyms (cache)
    (once-only (xpath)
      `(evaluate-compiled
	(let ((,cache (load-time-value (cons nil nil))))
	  (cond ((functionp ,xpath)
		 ,xpath)
		((same-expr-p (car ,cache) ,xpath *dynamic-namespaces*)
		 (cdr ,cache))
		(t
		 (setf (car ,cache) nil) ;; try to avoid race conditions
		 (prog1
		   (setf (cdr ,cache)
			 (compile-xpath ,xpath
					(make-dynamic-environment *dynamic-namespaces*)))
		   (setf (car ,cache) (cons ,xpath *dynamic-namespaces*))))))
	,context))))

;; errors

(define-condition xpath-error (simple-error)
  ()
  (:documentation "The class of all xpath errors."))

(defun xpath-error (fmt &rest args)
  (error 'xpath-error :format-control fmt :format-arguments args))
