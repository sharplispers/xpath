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

(in-package :xpath)

;; environment
;;
;; (a compilation-time context)

(defstruct environment)

(defgeneric environment-find-namespace (environment prefix)
  (:documentation "@arg[environment]{an XPath environment object}
  @arg[prefix]{prefix part of a QName}
  Returns namespace URI for specified @code{prefix}."))

(defgeneric environment-find-function (environment local-name uri)
  (:documentation "@arg[environment]{an XPath environment object}
  @arg[local-name]{local part of expanded-name of the function}
  @arg[uri]{namespace URI of the function}
  @return{an XPath function or nil if it cannot be found}
  @short{Finds an XPath function by @code{local-name} and @code{uri}}.

  XPath function is a Lisp function that takes zero or more \"thunks\"
  as its arguments (corresponding to XPath expressions passed as function
  arguments) and returns a new \"thunk\". A \"thunk\" is a function
  that takes an instance of @class{context} as its argument and returns
  the value of one of XPath types."))

(defgeneric environment-find-variable (environment local-name uri)
  (:documentation "@arg[environment]{an XPath environment object}
  @arg[local-name]{local part of expanded-name of the function}
  @arg[uri]{namespace URI of the function}
  @return{XPath variable \"thunk\"}
  @short{Finds an XPath variable by @code{local-name} and @code{uri}}.
  
  XPath variable is represented by a \"thunk\". A \"thunk\" is a function
  that takes an instance of @class{context} as its argument and returns
  the value of one of XPath types."))


;; dynamic environment
;;
;; The environment used automatically by EVALUATE macro, and that
;; knows about namespaces declared using WITH-NAMESPACES.

(defstruct (dynamic-environment
             (:include environment)
             (:constructor make-dynamic-environment
                           (namespaces)))
  namespaces)

(defmethod environment-find-namespace
    ((environment dynamic-environment) prefix)
  (cdr (assoc prefix (dynamic-environment-namespaces environment)
              :test 'equal)))

(defparameter *initial-namespaces*
  '((nil . "")
    ("xmlns" . #"http://www.w3.org/2000/xmlns/")
    ("xml" . #"http://www.w3.org/XML/1998/namespace")))

(defvar *dynamic-namespaces* *initial-namespaces*)
(defvar *dynamic-var-bindings* nil)

(defmethod environment-find-function ((environment dynamic-environment) lname uri)
  (find-xpath-function lname uri))

(defmethod environment-find-variable ((environment dynamic-environment) lname uri)
  (lambda (ctx)
    (declare (ignore ctx))
    (let ((item (assoc (cons lname uri)
                       *dynamic-var-bindings* :test 'equal)))
      (if item
          (cdr item)
          (error "undeclared variable: ~s (uri ~s)" lname uri)))))

(defun %namespace-binding-pair (prefix uri)
  (when (equal prefix "")
    (setf prefix nil))
  (check-type prefix (or string null))
  (check-type uri string)
  (cons (copy-seq prefix)
        (copy-seq uri)))

(defmacro with-namespaces ((&rest bindings) &body body)
  "@arg[bindings]{bindings in the form (PREFIX VALUE). PREFIXes and VALUEs are evaluated}
   @return{the tresult of evaluating @code{body}}
   @short{Provides namespace bindings for XPath compilation}

   Namespace bindings are used for compilation of XPath expressions.
   nil is equivalent of \"\" prefix. Bindings provided by this macro
   have dynamic scope."
  `(let ((*dynamic-namespaces*
          (append (list
                   ,@(loop for (prefix uri) in bindings
                           collect `(%namespace-binding-pair ,prefix ,uri)))
                  *dynamic-namespaces*)))
     ,@body))

(defun decode-dynamic-qname (qname)
  (multiple-value-bind (prefix local-name)
      (cxml::split-qname qname)
    (values local-name (find-dynamic-namespace prefix))))

(defun find-dynamic-namespace (prefix)
  (if prefix
      (or (cdr (assoc prefix *dynamic-namespaces* :test #'equal))
          (xpath-error "undeclared namespace: ~A" prefix))
      ""))

(defun %variable-binding-pair (qname value)
  (multiple-value-bind (local-name uri)
      (decode-dynamic-qname qname)
    (cons (cons local-name uri) value)))

(defmacro with-variables ((&rest bindings) &body body)
  "@arg[bindings]{bindings in the form (QNAME VALUE). QNAMEs and VALUEs are evaluated}
   @return{the tresult of evaluating @code{body}}
   @short{Provides bindings for XPath variables}

   Variable bindings are used for evaluation of compiled XPath expressions. Bindings
   provided by this macro have dynamic scope."
  `(let ((*dynamic-var-bindings*
          (append (list
                   ,@(loop for (qname value) in bindings
                           collect
                           `(%variable-binding-pair ,qname ,value))))))
     ,@body))

;; test environment
;;
;; (An environment that pretends to know about every namespace, function,
;; and variable. For use only in parser tests.)

(defstruct (test-environment (:include environment)))

(defmethod environment-find-namespace ((environment test-environment) prefix)
  (if (zerop (length prefix))
      ""
      (concatenate 'string "dummy://" prefix)))

(defmethod environment-find-function ((environment test-environment) lname uri)
  #'(lambda (&rest args)
      (xpath-error "function ~A, ~A was called with args ~A" lname uri args)))

(defmethod environment-find-variable
    ((environment test-environment) lname uri)
  (declare (ignore lname uri))
  t)
