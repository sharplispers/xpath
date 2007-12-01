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
  "Get the first node of a NODE-SET"
  (pipe-head (pipe-of node-set)))

(defun all-nodes (node-set)
  "Retrieve all nodes of a NODE-SET as a list"
  (force (pipe-of node-set)))

(defun map-node-set (func node-set)
  "Call FUNC for each node in NODE-SET"
  (map-pipe func (pipe-of node-set)))

(defmacro do-node-set ((var node-set &optional result) &body body)
  "Execute BODY for each node in NODE-SET binding VAR to the current node.
Return value of RESULT form or NIL if it doesn't specified."
  (check-type var symbol)
  `(block nil
     (map-node-set #'(lambda (,var) ,@body) ,node-set)
     ,result))

(defmacro xpath (form)
  "Used to specify sexpr-based XPath expression"
  `(list 'xpath ',form))

(defun compile-xpath (xpath &optional environment)
  (unless (or (stringp xpath) (functionp xpath)
              (and (consp xpath) (eq (car xpath) 'xpath) (null (cddr xpath))))
    (error "invalid xpath designator: ~A" xpath))
  (if (functionp xpath)
      xpath
      (compile-xpath/sexpr (if (stringp xpath)
                               (parse-xpath xpath)
                               (second xpath))
                           (or environment
                               (make-lexical-environment
                                *lexical-namespaces*)))))

(defun evaluate (xpath context)
  "Evaluate an XPath expression specified by XPATH in specified CONTEXT"
  ;; FIXME: Should this perhaps compute position and size based on 
  ;; the node's siblings instead?
  (funcall (compile-xpath xpath)
           (if (typep context 'context) context (make-context context))))

(define-compiler-macro evaluate (&whole whole &environment env xpath context)
  (if (not (constantp xpath env))
      whole
      (let ((namespaces (macroexpand '(lexical-namespaces) env)))
        (cond (namespaces
               `(evaluate (load-time-value
                           (compile-xpath ,xpath
                                          (make-lexical-environment ',namespaces)))
                          ,context))
              (t
               (warn "not using compiler macro because EVALUATE is used not inside with-namespaces")
               whole)))))
