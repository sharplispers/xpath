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

;; XPath compilation core

;; location path assembly

;; returns function: node -> pipe
(defun make-location-step (axis node-test predicates environment)
  (assert (axis-function axis) () "unknown axis: ~s" axis)
  (let ((predicate-closure (compile-predicates predicates environment)))
    (multiple-value-bind (axis-function axis-ordering)
        (axis-properties axis)
      #'(lambda (node starting-node)
          (values
           (funcall predicate-closure
                    (filter-pipe #'(lambda (node)
                                     (funcall node-test node
                                              (axis-principal-node-type axis)))
                                 (funcall axis-function node))
                    starting-node)
           axis-ordering)))))

(defun compile-predicates (predicates environment)
  (labels ((do-compile (predicates)
             (if predicates
                 (let ((predicate (car predicates))
                       (next (do-compile (rest predicates))))
                   #'(lambda (main-pipe starting-node)
                       (let ((context (make-context nil
                                                    #'(lambda () (pipe-length main-pipe))
                                                    0 starting-node)))
                         (funcall next
                                  (filter-pipe
                                   #'(lambda (cur-node)
                                       (setf (context-node context) cur-node)
                                       (incf (context-position context))
                                       (let ((pred-result (funcall predicate context)))
                                         (if (xnum-p pred-result)
                                             (= (context-position context) pred-result)
                                             (boolean-value pred-result))))
                                   main-pipe)
                                  starting-node))))
                 #'(lambda (node starting-node)
                     (declare (ignore starting-node))
                     node))))
    (do-compile (mapcar #'(lambda (p) (compile-xpath/sexpr p environment))
                        predicates))))

;; returns function: node -> pipe
(defun make-location-path (steps)
  (cond ((null steps) (xpath-error "make-location-path: no steps"))
        ((null (rest steps)) (first steps))
        (t
         (let ((first-of-path (first steps))
               (rest-of-path (make-location-path (rest steps))))
           #'(lambda (node starting-node)
               (let ((first-pipe (funcall first-of-path node starting-node)))
                 ;; we know that mappend-pipe will call the function eagerly
                 ;; once at the beginning, so we will get a value for
                 ;; for result-ordering before returning.
                 (let ((result-ordering :unordered))
                   (values
                    (mappend-pipe (lambda (n)
                                    (multiple-value-bind (pipe ordering)
                                        (funcall rest-of-path n starting-node)
                                      (setf result-ordering ordering)
                                      pipe))
                                  first-pipe)
                    ;; can't use the child step's ordering if we are
                    ;; unioning over multiple nodes:
                    (if (pipe-tail first-pipe)
                        :unordered
                        result-ordering)))))))))

;; most basic primitive functions

(defun xf-value (x) (constantly x))

(defun xf-true () (xf-value t))

(defun xf-location-path (path)
  #'(lambda (context)
      (multiple-value-call #'make-node-set
        (funcall path (context-node context)
                 (context-starting-node context)))))

;; compilation

(defun decode-qname (qname environment attributep)
  (multiple-value-bind (prefix local-name)
      (cxml::split-qname qname)
    (values local-name (find-namespace prefix environment attributep))))

(defun find-namespace (prefix environment attributep)
  (if (or prefix (not attributep))
      (or (environment-find-namespace environment prefix)
          (xpath-error "undeclared namespace: ~A" prefix))
      ""))

(defun compile-xpath/sexpr (expr environment)
  (cond ((atom expr) (xf-value expr))
        ((eq (first expr) :path)
         (compile-path (rest expr) environment))
        ((eq (first expr) :filter)
         (destructuring-bind (filter predicate &rest steps) (rest expr)
           (compile-filter-path filter predicate steps environment)))
        ((eq (first expr) :variable)
         (compile-variable (second expr) environment))
        (t
         (let ((name (first expr))
               (thunks
                (mapcar #'(lambda (e) (compile-xpath/sexpr e environment))
                        (rest expr))))
           (etypecase name
             ((cons (member :qfunc) (cons string (cons string null)))
              (destructuring-bind (prefix local-name) (rest name)
                (let ((uri (find-namespace prefix environment nil)))
                  (compile-xpath-function-call
                   (or
                    (environment-find-function environment local-name uri)
                    (xpath-error "no such function: ~s in namespace ~s" local-name uri))
                   thunks))))
             (symbol
              (compile-xpath-function-call
               (or (environment-find-function environment (string-downcase name) "")
                   (xpath-error "no such function: ~s" name))
               thunks)))))))

(defun compile-variable (name environment)
  (multiple-value-bind (local-name uri)
      (decode-qname name environment nil)
    (or (environment-find-variable environment local-name uri)
        (xpath-error "undeclared variable: ~A in namespace ~A"
                     local-name
                     uri))))

(defun compile-node-test (node-test environment attributep)
  (etypecase node-test
    (string
     (multiple-value-bind (local-name uri)
         (decode-qname node-test environment attributep)
       (node-test-name local-name uri)))
    (list
     (ecase (first node-test)
       (:processing-instruction
        (node-test-processing-instruction (second node-test)))
       (:namespace
        (let* ((prefix (second node-test))
               (uri (find-namespace prefix environment attributep)))
          (node-test-namespace uri)))
       (:qname
        ;; This case is just an alternative to the string case for the
        ;; convenience of callers that have a split name already.
        (destructuring-bind (prefix local-name) (rest node-test)
          (let ((uri (find-namespace prefix environment attributep)))
            (node-test-name local-name uri))))))
    (t
     (case node-test
       (* (node-test-principal))
       (:node (node-test-node))
       (:text (node-test-text-node))
       (:processing-instruction (node-test-processing-instruction))
       (:comment (node-test-comment))))))

(defun compile-location-step (step-spec environment)
  (destructuring-bind (axis node-test &rest predicates) step-spec
      (make-location-step axis
                          (compile-node-test node-test
                                             environment
                                             (eq axis :attribute))
                          predicates environment)))

(defun compile-path (path environment)
  (xf-location-path
   (make-location-path
    (mapcar #'(lambda (step) (compile-location-step step environment)) path))))

;; like compile-path, but with an initial node set that is computed by
;; a user expression `filter' rather than as the current node
(defun compile-filter-path (filter predicate path environment)
  (let* ((filter-thunk (compile-xpath/sexpr filter environment))
         (predicate-thunk (compile-predicates (list predicate) environment))
         (steps (mapcar #'(lambda (step)
                            (compile-location-step step environment))
                        path))
         (path-thunk (when steps
                       (make-location-path steps))))
    #'(lambda (context)
        (make-node-set
         (force
          (let ((initial-node-set (funcall filter-thunk context)))
            (unless (typep initial-node-set 'node-set)
              (xpath-error "not a node set: ~A" initial-node-set))
            (let* ((good-nodes
                    (funcall predicate-thunk
                             (sorted-pipe-of initial-node-set)
                             (context-starting-node context))))
              (if path-thunk
                  (mappend-pipe
                   #'(lambda (node)
                       (funcall path-thunk node (context-starting-node context)))
                   good-nodes)
                  good-nodes))))
         (if steps
             :unordered
             :document-order)))))
