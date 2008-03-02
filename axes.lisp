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

;; axes


#|
FIXME: need to implement all axes

[6]     AxisName           ::=          'ancestor'      
                        | 'ancestor-or-self'    
                        | 'attribute'   
                        | 'child'       
                        | 'descendant'  
                        | 'descendant-or-self'  
                        | 'following'   
                        | 'following-sibling'   
                        | 'namespace'   
                        | 'parent'      
                        | 'preceding'   
                        | 'preceding-sibling'   
                        | 'self'        
|#


(defmacro define-axis (name (ordering &optional (principal-node-type :element)) &body body)
  (let ((func-name (hypsym 'axis name)))
    `(progn
       (defun ,func-name (node) ,@body)
       (setf (get ',name 'axis-function) ',func-name
             (get ',name 'axis-ordering) ',ordering
             (get ',name 'axis-principal-node-type) ',principal-node-type))))

(defun axis-function (axis) (get axis 'axis-function))

(defun axis-properties (axis)
  (values
   (get axis 'axis-function)
   (get axis 'axis-ordering)))

(defun axis-principal-node-type (axis) (get axis 'axis-principal-node-type))

(define-axis :child (:document-order) (xpath-protocol:child-pipe node))

(define-axis :self (:document-order) (list node))

(define-axis :parent (:reverse-document-order) (parent-pipe node))

(define-axis :descendant-or-self (:document-order)
  (make-pipe node
             (mappend-pipe (axis-function :descendant-or-self)
                           (xpath-protocol:child-pipe node))))

;; internal helper axis
(define-axis reverse-descendant-or-self (:reverse-document-order)
  (append-pipes (mappend-pipe (axis-function 'reverse-descendant-or-self)
                              (reverse
                               (force (xpath-protocol:child-pipe node))))
                (list node)))

(define-axis :descendant (:document-order)
  (mappend-pipe (axis-function :descendant-or-self)
                (xpath-protocol:child-pipe node)))

(define-axis :following-sibling (:document-order)
  (if (or (xpath-protocol:node-type-p node :namespace)
          (xpath-protocol:node-type-p node :attribute))
      empty-pipe
      (unless (null (xpath-protocol:parent-node node))
        (subpipe-after node (xpath-protocol:child-pipe
                             (xpath-protocol:parent-node node))))))

(define-axis :preceding-sibling (:reverse-document-order)
  (if (or (xpath-protocol:node-type-p node :namespace)
          (xpath-protocol:node-type-p node :attribute))
      empty-pipe
      (let ((parent (xpath-protocol:parent-node node)))
        (if parent
            (nreverse (force (subpipe-before node (xpath-protocol:child-pipe
                                                   parent))))
            empty-pipe))))

;; FIXME: test
;; FIXME: order

(define-axis :following (:document-order)
  (if (xpath-protocol:node-type-p node :attribute)
      (append-pipes
       (funcall (axis-function :descendant)
                (xpath-protocol:parent-node node))
       (funcall (axis-function :following)
                (xpath-protocol:parent-node node)))
      (mappend-pipe (axis-function :descendant-or-self)
                    (mappend-pipe (axis-function :following-sibling)
                                  (funcall (axis-function :ancestor-or-self)
                                           node)))))

(define-axis :preceding (:reverse-document-order)
  (mappend-pipe (axis-function 'reverse-descendant-or-self)
                (mappend-pipe (axis-function :preceding-sibling)
                              (funcall (axis-function :ancestor-or-self)
                                       node))))

(define-axis :ancestor (:reverse-document-order)
  (mappend-pipe (axis-function :ancestor-or-self)
                (parent-pipe node)))

(define-axis :ancestor-or-self (:reverse-document-order)
  (make-pipe node
             (mappend-pipe (axis-function :ancestor-or-self)
                           (parent-pipe node))))

(define-axis :attribute (:document-order :attribute)
  (xpath-protocol:attribute-pipe node))

(define-axis :namespace (:document-order :namespace)
  (when (xpath-protocol:node-type-p node :element)
    (xpath-protocol:namespace-pipe node)))

;; FIXME: This is a pseudo-axis used to implement absolute paths.
;; Is this the right approach?
(define-axis :root (:reverse-document-order)
  (loop
     for this = node then parent
     for parent = (xpath-protocol:parent-node this)
     while parent
     finally (return (list this))))
