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


;; generic functions of the XPath protocol

(defgeneric xpath-protocol:child-pipe (node))
(defgeneric xpath-protocol:attribute-pipe (node))
(defgeneric xpath-protocol:namespace-pipe (node))

(defgeneric xpath-protocol:parent-node (node))
(defgeneric xpath-protocol:string-value (node))
(defgeneric xpath-protocol:qualified-name (node))
(defgeneric xpath-protocol:local-name (node))
(defgeneric xpath-protocol:namespace-prefix (node))
(defgeneric xpath-protocol:namespace-uri (node))
(defgeneric xpath-protocol:processing-instruction-target (node))
(defgeneric xpath-protocol:node-type-p (node type))


;; helper functions

(defun parent-pipe (node)
  (let ((parent (xpath-protocol:parent-node node)))
    (if parent
	(list parent)
	empty-pipe)))


;; DOM mapping: simple slots

(defmethod xpath-protocol:parent-node ((node dom:node))
  (dom:parent-node node))

(defmethod xpath-protocol:local-name ((node dom:node))
  ;; fixme?
  (or (dom:local-name node) (dom:node-name node)))

(defmethod xpath-protocol:namespace-prefix ((node dom:node))
  (dom:prefix node))

(defmethod xpath-protocol:namespace-uri ((node dom:node))
  (or (dom:namespace-uri node) ""))

(defmethod xpath-protocol:qualified-name ((node dom:node))
  (dom:node-name node))

(defmethod xpath-protocol:processing-instruction-target ((node dom:node))
  (dom:node-value node))


;; DOM mapping: pipes

(defmethod xpath-protocol:parent-node ((node dom:node))
  (dom:parent-node node))

(defmethod xpath-protocol:child-pipe ((node dom:node))
  empty-pipe)

(defmethod xpath-protocol:child-pipe ((node dom:document))
  (list (dom:document-element node)))

(defmethod xpath-protocol:child-pipe ((node dom:element))
  (vector->pipe (dom:child-nodes node)))

(defmethod xpath-protocol:attribute-pipe ((node dom:node))
  empty-pipe)

(defmethod xpath-protocol:attribute-pipe ((node dom:element))
  (filter-pipe #'(lambda (item)
		   (not (equal (dom:namespace-uri item)
			       "http://www.w3.org/2000/xmlns/")))
	       (vector->pipe (dom:items (dom:attributes node)))))

(defmethod xpath-protocol:namespace-pipe ((node dom:node))
  (when (dom:parent-node node)
    (xpath-protocol:namespace-pipe (dom:parent-node node))))

(defstruct (dom-namespace
	     (:constructor make-dom-namespace (parent prefix uri)))
  parent
  prefix
  uri)

(defmethod xpath-protocol:child-pipe ((node dom-namespace)) empty-pipe)
(defmethod xpath-protocol:attribute-pipe ((node dom-namespace)) empty-pipe)
(defmethod xpath-protocol:namespace-pipe ((node dom-namespace)) empty-pipe)

(defmethod xpath-protocol:parent-node ((node dom-namespace))
  (dom-namespace-parent node))
(defmethod xpath-protocol:local-name ((node dom-namespace))
  (dom-namespace-prefix node))
(defmethod xpath-protocol:qualified-name ((node dom-namespace))
  (dom-namespace-prefix node))
(defmethod xpath-protocol:namespace-prefix ((node dom-namespace))
  nil)
(defmethod xpath-protocol:namespace-uri ((node dom-namespace))
  (dom-namespace-uri node))

(defmethod xpath-protocol:namespace-pipe ((node dom:element))
  ;; FIXME: completely untested
  ;; FIXME: rewrite this lazily?
  (let ((table (make-hash-table :test 'equal))
	(result '()))
    (labels ((record* (parent prefix uri)
	       (unless (or (equal prefix "xmlns")
			   (gethash prefix table))
		 (setf (gethash prefix table)
		       (make-dom-namespace parent prefix uri))))
	     (record (parent node)
	       (record* parent
			(or (dom:prefix node) "")
			(dom:namespace-uri node)))
	     (recurse (node)
	       (record node node)
	       (dolist (attribute (dom:items (dom:attributes node)))
		 (cond
		   ((equal (dom:namespace-uri attribute)
			   "http://www.w3.org/2000/xmlns/")
		    ;; record explicitly declared namespaces, which might
		    ;; not be in use anywhere
		    (record* node
			     (dom:local-name attribute)
			     (dom:value attribute)))
		   ((plusp (length (dom:prefix attribute)))
		    ;; record namespaces from DOM 2 slots, which might not
		    ;; be declared in an attribute
		    (record node attribute))))
	       (let ((parent (dom:parent-node node)))
		 (when parent
		   (recurse parent)))))
      (record* nil "xml" "http://www.w3.org/XML/1998/namespace")
      (recurse node))
    (maphash #'(lambda (prefix nsnode)
		 (declare (ignore prefix))
		 (push nsnode result))
	     table)
    result))

(defmethod xpath-protocol:string-value ((node dom:node))
  ;; FIXME: support document and document-fragment
  (with-output-to-string (s)
    (labels ((write-text (node)
	       (let ((value (dom:node-value node)))
		 (when value (write-string value s))
                 (unless (dom:attribute-p node) ;; FIXME: verify CDATA sections
                   (dom:do-node-list (child (dom:child-nodes node))
                     (cond ((or (dom:element-p child)
                                (dom:entity-reference-p child))
                            (write-text child))
                           ((or (dom:text-node-p child)
                                (dom:attribute-p child)
                                (dom:cdata-section-p child))
                            (write-string (dom:node-value child) s))))))))
      (write-text node))))

;; currently computed from child-pipe
;;; (defmethod preceding-sibling-pipe ()
;;;   (let ((parent (dom:parent-node node)))
;;;     (if parent
;;; 	(let* ((children (dom:child-nodes parent))
;;; 	       (pos (position node children)))
;;; 	  (loop
;;; 	     for i from (1- pos) downto 0
;;; 	     collect (elt children i)))
;;; 	empty-pipe)))

(defmethod xpath-protocol:node-type-p ((node dom:node) type)
  (declare (ignore type))
  nil)

(defmethod xpath-protocol:node-type-p ((node dom-namespace) type)
  (declare (ignore type))
  nil)

(macrolet ((deftypemapping (class keyword)
	     `(defmethod xpath-protocol:node-type-p
		  ((node ,class) (type (eql ,keyword)))
		t)))
  (deftypemapping dom:comment :comment)
  (deftypemapping dom:processing-instruction :processing-instruction)
  (deftypemapping dom:text :text)
  (deftypemapping dom:attr :attribute)
  (deftypemapping dom:element :element)
  (deftypemapping dom-namespace :namespace))
