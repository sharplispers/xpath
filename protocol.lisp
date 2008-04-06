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


;; generic functions of the XPath protocol

(defparameter *navigator* :default-navigator)

(macrolet ((defprotocol (name &rest extra-args)
             (let ((navi-name
                    (find-symbol
                     (format nil "~A~A" name :-using-navigator)
                     (symbol-package name))))
               `(progn
                  (defgeneric ,navi-name (navigator node ,@extra-args))
                  (defun ,name (node ,@extra-args)
                    (,navi-name *navigator* node ,@extra-args))))))
  (defprotocol xpath-protocol:node-p)
  ;; NODE-EQUAL and NODE-IDENTITY need to be implemented so that the
  ;; following equavalence holds (but NODE-EQUAL might be implemented
  ;; directly instead):
  ;;   (node-equal a b) <=> (equal (hash-key a) (hash-key b))
  ;; (This is unlike Java's equals() and hashCode(), because HASH-KEY
  ;; doesn't return a hash code, it returns an object to be used in an
  ;; EQUAL hash table.)
  (defprotocol xpath-protocol:node-equal other)
  (defprotocol xpath-protocol:hash-key)
  (defprotocol xpath-protocol:child-pipe)
  (defprotocol xpath-protocol:attribute-pipe)
  (defprotocol xpath-protocol:namespace-pipe)
  (defprotocol xpath-protocol:parent-node)
  (defprotocol xpath-protocol:node-text)
  (defprotocol xpath-protocol:qualified-name)
  (defprotocol xpath-protocol:local-name)
  (defprotocol xpath-protocol:namespace-prefix)
  (defprotocol xpath-protocol:namespace-uri)
  (defprotocol xpath-protocol:processing-instruction-target)
  (defprotocol xpath-protocol:node-type-p type)
  (defprotocol xpath-protocol:base-uri)
  (defprotocol xpath-protocol:get-element-by-id id)
  (defprotocol xpath-protocol:unparsed-entity-uri name))

(defmacro define-default-method (name (&rest args) &body body)
  (let ((navi-name
         (find-symbol
          (format nil "~A~A" name :-using-navigator)
          (symbol-package name))))
    `(defmethod ,navi-name ((navigator (eql :default-navigator)) ,@args)
       ,@body)))

(define-default-method xpath-protocol:node-p ((node t))
  nil)

(define-default-method xpath-protocol:node-equal (a b)
  (eq a b))

(define-default-method xpath-protocol:hash-key (node)
  node)


;; helper functions

(defun parent-pipe (node)
  (let ((parent (xpath-protocol:parent-node node)))
    (if parent
        (list parent)
        empty-pipe)))

(defun vector->pipe (vector &optional (start 0))
  (if (>= start (length vector))
      empty-pipe
      (make-pipe (elt vector start)
                 (vector->pipe vector (1+ start)))))

;; DOM mapping: simple slots

(define-default-method xpath-protocol:node-p ((node dom:node)) t)

(define-default-method xpath-protocol:parent-node ((node dom:attr))
  (dom:owner-element node))

(define-default-method xpath-protocol:parent-node ((node dom:node))
  (dom:parent-node node))

(define-default-method xpath-protocol:local-name ((node dom:node))
  ;; fixme?
  (or (dom:local-name node) (dom:node-name node)))

(define-default-method xpath-protocol:namespace-prefix ((node dom:node))
  (dom:prefix node))

(define-default-method xpath-protocol:namespace-uri ((node dom:node))
  (or (dom:namespace-uri node) ""))

(define-default-method xpath-protocol:qualified-name ((node dom:node))
  (dom:node-name node))

(define-default-method xpath-protocol:processing-instruction-target
    ((node dom:node))
  (dom:node-value node))

(define-default-method xpath-protocol:base-uri ((node dom:node))
  ;; fixme
  "")


;; DOM mapping: pipes

(define-default-method xpath-protocol:parent-node ((node dom:node))
  (dom:parent-node node))

(define-default-method xpath-protocol:child-pipe ((node dom:node))
  empty-pipe)

(define-default-method xpath-protocol:child-pipe ((node dom:document))
  (list (dom:document-element node)))

(define-default-method xpath-protocol:child-pipe ((node dom:element))
  (vector->pipe (dom:child-nodes node)))

(define-default-method xpath-protocol:attribute-pipe ((node dom:node))
  empty-pipe)

(define-default-method xpath-protocol:attribute-pipe ((node dom:element))
  (filter-pipe #'(lambda (item)
                   (not (equal (dom:namespace-uri item)
                               "http://www.w3.org/2000/xmlns/")))
               (vector->pipe (dom:items (dom:attributes node)))))

(define-default-method xpath-protocol:namespace-pipe ((node dom:node))
  (when (dom:parent-node node)
    (xpath-protocol:namespace-pipe (dom:parent-node node))))

(defstruct (dom-namespace
             (:constructor make-dom-namespace (parent prefix uri)))
  parent
  prefix
  uri)

(define-default-method xpath-protocol:node-p ((node dom-namespace))
  t)

(define-default-method xpath-protocol:node-equal
    ((a dom-namespace) (b dom-namespace))
  (and (eq (dom-namespace-parent a) (dom-namespace-parent b))
       (equal (dom-namespace-prefix a) (dom-namespace-prefix b))))

(define-default-method xpath-protocol:hash-key
    ((node dom-namespace))
  (cons (dom-namespace-parent node) (dom-namespace-prefix node)))

(define-default-method xpath-protocol:child-pipe
    ((node dom-namespace))
  empty-pipe)
(define-default-method xpath-protocol:attribute-pipe
    ((node dom-namespace))
  empty-pipe)
(define-default-method xpath-protocol:namespace-pipe
    ((node dom-namespace))
  empty-pipe)

(define-default-method xpath-protocol:parent-node ((node dom-namespace))
  (dom-namespace-parent node))
(define-default-method xpath-protocol:local-name ((node dom-namespace))
  (dom-namespace-prefix node))
(define-default-method xpath-protocol:qualified-name ((node dom-namespace))
  (dom-namespace-prefix node))
(define-default-method xpath-protocol:namespace-prefix ((node dom-namespace))
  nil)
(define-default-method xpath-protocol:namespace-uri ((node dom-namespace))
  "")

(define-default-method xpath-protocol:namespace-pipe ((node dom:element))
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

(define-default-method xpath-protocol:node-text ((node dom:node))
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

(define-default-method xpath-protocol:node-text ((node dom-namespace))
  (dom-namespace-uri node))

;; currently computed from child-pipe
;;; (defmethod preceding-sibling-pipe ()
;;;   (let ((parent (dom:parent-node node)))
;;;     (if parent
;;;     (let* ((children (dom:child-nodes parent))
;;;            (pos (position node children)))
;;;       (loop
;;;          for i from (1- pos) downto 0
;;;          collect (elt children i)))
;;;     empty-pipe)))

(define-default-method xpath-protocol:node-type-p ((node dom:node) type)
  (declare (ignore type))
  nil)

(define-default-method xpath-protocol:node-type-p ((node dom-namespace) type)
  (declare (ignore type))
  nil)

(macrolet ((deftypemapping (class keyword)
             `(define-default-method xpath-protocol:node-type-p
                  ((node ,class) (type (eql ,keyword)))
                t)))
  (deftypemapping dom:comment :comment)
  (deftypemapping dom:processing-instruction :processing-instruction)
  (deftypemapping dom:text :text)
  (deftypemapping dom:attr :attribute)
  (deftypemapping dom:element :element)
  (deftypemapping dom-namespace :namespace)
  (deftypemapping dom:document :document))

(define-default-method xpath-protocol:get-element-by-id ((node dom:node) id)
  (dom:get-element-by-id
   (if (dom:document-p node) node (dom:owner-document node)) id))

(define-default-method xpath-protocol:unparsed-entity-uri
    ((node dom:node) name)
  (let ((dtd (rune-dom::dtd (if (dom:document-p node)
				node
				(dom:owner-document node)))))
    (when dtd
      (let ((entdef (cdr (gethash name (cxml::dtd-gentities dtd)))))
	(when (typep entdef 'cxml::external-entdef)
	  (let ((uri (cxml::extid-system (cxml::entdef-extid entdef))))
	    (when uri
	      (puri:render-uri uri nil))))))))

;; Character data

(define-default-method xpath-protocol:local-name
    ((node dom:character-data))
  "")

(define-default-method xpath-protocol:namespace-prefix
    ((node dom:character-data))
  "")

(define-default-method xpath-protocol:namespace-uri
    ((node dom:character-data))
  "")

(define-default-method xpath-protocol:qualified-name
    ((node dom:character-data))
  "")
