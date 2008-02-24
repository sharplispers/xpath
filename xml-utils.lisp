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

(defun get-node-text (node)
  (xpath-protocol:string-value node))

;; pipe-related

(defun vector->pipe (vector &optional (start 0))
  (if (>= start (length vector))
      empty-pipe
      (make-pipe (elt vector start)
		 (vector->pipe vector (1+ start)))))


;; some hairy hacks follow that make it easier for me to work with CXML DOM from REPL

(defun xml-serialize (xml &key skip-xmlns indent-p canonical-p)
  (dom:map-document (cxml:make-whitespace-normalizer
                     (cxml:make-string-sink :canonical canonical-p
                                            :indentation (and indent-p 4)
                                            :width 1000))
                    (ensure-document xml)
		    :include-xmlns-attributes (not skip-xmlns)))

(defun xml->string/headless (item &key skip-xmlns indent-p canonical-p)
  (cl-ppcre:regex-replace (rx "^<\\?.*?\\?>\\s+" :single-line-mode t) ;; FIXME: somewhat hairy approach
                          (xml-serialize item :skip-xmlns skip-xmlns :indent-p indent-p :canonical-p canonical-p) ""))

(defun sx (item) ;; for REPL -- hairy!
  (princ (xml->string/headless item :skip-xmlns t :indent-p t))
  (values))

(defun element->document (element)
  (let ((new-document (dom:create-document (dom:implementation (dom:owner-document element)) nil nil nil)))
    (dom:append-child new-document (dom:import-node new-document element t))
    new-document))

(defun ensure-document (item)
  (etypecase item
    (dom:element (element->document item))
    (dom:document item)))
