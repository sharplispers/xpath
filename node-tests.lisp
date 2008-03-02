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

;; node tests

(defun node-test-name (local-name &optional (uri ""))
  #'(lambda (node principal-node-type)
      #+nil
      (format t "node ~s principal-node-type ~s name ~s uri ~s~%"
              node principal-node-type
              local-name
              uri)
      (and (xpath-protocol:node-type-p node principal-node-type)
           (or (eq principal-node-type :namespace)
               (string= (xpath-protocol:namespace-uri node) uri))
           (string= (xpath-protocol:local-name node) local-name))))

(defun node-test-namespace (uri)
  #'(lambda (node principal-node-type)
      (and (xpath-protocol:node-type-p node principal-node-type)
           (string= (xpath-protocol:namespace-uri node) uri))))

(defun node-test-principal ()
  #'(lambda (node principal-node-type)
      (xpath-protocol:node-type-p node principal-node-type)))

(defun node-test-text-node ()
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (xpath-protocol:node-type-p node :text)))

(defun node-test-processing-instruction (&optional name)
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (and (xpath-protocol:node-type-p node :processing-instruction)
             (or (null name)
                 (equal (xpath-protocol:processing-instruction-target node)
                        name)))))

(defun node-test-comment ()
  #'(lambda (node principal-node-type)
      (declare (ignore principal-node-type))
      (xpath-protocol:node-type-p node :comment)))

(defun node-test-node () ;; FIXME: test
  #'(lambda (node principal-node-type)
      (declare (ignore node principal-node-type))
      t))
