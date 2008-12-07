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

;; Plexippus-XPath extensions

(defparameter *plx-uri* "http://common-lisp.net/project/plexippus-xpath/plx")

(defmacro with-plx-extensions (&body body)
  "@short{Binds plx prefix to Plexippus XPath extensions namespace.}

   The following functions are currently available:

   @pre{plx:matches(string, pattern, flags?)}
   Returns true if @code{string} is matched by regular expression @code{pattern},
   false otherwise. Optional @code{flags} specify modifiers (i, m, s). CL-PPCRE
   is used as regular expression engine.

   @pre{plx:replace(string, pattern, replacement, flags?)}
   Returns @code{string} with all matches of regular expression @code{pattern}
   replaced with @code{replacement}. Optional @code{flags} specify modifiers
   (i, m, s).

   @pre{plx:current()}
   Returns a @class{node-set} consisting of one node which was specifed
   as context node for expression evaluation. Analagous to @code{current()}
   function of XSLT.

   @pre{plx:generate-id(node-set?)}
   Returns an alphanumeric string that uniquely identifies the first node of
   the @code{node-set} (or context node if @code{node-set} isn't specified)
   within its document. Analagous to @code{generate-id()} of XSLT (except
   that the latter works across multiple documents)."
  `(with-namespaces (("plx" *plx-uri*)) ,@body))

(define-extension plx *plx-uri*)

(defmacro define-regex-function (name args &body body)
  `(define-xpath-function/lazy plx ,name (,@args)
     (let ((prev-pattern (cons nil nil)))
       #'(lambda (ctx)
           (let* ((prev-pattern-copy prev-pattern)
                  (string (string-value (funcall string-thunk ctx)))
                  (pattern (format nil "~@[(?~a)~]~a"
                                   (when flags-thunk
                                     (string-value (funcall flags-thunk ctx)))
                                   (string-value (funcall pattern-thunk ctx))))
                  (compiled-pattern
                   (cdr (if (equal (car prev-pattern-copy) pattern)
                            prev-pattern-copy
                            (setf prev-pattern
                                  (cons pattern
                                        (progn
                                          (handler-case
                                              (cl-ppcre:create-scanner pattern)
                                            (cl-ppcre:ppcre-syntax-error (e)
                                              (xpath-error "regular expression syntax error: ~a: ~a"
                                                           pattern e))))))))))
             ,@body)))))

(define-regex-function matches (string-thunk pattern-thunk &optional flags-thunk)
  (when (cl-ppcre:scan compiled-pattern string) t))

(define-regex-function replace (string-thunk pattern-thunk replacement-thunk
                                             &optional flags-thunk)
  (cl-ppcre::regex-replace-all
   compiled-pattern string (funcall replacement-thunk ctx)))

(define-xpath-function/lazy plx current ()
  #'(lambda (ctx) (make-node-set (make-pipe (context-starting-node ctx) nil))))

(define-xpath-function/lazy plx generate-id (&optional node-set-thunk)
  (if node-set-thunk
      #'(lambda (ctx)
          (get-node-id (node-set-value (funcall node-set-thunk ctx))))
      #'(lambda (ctx)
          (get-node-id (context-node ctx)))))
