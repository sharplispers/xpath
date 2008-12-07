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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Please note:
;;;
;;; When changing Plexippus XPath, also run the entire
;;; XSLT suite using Xuriella XSLT like this:
;;;
;;;   $ cd xuriella && git submodule update --init
;;;   > (xuriella::run-tests)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tests* nil)

(defmacro assert-equal (expected actual)
  "Check whether two values are equal"
  (with-gensyms (exp-value act-value)
    `(let ((,exp-value ,expected)
           (,act-value ,actual))
      (unless (equal ,exp-value ,act-value)
        (error "TEST FAILED: ~s is expected to be~%~s~%but was~%~s"
               ',actual ,exp-value ,act-value)))))

(defmacro assert-equal* (&rest pairs)
  (maybe-progn
   (loop for (expected actual) on pairs by #'cddr
         collect `(assert-equal ,expected ,actual))))

(defmacro assert* (&rest expressions)
  (maybe-progn
   (loop for expr in expressions collect `(assert ,expr))))

(defmacro deftest (name &body body)
  `(progn
     (defun ,name ()
       (with-float-traps-masked ()
         ,@body))
     (pushnew ',name *tests*)))

(defun run-all-tests ()
  (mapc #'funcall (sort (copy-list *tests*) #'string<)))
