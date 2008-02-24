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

(defun mklist (value)
  (if (atom value) (list value) value))

(defun concat (&rest values)
  "Concatenate string representations (PRINC-TO-STRING) of VALUES"
  (apply #'concatenate 'string
	 (mapcar #'princ-to-string values)))

(defun string-replace (string substring replacement)
  "Replace occurences of SUBSTRING in STRING with REPLACEMENT."
  (if (or (zerop (length string)) (zerop (length substring)))
      string
      (with-output-to-string (out)
	(labels ((rec (start)
		   (let ((p (search substring string :start2 start)))
		     (cond (p (write-string string out :start start :end p)
			      (write-string replacement out)
			      (rec (+ p (length substring))))
			   (t (write-string string out :start start))))))
	  (rec 0)))))

(defun trim (str)
  (string-trim '(#\space #\tab #\newline #\return) (or str "")))

(defmacro with-gensyms (syms &rest body)
  "Execute the body binding syms to gensyms"
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (gensym ,(concat (string sym) "-"))))
		 syms)
     ,@body))

;; ONCE-ONLY was lifted from Practical Common Lisp (slightly modified)
(defmacro once-only ((&rest names) &body body)
  (assert (every #'symbolp names) () "all names should be symbols: ~s" names)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
		,@body)))))

(defun proper-list-p (list)
  "True if the LIST is proper list"
  (or (null list)
      (and (consp list)
	   (proper-list-p (cdr list)))))

(defun maybe-progn (body)
  "Wrap the BODY with PROGN if it contains more than one form.
Otherwise return the first form or NIL if the body is empty"
  (assert (proper-list-p body) () "body is not a proper list: ~s" body)
  (if (rest body)
      `(progn ,@body)
      (first body)))

(defun hypsym (&rest parts)
  "Assemble a symbol from PARTS interleaving them with hyphens"
  (intern
   (with-standard-io-syntax
     (format nil "~{~a~^-~}" parts))))
