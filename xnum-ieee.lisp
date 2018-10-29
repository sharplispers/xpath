;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007,2008 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2003-2008 David Lichteblau <david@lichteblau.com>

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

(defmacro with-float-traps-masked ((&optional) &body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero)
     ,@body)
  #-sbcl
  (error "implement me"))

(deftype xnum () 'number)

(defun xnum-p (value)
  (numberp value))

(defun nan-p (xnum)
  (and (floatp xnum)
       #+sbcl (sb-ext:float-nan-p xnum)
       #-sbcl (error "implement me")))

(defun x-zerop (xnum)
  (and (numberp xnum) (zerop xnum)))

(defun x-plusp (xnum)
  (and (numberp xnum) (plusp xnum)))

(defun x-minusp (xnum)
  (and (numberp xnum) (minusp xnum)))

(defun inf-p (num)
  (and (floatp num)
       #+sbcl (sb-ext:float-infinity-p num)
       #-sbcl (error "implement me")))

(defun finite-p (num)
  (not (inf-p num)))

(defun compare-numbers (op a b)
  (if (and (numberp a) (numberp b))
      (funcall (case op
		 (equal '=)
		 (t op))
	       (float a 1.0d0)
	       (float b 1.0d0))
      nil))

#+sbcl
(defun xnum-/ (a b)
  ;; force inline division, otherwise division by zero still errors out:
  (declare (optimize (speed 3) (safety 0)))
  (/ (float a 1.0d0) (float b 1.0d0)))
#-sbcl
(defun xnum-/ (a b)
  (error "implement me"))

(defun xnum-* (a b)
  (* (float a 1.0d0) (float b 1.0d0)))

(defun xnum-+ (a b)
  (+ (float a 1.0d0) (float b 1.0d0)))

(defun xnum-- (a &optional b)
  (if b
      (- (float a 1.0d0) (float b 1.0d0))
      (- (float a 1.0d0))))

#+sbcl
(sb-alien:define-alien-routine "fmod" double-float
  (f double-float)
  (g double-float))

(defun long-to-double (l)
  #+sbcl (sb-kernel:make-double-float (ldb (byte 32 32) l) (ldb (byte 32 0) l))
  #-sbcl (error "implement me"))

(defvar +nan+ (long-to-double 9221120237041090560))

(defun xnum-mod (f g)
  (if (zerop g)
      +nan+
      #+sbcl (fmod (float f 1.0d0) (float g 1.0d0))
      #-sbcl (error "implement me")))

;; Round to an integer, not a float.  But still pass NaN and infinity through.
(defun round-to-integer (a)
  (if (or (not (numberp a)) (nan-p a) (inf-p a))
      a
      ;; quick&dirty for now: first XNUM-ROUND to get the rounding mode right,
      ;; then ROUND to make an int
      (round (xnum-round a))))

(defun xnum-round (a)
  (setf a (float a 1.0d0))
  (cond
    ((eql a 0.0d0)
     0.0d0)
    ((<= -0.5d0 a 0.0d0)
     -0.0d0)
    (t
     (xnum-floor (+ a 0.5d0)))))

(defun xnum-ceiling (a)
  (fceiling a))

(defun xnum-floor (a)
  (ffloor a))

(defun preprocess-number-str (str)
  (cond ((member (mismatch str "+") '(1 nil)) "") ;force NaN
        ((position #\e str) (string-replace str "e" "d"))
        ((position #\d str) str)
        ((string= str ".") "")
        ((or (position #\. str)
             (and (plusp (length str)) (char= (elt str 0) #\-)))
         (concat str "d0"))
        (t str)))

(defun parse-xnum (str)
  (handler-case
      (parse-number:parse-number
       (preprocess-number-str (trim str)))
    ;; cannot use IGNORE-ERRORS here because PARSE-NUMBER::INVALID-NUMBER is
    ;; not an ERROR
    ((or error parse-number::invalid-number) ()
      +nan+)))

(defun xnum->string (xnum)
  (setf xnum (float xnum 1.0d0))
  ;; FIXME: this is not quite correct
  (cond
    ((nan-p xnum) "NaN")
    ((inf-p xnum)
     (if (plusp xnum)
	 "Infinity"
	 "-Infinity"))
    ((eql xnum -0.0d0)
     ;; Insane: We have to go through a lot of trouble to get -0 right, but
     ;; then we're supposed to *print* it as 0 after all.
     "0")
    (t (cl-ppcre:regex-replace "\\.?0*$" (string-replace (format nil "~f" xnum) "d" "e") ""))))
