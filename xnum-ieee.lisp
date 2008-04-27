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

(defmacro assert-float-equal (expected actual)
  "Check whether two floating-point values are equal"
  (with-gensyms (exp-value act-value)
    `(let ((,exp-value ,expected)
           (,act-value ,actual))
      (unless (or (= ,exp-value ,act-value)
		  (and (nan-p ,exp-value) (nan-p ,act-value)))
        (error "TEST FAILED: ~s is expected to be~%~s~%but was~%~s"
               ',actual ,exp-value ,act-value)))))

(defmacro assert-float-equal* (&rest pairs)
  (maybe-progn
   (loop for (expected actual) on pairs by #'cddr
         collect `(assert-float-equal ,expected ,actual))))

;; TODO: converting to string
(deftest test-xnum
  (with-float-traps-masked ()
    (assert* (xnum-p 1)
	     (xnum-p -1d0)
	     (xnum-p 3/5)
	     (xnum-p #.sb-ext:double-float-positive-infinity)
	     (xnum-p #.sb-ext:double-float-negative-infinity)
	     (xnum-p +nan+)
	     (compare-numbers 'equal #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-positive-infinity)
	     (compare-numbers 'equal #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-negative-infinity)
	     (compare-numbers '< #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-positive-infinity)
	     (compare-numbers '<= #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-positive-infinity)
	     (compare-numbers '> #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-negative-infinity)
	     (compare-numbers '>= #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-negative-infinity)
	     (compare-numbers '< #.sb-ext:double-float-negative-infinity 42)
	     (compare-numbers '> #.sb-ext:double-float-positive-infinity 42)
	     (compare-numbers '<= #.sb-ext:double-float-negative-infinity 42)
	     (compare-numbers '>= #.sb-ext:double-float-positive-infinity 42)
	     (compare-numbers 'equal 1/10000 0.0001d0))
    (loop for op in '(equal < > <= >=)
       do (assert* (not (compare-numbers op +nan+ +nan+))
		   (not (compare-numbers op +nan+ #.sb-ext:double-float-positive-infinity))
		   (not (compare-numbers op #.sb-ext:double-float-positive-infinity +nan+))
		   (not (compare-numbers op +nan+ #.sb-ext:double-float-negative-infinity))
		   (not (compare-numbers op #.sb-ext:double-float-negative-infinity +nan+))
		   (not (compare-numbers op +nan+ 42))
		   (not (compare-numbers op +nan+ -42))
		   (not (compare-numbers op 42 +nan+))
		   (not (compare-numbers op +nan+ "34"))))
    (assert-float-equal* 42 (parse-xnum "42") ;; FIXME: double-float?
			 -1 (parse-xnum "-1")
			 5 (parse-xnum "   5  ")
			 +nan+ (parse-xnum "abc")
			 +nan+ (parse-xnum ""))
    (assert (< (abs (- 2.3d2 (parse-xnum "  2.3e+2  "))) 1e-7)) ;; FIXME
    (assert-float-equal* #.sb-ext:double-float-positive-infinity (xnum-/ 1 0d0)
			 #.sb-ext:double-float-negative-infinity (xnum-/ -1 0)
			 0 (xnum-/ 42 #.sb-ext:double-float-positive-infinity)
			 0 (xnum-/ 42d0 #.sb-ext:double-float-negative-infinity)
			 42 (xnum-/ 84 2)
			 +nan+ (xnum-/ 0 0)
			 +nan+ (xnum-/ #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-/ #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-negative-infinity)
			 +nan+ (xnum-/ #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-/ #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-negative-infinity)
			 42 (xnum-* 21 2)
			 0 (xnum-* 0 0)
			 #.sb-ext:double-float-positive-infinity (xnum-* #.sb-ext:double-float-positive-infinity 42)
			 #.sb-ext:double-float-positive-infinity (xnum-* 42 #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-positive-infinity (xnum-* #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-positive-infinity (xnum-* #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-negative-infinity)
			 #.sb-ext:double-float-negative-infinity (xnum-* #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-negative-infinity (xnum-* 42 #.sb-ext:double-float-negative-infinity)
			 #.sb-ext:double-float-negative-infinity (xnum-* #.sb-ext:double-float-negative-infinity 42)
			 +nan+ (xnum-* 0 #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-* 0 #.sb-ext:double-float-negative-infinity)
			 +nan+ (xnum-* #.sb-ext:double-float-positive-infinity 0)
			 +nan+ (xnum-* #.sb-ext:double-float-negative-infinity 0)
			 +nan+ (xnum-* 0 #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-* 0 #.sb-ext:double-float-negative-infinity)
			 42 (xnum-+ 20 22)
			 #.sb-ext:double-float-positive-infinity (xnum-+ #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-positive-infinity (xnum-+ 42 #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-positive-infinity (xnum-+ #.sb-ext:double-float-positive-infinity 42)
			 #.sb-ext:double-float-negative-infinity (xnum-+ 42 #.sb-ext:double-float-negative-infinity)
			 #.sb-ext:double-float-negative-infinity (xnum-+ #.sb-ext:double-float-negative-infinity 42)
			 #.sb-ext:double-float-negative-infinity (xnum-+ #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-negative-infinity)
			 +nan+ (xnum-+ #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-+ #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-negative-infinity)
			 -42 (xnum-- 42)
			 #.sb-ext:double-float-negative-infinity (xnum-- #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-positive-infinity (xnum-- #.sb-ext:double-float-negative-infinity)
			 42 (xnum-- 100 58)
			 #.sb-ext:double-float-positive-infinity (xnum-- 1 #.sb-ext:double-float-negative-infinity)
			 #.sb-ext:double-float-positive-infinity (xnum-- #.sb-ext:double-float-positive-infinity 1)
			 #.sb-ext:double-float-negative-infinity (xnum-- #.sb-ext:double-float-negative-infinity 1)
			 #.sb-ext:double-float-negative-infinity (xnum-- 1 #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-- #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-- #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-negative-infinity)
			 +nan+ (xnum-- +nan+)
			 42 (xnum-mod 142 100)
			 42 (xnum-mod 42 #.sb-ext:double-float-positive-infinity)
			 42 (xnum-mod 42 #.sb-ext:double-float-negative-infinity)
			 -42 (xnum-mod -42 #.sb-ext:double-float-positive-infinity)
			 -42 (xnum-mod -42 #.sb-ext:double-float-negative-infinity)
			 +nan+ (xnum-mod #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-negative-infinity)
			 +nan+ (xnum-mod #.sb-ext:double-float-negative-infinity #.sb-ext:double-float-positive-infinity)
			 +nan+ (xnum-mod #.sb-ext:double-float-negative-infinity 42)
			 +nan+ (xnum-mod #.sb-ext:double-float-positive-infinity 42)
			 +nan+ (xnum-mod 0 0)
			 +nan+ (xnum-mod 42 0)
			 2 (xnum-round 1.6)
			 +nan+ (xnum-round +nan+)
			 #.sb-ext:double-float-positive-infinity (xnum-round #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-negative-infinity (xnum-round #.sb-ext:double-float-negative-infinity)
			 1 (xnum-floor 1.6)
			 +nan+ (xnum-floor +nan+)
			 #.sb-ext:double-float-positive-infinity (xnum-floor #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-negative-infinity (xnum-floor #.sb-ext:double-float-negative-infinity)
			 2 (xnum-ceiling 1.6)
			 +nan+ (xnum-ceiling +nan+)
			 #.sb-ext:double-float-positive-infinity (xnum-ceiling #.sb-ext:double-float-positive-infinity)
			 #.sb-ext:double-float-negative-infinity (xnum-ceiling #.sb-ext:double-float-negative-infinity))
    (loop for op in '(xnum-/ xnum-* xnum-+ xnum-- xnum-mod)
       do (assert-float-equal*
	   +nan+ (funcall op +nan+ +nan+)
	   +nan+ (funcall op +nan+ 42)
	   +nan+ (funcall op 42 +nan+)))
    (assert-equal* "42" (xnum->string 42)
		   "-42" (xnum->string -42)
		   "NaN" (xnum->string +nan+)
		   "Infinity" (xnum->string #.sb-ext:double-float-positive-infinity)
		   "-Infinity" (xnum->string #.sb-ext:double-float-negative-infinity))))
