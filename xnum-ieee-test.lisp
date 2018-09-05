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

(in-package :xpath-test)

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
