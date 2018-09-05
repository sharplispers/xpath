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

(in-package #:xpath-test)

;; TODO: converting to string
(deftest test-xnum
  (assert* (xnum-p 1)
           (xnum-p -1d0)
           (xnum-p 3/5)
           (xnum-p :inf)
           (xnum-p :-inf)
           (xnum-p :nan)
           (compare-numbers 'equal :inf :inf)
           (compare-numbers 'equal :-inf :-inf)
           (compare-numbers '< :-inf :inf)
           (compare-numbers '<= :-inf :inf)
           (compare-numbers '> :inf :-inf)
           (compare-numbers '>= :inf :-inf)
           (compare-numbers '< :-inf 42)
           (compare-numbers '> :inf 42)
           (compare-numbers '<= :-inf 42)
           (compare-numbers '>= :inf 42)
           (compare-numbers 'equal 1/10000 0.0001d0))
  (loop for op in '(equal < > <= >=)
        do (assert* (not (compare-numbers op :nan :nan))
                    (not (compare-numbers op :nan :inf))
                    (not (compare-numbers op :inf :nan))
                    (not (compare-numbers op :nan :-inf))
                    (not (compare-numbers op :-inf :nan))
                    (not (compare-numbers op :nan 42))
                    (not (compare-numbers op :nan -42))
                    (not (compare-numbers op 42 :nan))
                    (not (compare-numbers op :nan "34"))))
  (assert-equal* 42 (parse-xnum "42")   ;; FIXME: double-float?
                 -1 (parse-xnum "-1")
                 5 (parse-xnum "   5  ")
                 :nan (parse-xnum "abc")
                 :nan (parse-xnum ""))
  (assert (< (abs (- 2.3d2 (parse-xnum "  2.3e+2  "))) 1e-7)) ;; FIXME
  (assert-equal* :inf (xnum-/ 1 0d0)
                 :-inf (xnum-/ -1 0)
                 0 (xnum-/ 42 :inf)
                 0 (xnum-/ 42d0 :-inf)
                 42 (xnum-/ 84 2)
                 :nan (xnum-/ 0 0)
                 :nan (xnum-/ :inf :inf)
                 :nan (xnum-/ :-inf :-inf)
                 :nan (xnum-/ :-inf :inf)
                 :nan (xnum-/ :inf :-inf)
                 42 (xnum-* 21 2)
                 0 (xnum-* 0 0)
                 :inf (xnum-* :inf 42)
                 :inf (xnum-* 42 :inf)
                 :inf (xnum-* :inf :inf)
                 :inf (xnum-* :-inf :-inf)
                 :-inf (xnum-* :-inf :inf)
                 :-inf (xnum-* 42 :-inf)
                 :-inf (xnum-* :-inf 42)
                 :nan (xnum-* 0 :inf)
                 :nan (xnum-* 0 :-inf)
                 :nan (xnum-* :inf 0)
                 :nan (xnum-* :-inf 0)
                 :nan (xnum-* 0 :inf)
                 :nan (xnum-* 0 :-inf)
                 42 (xnum-+ 20 22)
                 :inf (xnum-+ :inf :inf)
                 :inf (xnum-+ 42 :inf)
                 :inf (xnum-+ :inf 42)
                 :-inf (xnum-+ 42 :-inf)
                 :-inf (xnum-+ :-inf 42)
                 :-inf (xnum-+ :-inf :-inf)
                 :nan (xnum-+ :-inf :inf)
                 :nan (xnum-+ :inf :-inf)
                 -42 (xnum-- 42)
                 :-inf (xnum-- :inf)
                 :inf (xnum-- :-inf)
                 42 (xnum-- 100 58)
                 :inf (xnum-- 1 :-inf)
                 :inf (xnum-- :inf 1)
                 :-inf (xnum-- :-inf 1)
                 :-inf (xnum-- 1 :inf)
                 :nan (xnum-- :inf :inf)
                 :nan (xnum-- :-inf :-inf)
                 :nan (xnum-- :nan)
                 42 (xnum-mod 142 100)
                 42 (xnum-mod 42 :inf)
                 42 (xnum-mod 42 :-inf)
                 -42 (xnum-mod -42 :inf)
                 -42 (xnum-mod -42 :-inf)
                 :nan (xnum-mod :inf :-inf)
                 :nan (xnum-mod :-inf :inf)
                 :nan (xnum-mod :-inf 42)
                 :nan (xnum-mod :inf 42)
                 :nan (xnum-mod 0 0)
                 :nan (xnum-mod 42 0)
                 2 (xnum-round 1.6)
                 :nan (xnum-round :nan)
                 :inf (xnum-round :inf)
                 :-inf (xnum-round :-inf)
                 1 (xnum-floor 1.6)
                 :nan (xnum-floor :nan)
                 :inf (xnum-floor :inf)
                 :-inf (xnum-floor :-inf)
                 2 (xnum-ceiling 1.6)
                 :nan (xnum-ceiling :nan)
                 :inf (xnum-ceiling :inf)
                 :-inf (xnum-ceiling :-inf))
  (loop for op in '(xnum-/ xnum-* xnum-+ xnum-- xnum-mod)
        do (assert-equal*
            :nan (funcall op :nan :nan)
            :nan (funcall op :nan 42)
            :nan (funcall op 42 :nan)))
  (assert-equal* "42" (xnum->string 42)
                 "-42" (xnum->string -42)
                 "NaN" (xnum->string :nan)
                 "Infinity" (xnum->string :inf)
                 "-Infinity" (xnum->string :-inf)))
