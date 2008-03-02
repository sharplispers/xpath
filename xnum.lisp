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

;; mock IEEE 754 support (to become proper IEEE 754 emulation module someday)

(defmacro with-float-traps-masked ((&optional) &body body)
  `(progn ,@body))

(defvar +nan+ :nan)

(deftype xnum () '(or number (member :nan :inf :-inf)))

(defun xnum-p (value)
  (typep value 'xnum))

(defun nan-p (xnum) (eq xnum :nan))

(defun x-zerop (xnum) (and (numberp xnum) (zerop xnum)))

(defun x-plusp (xnum) (or (eq xnum :inf) (and (numberp xnum) (plusp xnum))))

(defun x-minusp (xnum) (or (eq xnum :-inf) (and (numberp xnum) (minusp xnum))))

(defun inf-p (num) (or (eq num :inf) (eq num :-inf)))

(defun finite-p (num) (numberp num))

(defun compare-numbers (op a b) ; FIXME: this func too hairy
  (and (not (nan-p a))
       (not (nan-p b))
       (labels ((v= () (or (eq a b) (and (numbers-p) (= (coerce a 'double-float) (coerce b 'double-float)))))
                (numbers-p () (and (numberp a) (numberp b)))
                (v<= () (or (eq a :-inf) (eq b :inf) (and (numbers-p) (< a b))))
                (v>= () (or (eq a :inf) (eq b :-inf) (and (numbers-p) (> a b)))))
         (case op
           (equal (v=))
           (< (and (not (v=)) (v<=)))
           (<= (or (v=) (v<=)))
           (> (and (not (v=)) (v>=)))
           (>= (or (v=) (v>=)))))))

(defun xnum-/ (a b)
  (cond ((or (nan-p a) (nan-p b)) :nan)
        ((and (inf-p a) (inf-p b)) :nan)
        ((and (x-zerop a) (x-zerop b)) :nan)
        ((eq a :inf) (if (x-plusp b) :inf :-inf))
        ((eq a :-inf) (if (x-plusp b) :-inf :inf))
        ((inf-p b) 0)
        ((x-zerop b) (if (x-plusp a) :inf :-inf))
        (t (/ a b))))

(defun xnum-* (a b)
  (cond ((or (nan-p a) (nan-p b)) :nan)
        ((and (inf-p a) (x-zerop b)) :nan)
        ((and (x-zerop a) (inf-p b)) :nan)
        ((and (finite-p a) (finite-p b)) (* a b))
        ((and (x-plusp a) (x-plusp b)) :inf)
        ((and (x-minusp a) (x-minusp b)) :inf)
        (t :-inf)))

(defun xnum-+ (a b)
  (cond ((or (nan-p a) (nan-p b)) :nan)
        ((and (eq a :inf) (eq b :-inf)) :nan)
        ((and (eq a :-inf) (eq b :inf)) :nan)
        ((and (finite-p a) (finite-p b)) (+ a b))
        ((or (eq a :inf) (eq b :inf)) :inf)
        (t :-inf)))

(defun xnum-- (a &optional b)
  (cond ((or (nan-p a) (nan-p b)) :nan)
        ((and (null b) (eq a :inf)) :-inf)
        ((and (null b) (eq a :-inf)) :inf)
        ((null b) (- a))
        ((and (finite-p a) (finite-p b)) (- a b))
        ((eq a b) :nan)
        ((or (eq a :inf) (eq b :-inf)) :inf)
        (t :-inf)))

(defun xnum-mod (a b)
  (cond ((or (nan-p a) (nan-p b) (inf-p a) (x-zerop b)) :nan)
        ((inf-p b) a)
        (t (rem a b))))

;; Round to an integer, not a float.  But pass NaN and infinity through.
(defun round-to-integer (a)
  (xnum-round a))

(defun xnum-round (a)
  (if (or (nan-p a) (inf-p a))
      a
      (multiple-value-bind (ceiling rem)
          (ceiling a)
        (if (= rem -0.5)
            ceiling
            (round a)))))

(defun xnum-ceiling (a)
  (if (or (nan-p a) (inf-p a))
      a
      (ceiling a)))

(defun xnum-floor (a)
  (if (or (nan-p a) (inf-p a))
      a
      (floor a)))

(defun preprocess-number-str (str)
  (cond ((position #\e str) (string-replace str "e" "d"))
        ((position #\d str) str)
        ((string= str ".") "")
        ((position #\. str) (concat str "d0"))
        (t str)))

(defun parse-xnum (str)
  (handler-case
      (parse-number:parse-number
       (preprocess-number-str (trim str)))
    ;; cannot use IGNORE-ERRORS here because PARSE-NUMBER::INVALID-NUMBER is
    ;; not an ERROR
    ((or error parse-number::invalid-number) ()
      :nan)))

(defun xnum->string (xnum)
  ;; FIXME: this is not quite correct
  (case xnum
    (:nan "NaN")
    (:inf "Infinity")
    (:-inf "-Infinity")
    (t (cl-ppcre:regex-replace "\\.?0*$" (string-replace (format nil "~f" xnum) "d" "e") ""))))

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
  (assert-equal* 42 (parse-xnum "42") ;; FIXME: double-float?
                 -1 (parse-xnum "-1")
                 5 (parse-xnum "   5  ")
                 :nan (parse-xnum "abc")
                 :nan (parse-xnum ""))
  (assert (< (abs (- 2.3d2 (parse-xnum "  2.3e+2  "))) 1e-7))  ;; FIXME
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
