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
