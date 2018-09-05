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

(in-package :xpath-test)

(defun test-lexer (str)
  (let* ((l '())
         (m '())
         (g (xpath-lexer str))
         (fn (make-fixup-lexer
              (lambda ()
                (multiple-value-bind (a b)
                    (funcall g)
                  (push (cons a b) l)
                  (values a b))))))
    (loop
      (multiple-value-bind (a b) (funcall fn)
        (unless a (return))
        (push (cons a b) m)))
    (setf l (nreverse l))
    (setf m (nreverse m))
    (loop
      for (a . b) = (pop l)
      for (c . d) = (pop m)
      while (or a b)
      do
         (format t "~:[*~; ~] ~S~,10T ~S~,30T ~S~,10T ~S~%"
                 (eq a c) a b c d))))

(defun dribble-tests ()
  (with-open-file (log "/home/david/src/lisp/xpath/TESTS"
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf8)
    (let ((test-cases
            (with-open-file (s "/home/david/src/lisp/xuriella/XPATH"
                               :external-format :utf8)
              (read s)))
          (npass 0)
          (env (make-test-environment)))
      (loop
        for str in test-cases
        for i from 0
        do
           (handler-case
               (let ((form (parse-xpath str)))
                 (handler-case
                     (progn
                       (compile-xpath form env)
                       (incf npass)
                       (format log "~D PASS~%" i))
                   (error (c)
                     (format log "~D FAIL (compile): ~A~%  ~A~%" i str c))))
             (error (c)
               (format log "~D FAIL (parse): ~A~%  ~A~%" i str c))))
      (format log "Passed ~D/~D tests.~%" npass (length test-cases)))))
