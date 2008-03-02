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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro lambda* ((&rest args) &body body)
    (setf args (mapcar (lambda (arg) (or arg (gensym))) args))
    `(lambda (&rest .args.)
       (unless (equal (length .args.) ,(length args))
         (error "expected ~A, got ~A" ',args .args.))
       (destructuring-bind (,@args) .args.
         (declare (ignorable ,@args))
         ,@body))))

(defun make-function-name (x)
  (cond
    ((symbolp x) x)
    ((consp x) (list :qfunc (car x) (cdr x)))
    ((find #\: x) x)
    (t (intern (string-upcase x) :keyword))))

;;; After the fixup phase, we can parse using a cl-yacc parser for the grammar
;;; as stated in the spec:
(yacc:define-parser *xpath-parser*
  (:start-symbol expr)
  (:terminals (:lparen :rparen :lbracket :rbracket :two-dots :dot
                       :at :comma :colons :colon :variable
                       :ncname :literal :number :// :/ :pipe :+
                       :- := :!= :<= :< :> :>= :star :multiply :ns-name
                       :qname :axis-name :node-type-or-function-name
                       :function-name :and :or :mod :div
                       :processing-instruction))
  #+debug (:print-first-terminals t)
  #+debug (:print-states t)
  #+debug (:print-lookaheads t)
  #+debug (:print-goto-graph t)
  (:muffle-conflicts (8 0))

  ;;;;;;;;;;;; 3.1 ;;;;;;;;;;;;
  ;;
  (expr or-expr)
  (primary-expr (:variable (lambda* (x) `(:variable ,x)))
                (:lparen expr :rparen
                         (lambda* (nil var nil) var))
                :literal
                :number
                function-call)

  ;;;;;;;;;;;; 2 ;;;;;;;;;;;;
  ;;
  (location-path relative-location-path
                 absolute-location-path)

  (absolute-location-path (:/
                           (lambda* (nil) `(:path (:root :node))))
                          (:/ relative-location-path
                              (lambda* (nil a) `(:path (:root :node) ,@(cdr a))))
                          abbreviated-absolute-location-path)

  (relative-location-path
   (step (lambda* (a) `(:path ,a)))
   (relative-location-path :/ step
                           (lambda* (a nil b) `(:path ,@(cdr a) ,b)))
   abbreviated-relative-location-path)

  ;;;;;;;;;;;; 2.1 ;;;;;;;;;;;;
  ;;
  (step (axis-specifier node-test predicates
                        (lambda* (a b c)
                          `(,(intern (string-upcase a) :keyword) ,b ,@c)))
        (axis-specifier node-test
                        (lambda* (a b)
                          `(,(intern (string-upcase a) :keyword) ,b)))
        ;; now the first two cases again without axis, because of the ? in
        ;;   node-test => abbreviated-axis-specifier => '@'?
        (node-test predicates (lambda* (b c) `(:child ,b ,@c)))
        (node-test (lambda* (b) `(:child ,b)))
        abbreviated-step)
  (predicates (predicate)
              (predicate predicates (lambda* (a b) (cons a b))))
  (axis-specifier (:axis-name :colons (lambda* (a nil) a))
                  abbreviated-axis-specifier)

  ;;;;;;;;;;;; 2.3 ;;;;;;;;;;;;
  ;;
  (node-test
   ;; first three cases are for NameTest:
   (:ns-name (lambda* (a) `(:namespace ,a)))
   (:qname (lambda* (a) `(:qname ,(car a) ,(cdr a))))
   :ncname
   (operator-actually-ncname
    (lambda* (a) (string-downcase (symbol-name a))))
   (:star (lambda* (nil) '*))
   (:node-type-or-function-name
    :lparen :rparen
    (lambda* (a nil nil) (intern (string-upcase a) :keyword)))
   (:processing-instruction :lparen :rparen
                            (lambda* (nil nil nil)
                              :processing-instruction))
   (:processing-instruction :lparen :literal :rparen
                            (lambda* (nil nil a nil)
                                     `(:processing-instruction ,a))))
  (operator-actually-ncname :mod :div :and :or)

  ;;;;;;;;;;;; 2.4 ;;;;;;;;;;;;
  ;;
  (predicate (:lbracket predicate-expr :rbracket
                        (lambda* (nil a nil) a)))
  (predicate-expr expr)

  ;;;;;;;;;;;; 2.5 ;;;;;;;;;;;;
  ;;
  (abbreviated-absolute-location-path
   (:// relative-location-path
        (lambda* (nil a) `(:path (:root :node)
                                 (:descendant-or-self :node)
                                 ,@(cdr a)))))
  (abbreviated-relative-location-path
   (relative-location-path :// step
                           (lambda* (a nil b)
                             (append a `((:descendant-or-self :node) ,b)))))
  (abbreviated-step (:dot (lambda* (nil) '(:self :node)))
                    (:two-dots (lambda* (nil) '(:parent :node))))
  (abbreviated-axis-specifier
   (:at (lambda* (nil) :attribute)))

  ;;;;;;;;;;;; 3.2 ;;;;;;;;;;;;
  ;;
  (function-call (:function-name :lparen arguments :rparen
                                 (lambda* (name nil args nil)
                                          `(,(make-function-name name)
                                             ,@args)))
                 (:node-type-or-function-name :lparen arguments :rparen
                                              (lambda* (name nil args nil)
                                                `(,(make-function-name name)
                                                   ,@args)))
                 (:processing-instruction :lparen arguments :rparen
                                          (lambda* (name nil args nil)
                                            `(:processing-instruction
                                              ,@args))))

  (arguments ()
             (expr)
             (expr :comma arguments
                   (lambda* (a nil d) (cons a d))))

  ;;;;;;;;;;;; 3.3 ;;;;;;;;;;;;
  ;;
  (union-expr path-expr
              (path-expr :pipe path-expr
                         (lambda* (a nil d) `(:union ,a ,d)))
              (union-expr :pipe path-expr
                          (lambda* (a nil d) `(,@a ,d))))

  (path-expr location-path
             (filter-expr
              ;; FIXME: is this right?
              (lambda* (a)
                (destructuring-bind (expr predicate) (cdr a)
                  (if (equal predicate '(:true))
                      expr
                      a))))
             (filter-expr :/ relative-location-path
                          (lambda* (a nil d)
                            `(:filter ,@(cdr a) ,@(cdr d))))
             (filter-expr :// relative-location-path
                          (lambda* (a nil d)
                            `(:filter ,@(cdr a)
                                      (:descendant-or-self :node)
                                      ,@(cdr d)))))

  (filter-expr (primary-expr
                (lambda (a) `(:filter ,a (:true))))
               (filter-expr predicate
                            (lambda* (a b) `(:filter ,a ,b))))

  ;;;;;;;;;;;; 3.4 ;;;;;;;;;;;;
  ;;
  (or-expr and-expr
           (or-expr :or and-expr
                    (lambda* (a nil b) `(:or ,a ,b))))

  (and-expr equality-expr
            (and-expr :and equality-expr
                      (lambda* (a nil b) `(:and ,a ,b))))

  (equality-expr relational-expr
                 (equality-expr := relational-expr
                                (lambda* (a nil b) `(= ,a ,b)))
                 (equality-expr :!= relational-expr
                                (lambda* (a nil b) `(/= ,a ,b))))

  (relational-expr additive-expr
                   (relational-expr :< additive-expr
                                    (lambda* (a nil b) `(< ,a ,b)))
                   (relational-expr :<= additive-expr
                                    (lambda* (a nil b) `(<= ,a ,b)))
                   (relational-expr :> additive-expr
                                    (lambda* (a nil b) `(> ,a ,b)))
                   (relational-expr :>= additive-expr
                                    (lambda* (a nil b) `(>= ,a ,b))))

  ;;;;;;;;;;;; 3.5 ;;;;;;;;;;;;
  ;;
  (additive-expr multiplicative-expr
                 (additive-expr :+ multiplicative-expr
                                (lambda* (a nil b) `(+ ,a ,b)))
                 (additive-expr :- multiplicative-expr
                                (lambda* (a nil b) `(- ,a ,b))))

  (multiplicative-expr
   unary-expr
   (multiplicative-expr :multiply unary-expr
                        (lambda* (a op b) `(* ,a ,b)))
   (multiplicative-expr :div unary-expr
                        (lambda* (a nil b) `(/ ,a ,b)))
   (multiplicative-expr :mod unary-expr
                        (lambda* (a nil b) `(mod ,a ,b))))

  (unary-expr union-expr
              (:- unary-expr
                  (lambda* (nil a) `(- ,a)))))


;;;; parser interface

(defun parse-xpath (str)
  "@arg[str]{a string}
   @return{a s-expression-based XPath expression}
   Parses a string-based XPath expression into s-expression-based one."
  (handler-bind
      ((error
        (lambda (c)
          (unless (typep c 'xpath-error)
            (xpath-error "invalid XPath syntax: ~A in: ~A" c str)))))
    (yacc:parse-with-lexer (make-fixup-lexer (xpath-lexer str))
                           *xpath-parser*)))


;;;;

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
