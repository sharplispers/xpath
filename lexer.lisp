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

;;; Define a function called NAME with one argument (the XPath expression as a
;;; string), which returns a lexer closure suitable for use with cl-yacc.
;;;
;;;   - The lexer closure takes no arguments and returns the next token name
;;;     and the token value as multiple values, or NIL at the end of the
;;;     string.
;;;
;;;   - Each rule is of the form (regex (&rest arguments) &rest body)
;;;     BODY must return token name and value as multiple values, unless
;;;     body contains only a single keyword, in which case the keyword
;;;     is used as both token name and value.
;;;
;;;   - As a special exception, a rule may return as multiple values
;;;       (name1 value1 name2 value2)
;;;     In that case, name1 and value1 are returned as normal for this
;;;     invocation of the lexer closure, but the next invocation will
;;;     return name2 and value2 instead of parsing the next token.
;;;
;;;   - The number of arguments must match the number of registers used by
;;;     REGEX.
;;;
;;;   - Magic variables PREV-TOKEN-NAME and PREV-TOKEN-VALUE are bound
;;;     in BODY so that the previous token can be inspected.
;;;
;;;   - There is no implicit register group for the entire rule,
;;;     so make sure to wrap parens around the regex to get such a group
;;;     as an argument.
;;;
;;; Implementation notes:
;;;   - We implement lexing using cl-ppcre by collecting all rules into one
;;;     big regex of the form:
;;;       ^rule1|rule2|rule3|...rulenN
;;;     ruleIII is converted using a named register group as:
;;;        (?<gIII>...)
;;;     We use named register groups because the second return value of
;;;     cl-ppcre:create-scanner then tells us which register groups belongs
;;;     to which rule.
;;;
;;;   - In the lexer closure, we do a linear seach over the groups to find
;;;     the one named register group that actually matched.
;;;
;;;   - FIXME: cl-ppcre doesn't look for the longest match, so the rules
;;;     need to be written so that they check for ambiguous matches manually.
;;;
;;;   - FIXME: Whitespace is currently allowed before and after -every- rule.
;;;     Is this correct?
;;;
(defmacro deflexer (name &body rules)
  (let ((regex-stream (make-string-output-stream)))
    (format regex-stream "^(?:")
    (loop
       for counter from 0
       for (subregex args . body) in rules do
	 (unless (zerop counter)
	   (write-char #\| regex-stream))
	 (format regex-stream "(?<g~D>\\s*~A\\s*)" counter subregex))
    (format regex-stream ")")
    `(let ((actions
	    (vector
	     ,@(loop
		  for counter from 0
		  for (subregex args . body) in rules
		  collect
		  `(cons ,(length args)
			 (lambda (prev-token-name prev-token-value ,@args)
			   (declare (ignorable prev-token-name
					       prev-token-value))
			   ,@(if (and (keywordp (car body))
				      (null (cdr body)))
				 `((values ,(car body) ,(car body)))
				 body)))))))
       (multiple-value-bind (scanner group-numbers)
	   (let ((cl-ppcre:*allow-named-registers* t)
		 (group-table (make-array ,(length rules))))
	     (multiple-value-bind (scanner groups)
		 (cl-ppcre:create-scanner
		  ,(get-output-stream-string regex-stream))
	       (loop
		  for i from 0
		  for group in groups
		  when group do
		    (let ((group-number
			   (parse-integer group
					  :start 1
					  :junk-allowed t)))
		      (setf (elt group-table group-number) i)))
	       (values scanner group-table)))
	 (defun ,name (str)
	   (let ((pos 0)
		 (length (length str))
		 (prev-token-name nil)
		 (prev-token-value nil)
		 (next-token-name nil)
		 (next-token-value nil))
	     (lambda ()
	       (cond
		 (next-token-name
		  (multiple-value-prog1
		      (values next-token-name next-token-value)
		    (setf next-token-name nil)))
		 ((< pos length)
		  (multiple-value-bind
			(total-start total-end group-start group-end)
		      (cl-ppcre:scan scanner str :start pos)
		    (unless total-start
		      (error "not a well-formed XPath expression: ~
                             lexer failed at position ~D" pos))
		    (setf pos total-end)
		    (loop
		       for group-number across group-numbers
		       for (nargs . action) across actions
		       for start = (elt group-start group-number)
		       when start do
			 (multiple-value-bind
			       (token-name token-value extra-name extra-value)
			     (apply action
				    prev-token-name
				    prev-token-value
				    (loop
				       for i from (1+ group-number)
				       repeat nargs
				       collect
				       (let ((astart (elt group-start i)))
					 (and astart
					      (subseq str
						      astart
						      (elt group-end i))))))
			   (setf prev-token-name token-name
				 prev-token-value token-value)
			   (setf next-token-name extra-name
				 next-token-value extra-value)
			   (return (values token-name token-value)))
		       finally
			 (error "not a well-formed XPath expression: ~
                             no token rule matched at ~D" pos))))
		 (t nil)))))))))

#|
3.7 Lexical Structure

When tokenizing, the longest possible token is always returned.

For readability, whitespace may be used in expressions even though not explicitly allowed by the grammar: ExprWhitespace may be freely added within patterns before or after any ExprToken.

The following special tokenization rules must be applied in the order specified to disambiguate the ExprToken grammar:

    *

      If there is a preceding token and the preceding token is not one of @, ::, (, [, , or an Operator, then a * must be recognized as a MultiplyOperator and an NCName must be recognized as an OperatorName.
    *

      If the character following an NCName (possibly after intervening ExprWhitespace) is (, then the token must be recognized as a NodeType or a FunctionName.
    *

      If the two characters following an NCName (possibly after intervening ExprWhitespace) are ::, then the token must be recognized as an AxisName.
    *

      Otherwise, the token must not be recognized as a MultiplyOperator, an OperatorName, a NodeType, a FunctionName, or an AxisName.

Expression Lexical Structure
[28]    ExprToken          ::=          '(' | ')' | '[' | ']' | '.' | '..' | '@' | ',' | '::'   
                        | NameTest      
                        | NodeType      
                        | Operator      
                        | FunctionName  
                        | AxisName      
                        | Literal       
                        | Number        
                        | VariableReference     
[29]    Literal    ::=          '"' [^"]* '"'   
                        | "'" [^']* "'" 
[30]    Number     ::=          Digits ('.' Digits?)?   
                        | '.' Digits    
[31]    Digits     ::=          [0-9]+  
[32]    Operator           ::=          OperatorName    
                        | MultiplyOperator      
                        | '/' | '//' | '|' | '+' | '-' | '=' | '!=' | '<' | '<=' | '>' | '>='   
[33]    OperatorName       ::=          'and' | 'or' | 'mod' | 'div'    
[34]    MultiplyOperator           ::=          '*'     
[35]    FunctionName       ::=          QName - NodeType        
[36]    VariableReference          ::=          '$' QName       
[37]    NameTest           ::=          '*'     
                        | NCName ':' '*'        
                        | QName 
[38]    NodeType           ::=          'comment'       
                        | 'text'        
                        | 'processing-instruction'      
                        | 'node'        
[39]    ExprWhitespace     ::=          S
|#

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defun nc-name-p (str)
  (and (namep str) (cxml::nc-name-p str)))

(deflexer xpath-lexer
  ("\\(" () :lparen)
  ("\\)" () :rparen)
  ("\\[" () :lbracket)
  ("\\]" () :rbracket)
  ("\\.\\." () :two-dots)
  ("\\@" () :at)
  ("\\," () :comma)
  (":(:)?" (2p) (if 2p :colons :colon))
  ("\\$((?:(?:[\\w-.]+):)?(?:[\\w-.]+))"
   (qname)
   (values :variable qname))
  (#.(apply #'format nil "([^~C-~C~C-~C~C-~C][\\w-.]*)"
	    ;; some checking to make sure the first character looks at least
	    ;; a bit like a NCNameStartChar, so that numbers and operators
	    ;; won't get mistaken for an NCName.
	    (mapcar #'code-char '(#x000 #x40 #x5B #x60 #x7B #xbf)))
   (name)
   (unless (nc-name-p name)
     (error "not an NCName: ~A" name))
   (values :ncname name))
  ("\"([^\"]*)\"" (value) (values :literal value))
  ("'([^']*)'" (value) (values :literal value))
  ("(\\d+(?:\\.\\d*)?|(\\.(\\d*)))"
   (value dot digits)
   (if (and dot (zerop (length digits)))
       :dot
       (values :number (handler-case
			   (parse-number:parse-number value)
			 (org.mapcar.parse-number::invalid-number ()
			   ;; re-signal this condition, because it's not
			   ;; a subclass of error
			   (error "not a well-formed XPath number"))))))
  ("/(/)?" (2p) (if 2p :// :/))
  ("\\|" () :pipe)
  ("\\+" () :+)
  ("-" () :-)
  ("=" () :=)
  ("!=" () :!=)
  ("<(=)?" (=p) (if =p :<= :<))
  (">(=)?" (=p) (if =p :>= :>))
  ("\\*" () (case prev-token-name
              ((:at :two-colons :lparen :lbracket :comma
                    :// :/ :pipe :+ :- := :!= :< :<= :> :>=)
	       (values :star :star))
              (t (values :star-or-multiply :star-or-multiply)))))

;;; After the first lexing step, we need to correct various tokens, because
;;;   - XPath explicitly modifies the lexing result using its disambiguation
;;;     rules.
;;;   - Our DEFLEXER matches the shortest rules, not the longest rule,
;;;     making it inconvenient to distinguish various similar rules
;;;     directly in the lexer.
;;;
;;; To implement all of this, we use a special lexer closure that wraps around
;;; the actual DEFLEXER-generated closure.  The wrapper does a two-token
;;; lookahead and matches the three current tokens against hand-written
;;; rules, which then get a change to replace the current tokens with
;;; disambiguated versions.  If that happens, matching is repeated using
;;; the replaced token until no match can be found.  Finally the first of the
;;; three tokens is returned to the caller.
;;;
(defmacro define-fixup-lexer (name &rest rules)
  `(defun ,name (next-lexer)
     (let (a b c aname bname cname)
       (setf (values bname b) (funcall next-lexer))
       (setf (values cname c) (funcall next-lexer))
       (lambda ()
	 (flet ((shift ()
		  (multiple-value-bind (n v)
		      (funcall next-lexer)
		    (shiftf aname bname cname n)
		    (shiftf a b c v)))
		(ersetze! (name &optional (value name))
		  (setf aname name)
		  (setf a value)
		  t)
		(ersetze!b (name &optional (value name))
		  (setf bname name)
		  (setf b value)
		  t)
		(match (xa &optional (xb nil xbp) (xc nil xcp))
		  (and (or (eq xa t) (eq xa aname))
		       (or (not xbp)
			   (and (or (eq xb t) (eq xb bname))
				(or (not xcp)
				    (or (eq xc t) (eq xc cname))))))))
	   (shift)
	   (cond
	     ((null aname)
	       nil)
	     (t
	      (loop while (cond ,@rules))
	      (values aname a))))))))

(define-fixup-lexer make-fixup-lexer
  ((or (match :ncname :colon :star-or-multiply)
       (match :ncname :colon :star))
   (let ((value a))
     (shift)
     (shift)
     (ersetze! :ns-name value)))

  ((match :ncname :colon :ncname)
   (let ((value (cons a c)))
     (shift)
     (shift)
     (ersetze! :qname value)))

  ((match :ncname :colons)
   (ersetze! :axis-name a))

  ((and (match :ncname)
	(cl-ppcre:all-matches "^(and|or|mod|div)$" a))
   (let ((sym (intern (string-upcase a) :keyword)))
     (ersetze! sym sym)))

  ((and (match :ncname :lparen)
	(cl-ppcre:all-matches "^comment|text|processing-instruction|node$"
			      a))
   (let ((sym (intern (string-upcase a) :keyword)))
     (if (eq sym :processing-instruction)
	 (ersetze! :processing-instruction sym)
	 (ersetze! :node-type-or-function-name a))))

  ((match :ncname :lparen)
   (ersetze! :function-name a))

  ((and (match t :star-or-multiply)
	(find aname '(:at :colons :lparen :lbracket
		      :div :mod :and :or :multiply
		      :/ :// :pipe :+ :- := :!= :< :<= :> :>=)))
   (ersetze!b :star))

  ((match t :star-or-multiply)
   (ersetze!b :multiply))

  ((match :star-or-multiply)
   (ersetze! :star)))


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
  (:muffle-conflicts (7 0))

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
	      (union-expr :pipe path-expr
			  (lambda* (a nil d) `(union ,a ,d))))

  (path-expr location-path
	     (filter-expr
	      ;; FIXME: is this right?
	      (lambda* (a)
		(destructuring-bind (expr predicate) (cdr a)
		  (if (eq predicate t)
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
		(lambda (a) `(:filter ,a t)))
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
  (yacc:parse-with-lexer (make-fixup-lexer (xpath-lexer str))
			 *xpath-parser*))


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
