;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007,2008 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007,2008 David Lichteblau. All rights reserved.

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


;;;; API

(defparameter *allow-variables-in-patterns* t
  "If set to T, predicates in patterns are permitted to reference variables
   using $var syntax.  If set to NIL, such variable references signal a
   compilation-time error.  The default is T.  Bind this variable to NIL to
   enable compatibility with XSLT 1.0.
   @see{compute-patterns}")

(defstruct (pattern
             (:constructor %make-pattern
                           (key thunk priority value expression)))
  priority
  value
  key
  thunk
  ;; for profiler output only:
  expression)

(setf (documentation 'pattern 'type)
      "Represents a parsed XSLT pattern.
       @see-constructor{compute-patterns}
       @see-slot{pattern-value}
       @see-slot{pattern-priority}
       @see{make-pattern-matcher}")

(setf (documentation 'pattern-value 'function)
      "@arg[instance]{a @class{pattern}}
       @return{an object}
       Return the user-specified value that will be returned by pattern
       matchers if this pattern matches.
       @see{matching-value}
       @see{matching-values}")

(setf (documentation 'pattern-priority 'function)
      "@arg[instance]{a @class{pattern}}
       @return{an integer}
       @short{Return the priority of this pattern.}
       When several patters would match the same node, the pattern matcher
       will only consider the patterns that have the highest priority.
       @see{matching-value}
       @see{matching-values}")

(defun matching-values (matcher node)
  "@arg[matcher]{a pattern matching function}
   @arg[node]{any node implementing the XPath protocol}
   @return{an object}
   @short{Apply a pattern matcher to node, and return one or more matching
   values.}

   For use with @code{matcher} functions that have been returned by
   @fun{make-pattern-matcher} or a higher-level function like
   @fun{make-pattern-matcher*}.

   The resulting list will contain the user-specified values as
   returned by @fun{pattern-value} on the patterns for this matcher, in
   any order.   Duplicates under @code{eql} will have been removed from
   the list.

   @see{node-matches-p}
   @see{pattern-case}
   @see{pattern-ecase}"
  (with-float-traps-masked ()
    (funcall matcher node)))

(defun matching-value (matcher node &optional (default nil))
  "@arg[matcher]{a pattern matching function}
   @arg[node]{any node implementing the XPath protocol}
   @arg[default]{an object}
   @return{an object}
   @short{Apply a pattern matcher to node, and return exactly one value.}

   For use with @code{matcher} functions that have been returned by
   @fun{make-pattern-matcher} or a higher-level function like
   @fun{make-pattern-matcher*}.

   If exactly one pattern matches, or several patterns for the same value
   match, the user-specified values as determined by @fun{pattern-value}
   will be returned by this function.

   If no pattern matches, @code{default} will be returned instead.

   If more than one pattern of highest priority and different values
   match, an @code{xpath-error} will be signalled.

   @see{node-matches-p}
   @see{pattern-case}
   @see{pattern-ecase}"
  (let ((matching-values (matching-values matcher node)))
    (cond
      ((null matching-values)
       default)
      ((null (cdr matching-values))
       (car matching-values))
      (t
       (xpath-error "conflicting patterns")))))

(defun parse-pattern-expression (str)
  "@arg[str]{a string}
   @return{a s-expression-based pattern expression}
   Parses an XSLT pattern into an s-expression."
  (let ((form (parse-xpath str)))
    (unless (consp form)
      (xpath-error "not a valid pattern: ~A" str))
    (labels ((process-inner-form (form)
               (cond ((not (or (eq (car form) :path)
                               (and (eq (car form) :filter)
                                    (let ((filter (second form)))
                                      (and (consp filter)
                                           (member (car filter)
                                                   '(:key :id))))
                                    (equal (third form) '(:true)))
                               (member (car form) '(:key :id))))
                      (xpath-error "not a valid pattern: ~A ~A" str form))
                     ((not (valid-expression-p form))
                      (xpath-error "invalid pattern filter"))
                     (t (list form))))
             (process-form (form)
               (if (eq (car form) :union)
                   (mapcan #'process-inner-form (rest form))
                   (process-inner-form form))))
      (cons :patterns (process-form form)))))

(defun make-pattern-matcher* (expression environment)
  "@arg[expression]{a string or s-expression}
   @arg[environment]{an @code{environment}}
   @return{the pattern matcher, a function}
   @short{Create a pattern matcher for a single pattern.}

   This function is a convenience wrapper around @fun{compute-patterns}
   and @fun{make-pattern-matcher}.

   The resulting matcher will return T if the specified @code{expression}
   matches, or NIL if it doesn't.

   @see{compute-patterns}
   @see{matching-value}
   @see{matching-values}"
  (make-pattern-matcher (compute-patterns expression 42 t environment)))

(defun make-pattern-matcher (patterns)
  "@arg[patterns]{a list of @class{pattern}s}
   @return{the pattern matcher, a function}
   @short{Create a pattern matcher that distinguishes between
     multiple patterns.}

   This function combines several patterns, and creates a matcher function
   for use with @fun{matching-value} or @fun{matching-values}.
   The matcher function will compare a node against each pattern, and
   find the highest-priority pattern or patterns that match the node.

   @see{compute-patterns}"
  (let ((name-patterns (make-hash-table :test 'equal))
        (namespace-patterns (make-hash-table :test 'equal))
        (type-patterns (make-hash-table))
        (other-patterns '()))
    (loop
       for cp in patterns
       for key = (pattern-key cp)
       for type = (car key)
       for name = (cdr key)
       for spec = (list (pattern-priority cp)
                        (pattern-thunk cp)
                        (pattern-value cp))
       do
         (cond
           (name
            (if (car name)
                (push spec (gethash name name-patterns))
                (push spec (gethash (cdr name) namespace-patterns))))
           (type
            (push spec (gethash type type-patterns)))
           (t
            (push spec other-patterns))))
    (maybe-wrap-profiling
     patterns
     (lambda (node)
       (let ((results nil)
             (result-priority nil))
         (flet ((process-spec (spec)
                  (destructuring-bind (priority thunk value)
                      spec
                    (when (and (or (null result-priority)
                                   (<= result-priority priority))
                               (funcall thunk node))
                      (cond
                        ((null result-priority)
                         (setf result-priority priority))
                        ((< result-priority priority)
                         (setf result-priority priority)
                         (setf results nil)))
                      (pushnew value results)))))
           (let ((name (xpath-protocol:local-name node))
                 (uri (xpath-protocol:namespace-uri node))
                 (type (node-type node)))
             (when name
               (mapc #'process-spec (gethash (cons name uri) name-patterns))
               (mapc #'process-spec (gethash uri namespace-patterns)))
             (mapc #'process-spec (gethash type type-patterns))
             (mapc #'process-spec other-patterns)))
         results)))))

(defun compute-patterns (expression priority value environment)
  "@arg[expression]{a string or s-expression}
   @arg[priority]{an integer}
   @arg[value]{an object}
   @arg[environment]{an @code{environment}}
   @return{a list of @class{pattern}s}
   @short{Parse an XSLT pattern expression into one or more pattern objects.}

   Parses an expression, resolves its namespace-, variable-, and
   function-references using the specified @code{environment}, and creates
   a @class{pattern} object for the expression (if it does not use a union)
   or one @class{pattern} object for each sub-expression that is being
   joined into the union.

   The specified @code{priority} is used as the @fun{pattern-priority},
   and the specified @code{value} is used as the @fun{pattern-value}.

   @see{make-pattern-matcher*}
   @see{make-pattern-matcher}"
  (multiple-value-bind (keys thunks subexpressions)
      (compile-pattern-expression expression environment)
    (mapcar (lambda (key thunk subexpression)
              (%make-pattern key thunk priority value subexpression))
            keys
            thunks
            subexpressions)))

(defun node-matches-p (node pattern-expression)
  "@arg[node]{any node implementing the XPath protocol}
   @arg[pattern-expression]{a string or s-expression}
   @return{a boolean}
   @short{Determine whether @code{node} matches the pattern expression.}

   The expression is compiled using the dynamic environment.

   @see{with-namespaces}
   @see{with-variables}
   @see{pattern-case}
   @see{pattern-ecase}"
  (matching-value
   (if (functionp pattern-expression)
       pattern-expression
       (make-pattern-matcher* pattern-expression
                              (make-dynamic-environment *dynamic-namespaces*)))
   node))

(define-compiler-macro node-matches-p (node pattern)
  (once-only (pattern)
    `(matching-value
      (with-cache ((,pattern)
                   (*dynamic-namespaces* :test namespaces-match-p)
                   (*profiling-enabled-p* :test eql))
        (make-pattern-matcher*
         ,pattern
         (make-dynamic-environment *dynamic-namespaces*)))
      ,node)))

(defmacro pattern-ecase (node &rest clauses)
  "@arg[node]{any node implementing the XPath protocol}
   @arg[clauses]{cases of the form (expression &rest body)}
   @return{The value returned by the matching clause body.}
   @short{Match a node against static expressions.}

   Evaluates @code{node}, and matches them against the specified XSLT
   patterns. The first matching pattern will be chosen, i.e. earlier
   clauses have higher priority that later clauses.

   Expressions are compiled using the dynamic environment.

   If no clause matches, an error will be signalled.

   @see{with-namespaces}
   @see{pattern-case}
   @see{node-matches-p}
   @see{with-variables}"
  `(pattern-case ,node
     ,@clauses
     (t (error "pattern-ecase fell through"))))

(defmacro pattern-case (node &body clauses)
  "@arg[node]{any node implementing the XPath protocol}
   @arg[clauses]{cases of the form (expression &rest body)}
   @return{The value returned by the matching clause body, or nil.}
   @short{Match a node against static expressions.}

   Evaluates @code{node}, and matches them against the specified XSLT
   patterns. The first matching pattern will be chosen, i.e. earlier
   clauses have higher priority that later clauses.

   Expressions are compiled using the dynamic environment.

   As a special case, the last expression can be @code{t}, in which case
   it matches unconditionally.

   @see{with-namespaces}
   @see{pattern-ecase}
   @see{node-matches-p}
   @see{with-variables}"
  (let* ((otherwise-body nil)
         (patterns
          (loop
             for priority downfrom 0
             for ((expression . body) . rest) on clauses
             if (eq expression t)
             do
               (if rest
                   (xpath-error "t clause not at end of pattern-case")
                   (setf otherwise-body body))
             else collect
               `(compute-patterns ,expression
                                  ,priority
                                  (lambda () ,@body)
                                  (make-dynamic-environment
                                   *dynamic-namespaces*)))))
    `(funcall
      (matching-value
       (with-cache ((*dynamic-namespaces* :test namespaces-match-p)
                    (*profiling-enabled-p* :test eql))
         (make-pattern-matcher (append ,@patterns)))
       ,node
       (lambda () ,@otherwise-body)))))


;;;; Implementation

(deftype pattern-expr ()
  '(or string (cons (eql :patterns) (cons t null))))

(defun valid-expression-p (expr)
  (cond
    ((atom expr) t)
    ((eq (first expr) :path)
     (every (lambda (x)
              (let ((filter (third x)))
                (or (null filter) (valid-expression-p filter))))
            (cdr expr)))
    ((eq (first expr) :variable)
     *allow-variables-in-patterns*)
    (t
     (every #'valid-expression-p (cdr expr)))))

(defun compile-pattern-expression (pattern environment)
  (unless (typep pattern 'pattern-expr)
    (xpath-error "invalid pattern designator: ~A" pattern))
  (compile-pattern-expression/sexpr (if (stringp pattern)
                                        (parse-pattern-expression pattern)
                                        pattern)
                                    environment))

(defun step-key (step environment)
  (destructuring-bind (axis test &rest predicates)
      step
    (declare (ignore predicates))
    (let* ((type (axis-principal-node-type axis))
           (attributep (eq type :attribute))
           (name
            (etypecase test
              (string
               (multiple-value-bind (local-name uri)
                   (decode-qname test environment attributep)
                 (cons local-name uri)))
              (list
               (ecase (first test)
                 (:processing-instruction
                  (setf type :processing-instruction)
                  nil)
                 (:namespace
                  (let ((prefix (second test)))
                    (cons nil (find-namespace prefix environment attributep))))
                 (:qname
                  ;; This case is just an alternative to the string case for
                  ;; the convenience of callers that have a split name already.
                  (destructuring-bind (prefix local-name) (rest test)
                    (let ((uri (find-namespace prefix environment attributep)))
                      (cons local-name uri))))))
              (t
               (ecase test
                 (* nil)
                 (:node
                  (setf type nil)
                  nil)
                 (:text
                  (setf type :text)
                  nil)
                 (:processing-instruction
                  (setf type :processing-instruction)
                  nil)
                 (:comment
                  (setf type :comment)
                  nil))))))
      (cons type name))))

(defun compile-pattern-expression/sexpr (pattern environment)
  (assert (eq (car pattern) :patterns))
  (values
   (mapcar (lambda (x) (subpattern-key x environment)) (cdr pattern))
   (mapcar (lambda (x) (compile-subpattern x environment)) (cdr pattern))
   (cdr pattern)))

(defun subpattern-key (subpattern environment)
  (ecase (car subpattern)
    ((:path :filter)
     (step-key (car (last subpattern)) environment))
    ((:key :id) nil)))

(defun %compile-subpattern (subpattern environment)
  (ecase (car subpattern)
    (:path
     (let ((steps (cdr subpattern)))
       (if (eq (caar steps) :root)
           (values (lambda (node)
                     (xpath-protocol:node-type-p node :document))
                   (cdr steps))
           (values (constantly t) steps))))
    ((:key :id)
     (%compile-subpattern `(:filter ,subpattern (:true)) environment))
    (:filter
     (destructuring-bind (filter predicate &rest steps) (rest subpattern)
       (let* ((filter-thunk (compile-xpath/sexpr filter environment))
              (predicate-thunk
               (compile-predicates (list predicate) environment)))
         (values (lambda (starting-node)
                   (let ((initial-node-set
                          (funcall filter-thunk (make-context starting-node))))
                     (unless (typep initial-node-set 'node-set)
                       (xpath-error "not a node set: ~A" initial-node-set))
                     (find-in-pipe starting-node
                                   (funcall predicate-thunk
                                            (sorted-pipe-of initial-node-set)
                                            starting-node)
                                   :test #'xpath-protocol:node-equal)))
                 (nth-value 1 (%compile-subpattern (cons :path steps)
                                                   environment))))))))

(defun compile-subpattern (subpattern environment)
  (multiple-value-bind (starting-node-test steps)
      (%compile-subpattern subpattern environment)
    (make-reverse-location-path
     starting-node-test
     (mapcar (lambda (step)
               (make-reverse-location-step step environment))
             steps)
     (mapcar (lambda (step)
               (make-reverse-predicate-check step environment))
             steps))))

;; Example: Consider
;;   (make-reverse-location-step '(:child "bar") env)
;; with the document
;;   <foo> <bar> <baz/> </bar> </foo>
;;
;; Then a call to the closure with the path
;;   (#<element node for bar>
;;    #<element node for baz>)
;; will return a list of one path, with the next parent prepended:
;;   ((#<element node for foo>
;;     #<element node for bar>
;;     #<element node for baz>))
;;
;; But a call to the closure with the path
;;   (#<any-other-element>)
;; will return
;;   ()
;; indicating failure.
;;
;; More than one path can be returned if the descendant-or-self axis is used,
;; one for each ancestor.
;;
(defun make-reverse-location-step (step environment)
  (destructuring-bind (axis node-test &rest predicates)
      step
    (declare (ignore predicates))
    (let ((principal-node-type (axis-principal-node-type axis))
          (node-test-thunk-1
           (compile-node-test node-test environment (eq axis :attribute)))
          (node-test-thunk-2
           (case axis
             (:attribute
              (lambda (node)
                (xpath-protocol:node-type-p node :attribute)))
             ((:child :descendant-or-self)
              (lambda (node)
                (not (xpath-protocol:node-type-p node :attribute))))
             (t
              (xpath-error "invalid as a pattern axis: ~A" axis))))
          (upstep
           (case axis
             ((or :child :attribute)
              (lambda (current-node rest-of-path)
                (let ((parent (xpath-protocol:parent-node current-node)))
                  (if parent
                      (list (list* parent current-node rest-of-path))
                      nil))))
             (:descendant-or-self
              (lambda (current-node rest-of-path)
                (loop
                   for parent = current-node
                   then (xpath-protocol:parent-node parent)
                   while parent
                   collect (list* parent
                                  current-node
                                  rest-of-path))))
             (t
              (xpath-error "invalid as a pattern axis: ~A" axis)))))
      (lambda (current-path)
        (destructuring-bind (current-node &rest rest-of-path)
            current-path
          (if (and (funcall node-test-thunk-1 current-node principal-node-type)
                   (funcall node-test-thunk-2 current-node))
              (funcall upstep current-node rest-of-path)
              nil))))))

;; FIXME: optimize this for the case where there aren't any predicates.
(defun make-reverse-predicate-check (step environment)
  (destructuring-bind (axis node-test &rest predicates)
      step
    (if predicates
        (let* ((node-test-fun
                (compile-node-test node-test environment (eq axis :attribute)))
               (thunk
                (make-location-step axis node-test-fun predicates environment)))
          (lambda (parent node starting-node)
            (and (find-in-pipe node
                               (funcall thunk parent starting-node)
                               :test #'xpath-protocol:node-equal)
                 t)))
        (constantly t))))

(defun make-reverse-location-path (starting-node-check steps predicate-checks)
  ;; - Run from back to front, computing "paths" of nodes, with one node
  ;;   for each step in the location path (not including parents that the
  ;;   location path doesn't match on).
  ;;
  ;;   This list can have more than one path only if // is used, and multiple
  ;;   parents might be candidates for the starting point.
  ;;
  ;;   Predicates are ignored.
  ;;
  ;; - In a second step, check each such path to see if the predicates are
  ;;   a match by working front the front to the back as usual.
  ;;
  ;;   We can do this only after having determined the starting node,
  ;;   because a predicate might use current().
  ;;
  ;;   front-to-back is similar to `make-location-path', but doesn't
  ;;   have to follow descendent-or-self fully, and doesn't accumulate a full
  ;;   list of nodes.
  (labels ((back-to-front (steps)
             (if steps
                 (let ((head-thunk (first steps))
                       (rest-thunk (back-to-front (rest steps))))
                   (lambda (target-nodes)
                     (loop
                        for tail in (funcall rest-thunk target-nodes)
                        nconc (funcall head-thunk tail))))
                 (lambda (target-nodes)
                   (mapcar #'list target-nodes))))
           (front-to-back (checks)
             (if checks
                 (let ((check (first checks))
                       (rest-thunk (front-to-back (rest checks))))
                   (lambda (parent path starting-node)
                     (let ((node (car path)))
                       (and (funcall check parent node starting-node)
                            (funcall rest-thunk
                                     node
                                     (cdr path)
                                     starting-node)))))
                 (constantly t))))
    (let ((btf-thunk (back-to-front steps))
          (ftb-thunk (front-to-back predicate-checks)))
      (lambda (node)
        (some (lambda (path)
                (destructuring-bind (starting-node &rest child-path)
                    path
                  (and (funcall starting-node-check starting-node)
                       (funcall ftb-thunk
                                starting-node
                                child-path
                                starting-node))))
              (funcall btf-thunk (list node)))))))

;;; FIXME: replace xpath-protocol:node-type-p with a new generic function
;;; xpath-protocol:node-type then implement node-type-p as an ordinary
;;; function on top of that.
(defun node-type (node)
  (dolist (type '(:element
                  :attribute
                  :text
                  :document
                  :namespace
                  :processing-instruction
                  :comment))
    (when (xpath-protocol:node-type-p node type)
      (return type))))
