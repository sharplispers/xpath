(in-package :xpath)

;; XPath compilation core

;; location path assembly

;; returns function: node -> pipe
(defun make-location-step (axis node-test predicates)
  (assert (axis-function axis) () "unknown axis: ~s" axis)
  (let ((predicate-closure (compile-predicates predicates)))
    #'(lambda (node)
	(funcall predicate-closure
		 (filter-pipe #'(lambda (node)
				  (funcall node-test node
					   (axis-principal-node-type axis)))
			      (funcall (axis-function axis) node))))))

(defun compile-predicates (predicates)
  (if predicates
      (let ((predicate (car predicates))
	    (next (compile-predicates (rest predicates))))
	#'(lambda (main-pipe)
	    (let ((context (make-context nil
					 #'(lambda () (pipe-length main-pipe))
					 0)))
	      (funcall next
		       (filter-pipe
			#'(lambda (cur-node)
			    (setf (context-node context) cur-node)
			    (incf (context-position context))
			    (let ((pred-result (funcall predicate context)))
			      (if (xnum-p pred-result)
				  (= (context-position context) pred-result)
				  (boolean-value pred-result))))
			main-pipe)))))
      #'identity))

;; returns function: node -> pipe
(defun make-location-path (steps)
  (cond ((null steps) (error "make-location-path: no steps"))
        ((null (rest steps)) (first steps))
        (t
         (let ((first-of-path (first steps))
               (rest-of-path (make-location-path (rest steps))))
           #'(lambda (node)
               (mappend-pipe rest-of-path
                             (funcall first-of-path node)))))))

;; most basic primitive functions

(defun xf-value (x) (constantly x))

(defun xf-true () (xf-value t))

(defun xf-location-path (path)
  #'(lambda (context)
      (make-node-set
       (force
        (funcall path (context-node context))))))

;; compilation

(defmacro define-xpath-function/lazy (name args &body body)
  (with-gensyms (thunks)
    (let ((func-name (hypsym 'xfd name)))
      `(progn
         (defun ,func-name (,thunks)
           (declare (ignorable ,thunks))
           ,(if (null args)
                `(locally ,@body)
                `(destructuring-bind ,args ,thunks ,@body)))
         (setf (get ',name 'xpath-function) ',func-name)))))

(defmacro %define-xpath-function/eager (name converter args &body body)
  (with-gensyms (thunks)
    (let ((func-name (hypsym 'xfd name)))
      `(progn
         (defun ,func-name (,thunks)
           (declare (ignorable ,thunks))
           #'(lambda (context)
               (declare (ignorable context))
               ,(if (null args)
                    `(locally ,@body)
                    `(destructuring-bind ,args (mapcar ,converter ,thunks)
                       ,@body))))
         (setf (get ',name 'xpath-function) ',func-name)))))

(defmacro define-xpath-function/eager (name args &body body)
  (with-gensyms (thunk)
    `(%define-xpath-function/eager ,name
         #'(lambda (,thunk) (funcall ,thunk context))
         ,args ,@body)))

(defmacro define-xpath-function/single-type (name type args &body body)
  (check-type type (member boolean number string node-set))
  (with-gensyms (thunk)
    (let ((value-func-name (hypsym type 'value)))
      `(%define-xpath-function/eager ,name
           #'(lambda (,thunk) (,value-func-name (funcall ,thunk context)))
           ,args ,@body))))

(defun decode-qname (qname environment attributep)
  (multiple-value-bind (prefix local-name)
      (cxml::split-qname qname)
    (values local-name (find-namespace prefix environment attributep))))

(defun find-namespace (prefix environment attributep)
  (if (or prefix (not attributep))
      (or (environment-find-namespace environment prefix)
	  (error "undeclared namespace: ~A" prefix))
      ""))

(defun compile-xpath (expr environment)
  (cond ((atom expr) (xf-value expr))
        ((eq (first expr) :path)
         (compile-path (rest expr) environment))
        ((eq (first expr) :filter)
	 (destructuring-bind (filter predicate &rest steps) (rest expr)
	   (compile-filter-path filter predicate steps environment)))
        ((eq (first expr) :variable)
         (compile-variable (second expr) environment))
        (t
         (let ((name (first expr))
	       (thunks
		(mapcar #'(lambda (e) (compile-xpath e environment))
			(rest expr))))
	   (etypecase name
	     (symbol
	      (funcall (or (get name 'xpath-function)
			   (error "no such function: ~s" expr))
		       thunks))
	     (string
	      (multiple-value-bind (local-name uri)
		  (decode-qname name environment nil)
		(let ((fun (environment-find-function environment
						      local-name
						      uri)))
		  #'(lambda (context)
		      (apply fun (mapcar (lambda (thunk)
					   (funcall thunk context))
					 thunks)))))))))))

(defun compile-variable (name environment)
  (multiple-value-bind (local-name uri)
      (decode-qname name environment nil)
    (unless (environment-validate-variable environment local-name uri)
      (error "undeclared variable: ~A in namespace ~A" local-name uri))
    (lambda (context)
      (context-variable-value context local-name uri))))

(defun compile-node-test (node-test environment attributep)
  (etypecase node-test
    (string
     (multiple-value-bind (local-name uri)
	 (decode-qname node-test environment attributep)
       (node-test-name local-name uri)))
    (list
     (ecase (first node-test)
       (:processing-instruction
	(node-test-processing-instruction (second node-test)))
       (:namespace
	(let* ((prefix (second node-test))
	       (uri (find-namespace prefix environment attributep)))
	  (node-test-namespace uri)))
       (:qname
	;; This case is just an alternative to the string case for the
	;; convenience of callers that have a split name already.
	(destructuring-bind (prefix local-name) (rest node-test)
	  (let ((uri (find-namespace prefix environment attributep)))
	    (node-test-name local-name uri))))))
    (t
     (case node-test
       (* (node-test-principal))
       (:node (node-test-node))
       (:text (node-test-text-node))
       (:processing-instruction (node-test-processing-instruction))
       (:comment (node-test-comment))))))

(defun compile-location-step (step-spec environment)
  (destructuring-bind (axis node-test &rest predicates) step-spec
      (make-location-step axis
                          (compile-node-test node-test
					     environment
					     (eq axis :attribute))
                          (mapcar #'(lambda (p) (compile-xpath p environment))
				  predicates))))

(defun compile-path (path environment)
  (xf-location-path
   (make-location-path
    (mapcar #'(lambda (step) (compile-location-step step environment)) path))))

;; like compile-path, but with an initial node set that is computed by
;; a user expression `filter' rather than as the current node 
(defun compile-filter-path (filter predicate path environment)
  (let* ((filter-thunk (compile-xpath filter environment))
	 (predicate-thunk (compile-predicates (list predicate)))
	 (steps (mapcar #'(lambda (step)
			    (compile-location-step step environment))
			path))
	 (path-thunk (when steps
		       (make-location-path steps))))
    #'(lambda (context)
	(make-node-set
	 (force
	  (let* ((initial-node-set (funcall filter-thunk context))
		 (good-nodes (funcall predicate-thunk
				      (pipe-of initial-node-set))))
	    (if path-thunk
		(mappend-pipe path-thunk good-nodes)
		good-nodes)))))))

;; public evaluation API

(defun first-node (node-set)
  (pipe-head (pipe-of node-set)))

(defun all-nodes (node-set)
  (force (pipe-of node-set)))

(defmacro xpath (form)
  `(list 'xpath ',form))

(defun evaluate (xpath context)
  (when (typep xpath 'string)
    (setf xpath (list 'xpath (parse-xpath xpath))))
  (when (listp xpath)
    (unless (and (consp xpath) (eq (car xpath) 'xpath) (null (cddr xpath)))
      (error "invalid xpath designator: ~A" xpath))
    (setf xpath (compile-xpath (second xpath)
			       (make-lexical-environment
				*lexical-namespaces*))))
  (unless (functionp xpath)
    (error "invalid xpath designator: ~A" xpath))
  (unless (typep context 'context)
    ;; FIXME: Should this perhaps compute position and size based on 
    ;; the node's siblings instead?
    (setf context (make-context context)))
  (funcall xpath context))

(define-compiler-macro evaluate (&whole whole &environment env xpath context)
  (let ((namespaces (macroexpand '(lexical-namespaces) env)))
    (unless namespaces
      (error "EVALUATE used outside of with-namespaces"))
    (if (or (and (stringp xpath)
		 (not (functionp xpath)))
	    (and (consp xpath) (eq (car xpath) 'xpath)))
	(let ((y (if (typep xpath 'string)
		     (list 'xpath (parse-xpath xpath))
		     xpath)))
	  (unless (and (consp y)
		       (eq (car y) 'xpath)
		       (null (cddr y)))
	    (error "invalid xpath designator: ~A" y))
	  `(evaluate (load-time-value
		      (compile-xpath ',(second y)
				     (make-lexical-environment ',namespaces)))
		     ,context))
	whole)))
