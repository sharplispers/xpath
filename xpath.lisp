(in-package :xpath)

;; XPath compilation core

;; location path assembly

;; returns function: node -> pipe
(defun make-location-step (axis node-test predicate)
  (assert (axis-function axis) () "unknown axis: ~s" axis)
  #'(lambda (node)
      (let* ((main-pipe (filter-pipe #'(lambda (node)
                                         (funcall node-test node
                                                  (axis-principal-node-type axis)))
                                     (funcall (axis-function axis) node)))
             (context (make-context nil #'(lambda () (pipe-length main-pipe)) 0)))
        (filter-pipe #'(lambda (cur-node)
                         (setf (context-node context) cur-node)
                         (incf (context-position context))
                         (let ((pred-result (funcall predicate context)))
                           (if (xnum-p pred-result)
                               (= (context-position context) pred-result)
                               (boolean-value pred-result))))
                     main-pipe))))

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

(defun compile-xpath (expr)
  (cond ((atom expr) (xf-value expr))
        ((eq (first expr) :path)
         (compile-path (rest expr)))
        (t
         (funcall (or (get (first expr) 'xpath-function)
                      (error "no such function: ~s" expr))
                  (mapcar #'compile-xpath (rest expr))))))

(defun compile-node-test (node-test)
  (if (stringp node-test)
      (node-test-name node-test)
      (case node-test
        (* (node-test-principal))
        (:node (node-test-node))
        (:text (node-test-text-node))
        (:processing-instruction (node-test-processing-instruction))
        (:comment (node-test-comment)))))

(defun compile-location-step (step-spec)
  (destructuring-bind (axis node-test &optional predicate) step-spec
      (make-location-step axis
                          (compile-node-test node-test)
                          (if predicate (compile-xpath predicate) (xf-true)))))

(defun compile-path (path)
  (xf-location-path
   (make-location-path
    (mapcar #'compile-location-step path))))
