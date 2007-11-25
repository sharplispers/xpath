(in-package :xpath)

;; types

;; * node-set (an unordered collection of nodes without duplicates) --> node-set
;; * boolean (true or false) --> nil / t ("list" is used in defgeneric)
;; * number (a floating-point number) --> xnum
;; * string (a sequence of UCS characters) --> string

(defclass node-set ()
  ((pipe :accessor pipe-of :initform empty-pipe :initarg :pipe)))

(defun node-set-p (object) (typep object 'node-set))

(defun make-node-set (pipe) (make-instance 'node-set :pipe pipe))

;; equality

(defun compare-node-sets (op a b) ;; FIXME: may be inefficient in some cases
  (if (eq op 'equal)
      (let ((table (make-hash-table :test #'equal)))
        (block nil
          (enumerate (pipe-of a) :key #'(lambda (item) (setf (gethash (get-node-text item) table) t)))
          (enumerate (pipe-of b) :key #'(lambda (item) (when (gethash (get-node-text item) table) (return t))))
          nil))
      (block nil
        (enumerate (pipe-of a) ;; FIXME: use min/max finding or something for <, >, <=, >=
                   :key #'(lambda (x)
                            (let ((x (number-value (get-node-text x))))
                              (enumerate (pipe-of b)
                                         :key #'(lambda (y)
                                                  (when (funcall op x (number-value (get-node-text y)))
                                                    (return t)))))))
        nil)))

(defun compare-with-node-set (op node-set value)
  (block nil
    (enumerate (pipe-of node-set)
               :key #'(lambda (node)
                        (when (compare/no-node-sets op (get-node-text node) value)
                          (return t))))
    nil))

(defun compare/no-node-sets (op a b)
  (cond ((or (not (eq op 'equal))
             (xnum-p a)
             (xnum-p b))
         (compare-numbers op (number-value a) (number-value b))) ;; FIXME: NaN
        ((or (typep a 'boolean) (typep b 'boolean))
         (equal (boolean-value a) (boolean-value b)))
        (t
         (string= (string-value a) (string-value b)))))

(defun compare-values (op a b)
  (cond ((and (node-set-p a) (node-set-p b))
         (compare-node-sets op a b))
        ((node-set-p a)
         (compare-with-node-set op a b))
        ((node-set-p b)
         (compare-with-node-set op b a))
        (t (compare/no-node-sets op a b))))

(defun boolean-value (value)
  (typecase value
    (string (not (equal value "")))
    (xnum (not (or (nan-p value)
                   (x-zerop value))))
    (node-set (not (pipe-empty-p (pipe-of value))))
    (t (if value t nil))))

(defun number-value (value)
  (typecase value
    (string (parse-xnum value)) ;; FIXME!!!! it should be double-float; how to handle junk? NaN?
    (xnum value)
    (node-set (number-value (string-value value)))
    (t (if value 1 0))))

(defun string-value (value)
  (typecase value
    (string value)
    (xnum (xnum->string value)) ;; fixme; probably also should use format string
    (node-set
     (if (pipe-empty-p (pipe-of value))
         ""
         (get-node-text (pipe-head (pipe-of value)))))
    (t (if value "true" "false"))))

(defun node-set-value (value)
  (if (node-set-p value)
      value
      (error "cannot convert ~s to a NODE-SET" value)))

;; context

(defstruct (context
	    (:constructor make-context (node &optional (size/delayed 1) (position 1))))
  node size/delayed position)

(defun context-size (context)
  (symbol-macrolet ((size/delayed (context-size/delayed context)))
    (if (functionp size/delayed)
        (setf size/delayed (funcall size/delayed))
        size/delayed)))

(defgeneric context-variable-value (context local-name uri))


;; environment
;;
;; (a compilation-time context)

(defstruct environment)

(defgeneric environment-find-namespace (environment prefix))
(defgeneric environment-find-function (environment local-name uri))
(defgeneric environment-validate-variable (environment local-name uri))


;; test environment
;;
;; (An environment that pretends to know about every namespace, function,
;; and variable. For use only in parser tests.)

(defstruct (test-environment (:include environment)))

(defmethod environment-find-namespace ((environment test-environment) prefix)
  (if (zerop (length prefix))
      ""
      (concatenate 'string "dummy://" prefix)))

(defmethod environment-find-function ((environment test-environment) lname uri)
  #'(lambda (&rest args)
      (error "function ~A, ~A was called with args ~A" lname uri args)))

(defmethod environment-validate-variable
    ((environment test-environment) lname uri)
  (declare (ignore lname uri))
  t)
