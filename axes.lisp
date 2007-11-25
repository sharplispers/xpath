(in-package :xpath)

;; axes


#|
FIXME: need to implement all axes

[6]   	AxisName	   ::=   	'ancestor'	
			| 'ancestor-or-self'	
			| 'attribute'	
			| 'child'	
			| 'descendant'	
			| 'descendant-or-self'	
			| 'following'	
			| 'following-sibling'	
			| 'namespace'	
			| 'parent'	
			| 'preceding'	
			| 'preceding-sibling'	
			| 'self'	
|#


(defmacro define-axis (name (&optional (principal-node-type :element)) &body body)
  (let ((func-name (hypsym 'axis name)))
    `(progn
       (defun ,func-name (node) ,@body)
       (setf (get ',name 'axis-function) ',func-name
             (get ',name 'axis-principal-node-type) ',principal-node-type))))

(defun axis-function (axis) (get axis 'axis-function))

(defun axis-principal-node-type (axis) (get axis 'axis-principal-node-type))

(define-axis :child () (dom-child-nodes->pipe node))

(define-axis :self () (list node))

(define-axis :parent ()
  (if (and (dom:parent-node node) (dom:element-p (dom:parent-node)))
      (list (dom:parent-node node))
      empty-pipe))

(define-axis :descendant-or-self ()
  (make-pipe node
	     (mappend-pipe (axis-function :descendant-or-self)
			   (dom-child-nodes->pipe node))))

(define-axis :descendant ()
  (mappend-pipe (axis-function :descendant-or-self)
                (dom-child-nodes->pipe node)))

(define-axis :ancestor () ;; fixme -- define via axis-ancestor-or-self fixme -- (has-parent-element-p x)
  (if (and (dom:parent-node node) (dom:element-p (dom:parent-node)))
      (make-pipe (dom:parent-node node)
		 (funcall (axis-function :ancestor) (dom:parent-node node)))
      empty-pipe))

(define-axis :attribute (:attribute)
  (filter-pipe #'(lambda (item) (not (equal (dom:namespace-uri item) "http://www.w3.org/2000/xmlns/")))
	       (dom-attributes->pipe node)))
