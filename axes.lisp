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

(define-axis :child () (xpath-protocol:child-pipe node))

(define-axis :self () (list node))

(define-axis :parent () (parent-pipe node))

(define-axis :descendant-or-self ()
  (make-pipe node
	     (mappend-pipe (axis-function :descendant-or-self)
			   (xpath-protocol:child-pipe node))))

(define-axis :descendant ()
  (mappend-pipe (axis-function :descendant-or-self)
                (xpath-protocol:child-pipe node)))

(define-axis :following-sibling ()
  (unless (null (xpath-protocol:parent-node node))
    (subpipe-after node (xpath-protocol:child-pipe
			 (xpath-protocol:parent-node node)))))

(define-axis :preceding-sibling ()
  (let ((parent (xpath-protocol:parent-node node)))
    (if parent
	(nreverse (force (subpipe-before node (xpath-protocol:child-pipe
					       parent))))
	empty-pipe)))

(define-axis :following ()
  (mappend-pipe (axis-function :following-sibling)
		(funcall (axis-function :ancestor-or-self)
			 node)))

(define-axis :preceding ()
  (mappend-pipe (axis-function :preceding-sibling)
		(funcall (axis-function :ancestor-or-self)
			 node)))

(define-axis :ancestor ()
  (mappend-pipe (axis-function :ancestor-or-self)
                (parent-pipe node)))

(define-axis :ancestor-or-self ()
  (make-pipe node
	     (mappend-pipe (axis-function :ancestor-or-self)
			   (parent-pipe node))))

(define-axis :attribute (:attribute)
  (xpath-protocol:attribute-pipe node))

(define-axis :namespace (:namespace)
  (xpath-protocol:namespace-pipe node))

;; FIXME: This is a pseudo-axis used to implement absolute paths.
;; Is this the right approach?
(define-axis :root ()
  (loop
     for this = node then parent
     for parent = (xpath-protocol:parent-node this)
     while parent
     finally (return (list this))))
