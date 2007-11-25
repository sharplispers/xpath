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

(defun %node-parent-pipe (node) (funcall (axis-function :parent) node))
(defun %node-parent (node) (pipe-head (%node-parent-pipe node)))
(defun %node-children-pipe (node) (funcall (axis-function :child) node))

(define-axis :following-sibling ()
  (subpipe-after node (%node-children-pipe (%node-parent node))))

(define-axis :preceding-sibling ()
  (let ((parent (%node-parent node)))
    (if parent
	(let* ((children (dom:child-nodes parent))
	       (pos (position node children)))
	  (loop
	     for i from (1- pos) downto 0
	     collect (elt children i)))
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
                (%node-parent-pipe node)))

(define-axis :ancestor-or-self ()
  (make-pipe node
	     (mappend-pipe (axis-function :ancestor-or-self)
			   (%node-parent-pipe node))))

(define-axis :attribute (:attribute)
  (filter-pipe #'(lambda (item) (not (equal (dom:namespace-uri item) "http://www.w3.org/2000/xmlns/")))
	       (dom-attributes->pipe node)))

(define-axis :namespace (:namespace)
  ;; FIXME: completely untested
  ;; FIXME: rewrite this lazily?
  ;;
  ;; Note: We have to walk the tree ourselves here because of DOM's bad
  ;; namespace support.  In STP this would be a one-liner.
  ;;
  (let ((table (make-hash-table :test 'equal))
	(result '()))
    (labels ((record* (prefix uri)
	       (unless (or (equal prefix "xmlns")
			   (gethash prefix table))
		 (setf (gethash prefix table) uri)))
	     (record (node)
	       (record* (or (dom:prefix node) "") (dom:namespace-uri node)))
	     (recurse (node)
	       (record node)
	       (dolist (attribute (dom:items (dom:attributes node)))
		 (cond
		   ((equal (dom:namespace-uri attribute)
			   "http://www.w3.org/2000/xmlns/")
		    ;; record explicitly declared namespaces, which might
		    ;; not be in use anywhere
		    (record* (dom:local-name attribute)
			     (dom:value attribute)))
		   ((plusp (length (dom:prefix attribute)))
		    ;; record namespaces from DOM 2 slots, which might not
		    ;; be declared in an attribute
		    (record attribute))))
	       (let ((parent (dom:parent-node node)))
		 (when parent
		   (recurse parent)))))
      (record* "xml" "http://www.w3.org/XML/1998/namespace")
      (recurse node))
    (maphash #'(lambda (prefix uri)
		 (push (cons prefix uri) result))
	     table)
    result))

;; FIXME: This is a pseudo-axis used to implement absolute paths.
;; Is this the right approach?
(define-axis :root ()
  (loop
     for this = node then parent
     for parent = (dom:parent-node this)
     while parent
     finally (return (list this))))
