(in-package :xpath)

;; node tests

(defun node-test-name (local-name &optional (uri ""))
  ;; fixme: non-element nodes?
  #'(lambda (node principal-node-type)
      #+nil
      (dbg "node ~s principal-node-type ~s name ~s" node principal-node-type name)
      (and (node-type-p node principal-node-type)
	   (string= (dom:namespace-uri node) uri)
	   (string= (dom:local-name node) local-name))))

(defun node-test-namespace (uri)
  #'(lambda (node principal-node-type)
      (and (node-type-p node principal-node-type)
	   (string= (dom:namespace-uri node) uri))))

(defun node-test-principal ()
  #'(lambda (node principal-node-type)
      (node-type-p node principal-node-type)))

(defun node-test-text-node ()
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (dom:text-node-p node)))

(defun node-test-processing-instruction (&optional name)
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (and (dom:processing-instruction-p node)
	     (or (null name) (equal (dom:node-name node) name)))))

(defun node-test-comment ()
  #'(lambda (node principal-node-type)
      (declare (ignore principal-node-type))
      (dom:comment-p node)))

(defun node-test-node () ;; FIXME: test
  #'(lambda (node principal-node-type)
      (declare (ignore node principal-node-type))
      t))
