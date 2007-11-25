(in-package :xpath)

;; node tests

(defun node-test-name (local-name &optional (uri ""))
  #'(lambda (node principal-node-type)
      (and (xpath-protocol:node-type-p node principal-node-type)
	   (string= (xpath-protocol:namespace-uri node) uri)
	   (string= (xpath-protocol:local-name node) local-name))))

(defun node-test-namespace (uri)
  #'(lambda (node principal-node-type)
      (and (xpath-protocol:node-type-p node principal-node-type)
	   (string= (xpath-protocol:namespace-uri node) uri))))

(defun node-test-principal ()
  #'(lambda (node principal-node-type)
      (xpath-protocol:node-type-p node principal-node-type)))

(defun node-test-text-node ()
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (xpath-protocol:node-type-p node :text)))

(defun node-test-processing-instruction (&optional name)
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (and (xpath-protocol:node-type-p node :processing-instruction)
	     (or (null name)
		 (equal (xpath-protocol:processing-instruction-target node)
			name)))))

(defun node-test-comment ()
  #'(lambda (node principal-node-type)
      (declare (ignore principal-node-type))
      (xpath-protocol:node-type-p node :comment)))

(defun node-test-node () ;; FIXME: test
  #'(lambda (node principal-node-type)
      (declare (ignore node principal-node-type))
      t))
