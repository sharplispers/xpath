(in-package :xpath)

;; node tests

(defun node-test-name (name) ;; fixme: qnames? fixme: non-element nodes?
  #'(lambda (node principal-node-type)
      #+nil
      (dbg "node ~s principal-node-type ~s name ~s" node principal-node-type name)
      (and (node-type-p node principal-node-type)
           (string= (dom:node-name node) name))))

(defun node-test-principal ()
  #'(lambda (node principal-node-type)
      (node-type-p node principal-node-type)))

(defun node-test-text-node ()
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (dom:text-node-p node)))

(defun node-test-processing-instruction ()
    #'(lambda (node principal-node-type)
        (declare (ignore principal-node-type))
        (dom:processing-instruction-p node)))

(defun node-test-comment ()
  #'(lambda (node principal-node-type)
      (declare (ignore principal-node-type))
      (dom:comment-p node)))

(defun node-test-node () ;; FIXME: test
  #'(lambda (node principal-node-type)
      (declare (ignore node principal-node-type))
      t))
