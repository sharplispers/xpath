(in-package :xpath)

(defvar *tests* nil)

(defmacro assert-equal (expected actual)
  "Check whether two values are equal"
  (with-gensyms (exp-value act-value)
    `(let ((,exp-value ,expected)
	   (,act-value ,actual))
      (unless (equal ,exp-value ,act-value)
	(error "TEST FAILED: ~s is expected to be~%~s~%but was~%~s"
	       ',actual ,exp-value ,act-value)))))

(defmacro assert-equal* (&rest pairs)
  (maybe-progn
   (loop for (expected actual) on pairs by #'cddr
	 collect `(assert-equal ,expected ,actual))))

(defmacro assert* (&rest expressions)
  (maybe-progn
   (loop for expr in expressions collect `(assert ,expr))))

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun run-all-tests ()
  (mapc #'funcall (sort (copy-list *tests*) #'string<)))
