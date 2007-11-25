(in-package :xpath)

;; sought at http://paste.lisp.org/display/46324
;; (which is slighltly different from PAIP version)

(defconstant empty-pipe nil)

(defun pipe-empty-p (pipe) (eq pipe empty-pipe))

(defmacro make-pipe (head tail)
  `(cons ,head #'(lambda () ,tail)))

(defun pipe-head (pipe) (car pipe))

(defun pipe-tail (pipe)
  (if (functionp (cdr pipe))
      (setf (cdr pipe) (funcall (cdr pipe)))
      (cdr pipe)))

(defun enumerate (pipe &key count key (result pipe))
  "go through all or count elements of pipe,
  possibly applying the key function. "
  (if (or (pipe-empty-p pipe) (eql count 0))
      result
      (progn
	(unless (null key) (funcall key (pipe-head pipe)))
	(enumerate (pipe-tail pipe)
		   :count (if count (1- count))
		   :key key
		   :result result))))

(defun force (pipe)
  (enumerate pipe))

(defun pipe-length (pipe) ;; DANGER: this func should not be used for infinite pipes
  (length (force pipe)))

(defun map-pipe (function pipe)
  (if (pipe-empty-p pipe)
      empty-pipe
      (make-pipe (funcall function (pipe-head pipe))
		 (map-pipe function (pipe-tail pipe)))))

(defun pipe-elt (n pipe)
  (if (plusp n)
      (pipe-elt (1- n) (pipe-tail pipe))
      (pipe-head pipe)))

(defun filter-pipe (function pipe)
  (cond ((pipe-empty-p pipe) empty-pipe)
	((funcall function (pipe-head pipe))
	 (make-pipe (pipe-head pipe) (filter-pipe function (pipe-tail pipe))))
	(t (filter-pipe function (pipe-tail pipe)))))

#+nil
(defun append-pipes-rec (pipe-of-pipes) ;; FIXME
  (cond ((pipe-empty-p pipe-of-pipes) empty-pipe)
	((pipe-empty-p (pipe-head pipe-of-pipes))
	 (append-pipes-rec (pipe-tail pipe-of-pipes)))
	(t (make-pipe (head (pipe-head pipe-of-pipes))
		      (append-pipes-rec (make-pipe (pipe-tail (pipe-head pipe-of-pipes))
						   (pipe-tail pipe-of-pipes)))))))

(defun map-pipe-filtering (fn pipe &optional filter-test)
  "Map fn over pipe, delaying all but the first fn call,
 	   collecting results"
  (if (pipe-empty-p pipe)
      empty-pipe
      (let* ((head (pipe-head pipe))
	     (tail (pipe-tail pipe))
	     (result (funcall fn head)))
	(if (or (and filter-test (funcall filter-test result))
		result)
	    (make-pipe result (map-pipe-filtering fn tail filter-test))
	    (map-pipe-filtering fn tail filter-test)))))

(defun append-pipes (pipex pipey)
  "return a pipe that appends two pipes"
  (if (pipe-empty-p pipex)
      pipey
      (make-pipe (pipe-head pipex)
		 (append-pipes (pipe-tail pipex) pipey))))

(defun mappend-pipe (fn pipe)
  "lazily map fn over pipe, appending results"
  (if (pipe-empty-p pipe)
      empty-pipe
      (let ((x (funcall fn (pipe-head pipe))))
	(make-pipe (pipe-head x)
		   (append-pipes (pipe-tail x)
				 (mappend-pipe fn (pipe-tail pipe)))))))

(defun mappend-pipe-filtering (fn pipe &optional filter-test)
  "Map fn over pipe, delaying all but the first fn call,
  appending results, filtering along the way"
  (if (pipe-empty-p pipe)
      empty-pipe
      (let* ((head (pipe-head pipe))
	     (tail (pipe-tail pipe))
	     (result (funcall fn head)))
	(if (or (and filter-test (funcall filter-test result))
		result)
	    (make-pipe (pipe-head result)
		       (append-pipes (pipe-tail result)
				     (mappend-pipe-filtering fn tail filter-test)))
	    (mappend-pipe-filtering fn tail filter-test)))))


(defun subpipe-before (elt pipe &key (test #'eql))
  (if (pipe-empty-p pipe)
      pipe
      (let ((head (pipe-head pipe))
	    (tail (pipe-tail pipe)))
	(if (funcall test elt head)
	    empty-pipe
	    (make-pipe head (subpipe-before elt tail :test test))))))

(defun subpipe-after (elt pipe &key (test #'eql))
  (loop
     for p = pipe then (pipe-tail p)
     while (and p (not (funcall test (pipe-head p) elt)))
     finally (return (pipe-tail p))))
