;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2008 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2008 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :xpath)

;; simple profiling support

(defvar *profiling-enabled-p* nil)

(defun maybe-wrap-profiling (key fun)
  (if *profiling-enabled-p*
      (wrap-profiling key fun)
      fun))

(defvar *samples* nil)

(defun enable-profiling (&optional (verbosep t))
  "@return{nil}
   @short{Enables profiling.}

   Resets any existing profile samples and enables profiling for future
   XPath compilations.

   Previously returned closures that were created with profiling disabled
   are not affected by this setting, but closures created during an earlier
   profiling session will start sampling again.

   But note that @fun{evaluate}, @fun{node-matches-p}, and @fun{pattern-case}
   will recompile their cached closure when this setting has changed.

   Profiling is not thread safe.

   @see{disable-profiling}
   @see{report}"
  (setf *profiling-enabled-p* t)
  (setf *samples* nil)
  (format t "~&XPath profiling enabled.  (0 samples now recorded)~%~%")
  (when verbosep
    (format t "*** ENABLE-PROFILING does not affect closures that have~%")
    (format t "*** already been compiled.~%~%")))

(defun disable-profiling ()
  "@return{nil}
   @short{Disables profiling.}
   
   Disables profiling for future XPath compilations, but keeps recorded
   profiling samples for @fun{report}.

   Previously returned closures that were created with profiling enabled
   will not record samples until profiling is re-activated.

   @see{enable-profiling}"
  (setf *profiling-enabled-p* nil)
  (format t "~&XPath profiling disabled.  (~D sample~:P currently recorded)~%"
	  (length *samples*)))

(defun write-node-test (node-test s)
  (etypecase node-test
    (string
     (write-string node-test s))
    (list
     (ecase (first node-test)
       (:processing-instruction
       (format s "processing-instruction(~A)" (second node-test)))
       (:namespace
	(format s "~A:*" (second node-test)))
       (:qname
	(destructuring-bind (prefix local-name) (rest node-test)
	  (format s "~A:~A" prefix local-name)))))
    (t
     (write-string (ecase node-test
		     (* "*")
		     (:node "node()")
		     (:text "text()")
		     (:processing-instruction "processing-instruction()")
		     (:comment "comment()"))
		   s))))

(defun stringify-pattern-expression (expr)
  (if (stringp expr)
      expr
      (with-output-to-string (s)
	(labels ((write-filter (x)
		   (unless (typep x '(member nil :true))
		     (write-char #\[ s)
		     (recurse x)
		     (write-char #\] s)))
		 (recurse (x)
		   (etypecase x
		     (string
		      (cond
			((find #\' x)
			 (when (find #\" x)
			   (error "bad string"))
			 (format s "\"~A\"" x))
			(t
			 (format s "\'~A\'" x))))
		     (number
		      (write x :stream s))
		     (cons
		      (case (car x)
			(:path
			 (let ((want-slash-p nil)
			       (steps (cdr x)))
			   (when (equal (car steps) '(:root :node))
			     (pop steps)
			     (write-char #\/ s))
			   (dolist (step steps)
			     (when want-slash-p
			       (write-char #\/ s))
			     (setf want-slash-p t)
			     (format s "~(~A~)::" (first step))
			     (write-node-test (second step) s)
			     (write-filter (third step)))))
			(:filter
			 (write-char #\( s)
			 (recurse (second x))
			 (write-char #\) s)
			 (write-filter (third x)))
			((< <= > >= = != + - * / mod)
			 (recurse (second x))
			 (princ (car x) s)
			 (when (cddr x)
			   (recurse (third x))))
			(t
			 (let ((fn (car x)))
			   (if (listp fn)
			       (format s "~A:~A" (second fn) (third fn))
			       (format s "~(~A~)" fn)))
			 (write-char #\( s)
			 (let ((want-comma-p nil)
			       (args (cdr x)))
			   (dolist (arg args)
			     (when want-comma-p
			       (write-char #\, s))
			     (setf want-comma-p t)
			     (recurse arg)))
			 (write-char #\) s)))))))
	  (recurse expr)))))

(defun stringify-pattern-expressions (patterns)
  (sort (mapcar #'stringify-pattern-expression
		(mapcar #'pattern-expression patterns))
	#'string<))

(defun wrap-profiling (key fun)
  (when (or (null key)
	    (and (consp key) (typep (car key) 'pattern)))
    (setf key (cons 'pattern (stringify-pattern-expressions key))))
  (lambda (ctx-or-node)
    (if *profiling-enabled-p*
        (let ((run0 (get-internal-run-time))
	      (real0 (get-internal-real-time)))
          (unwind-protect
               (let ((*profiling-enabled-p*
		      ;; protect against recursion, meaning that recursive
		      ;; calls will be counted for the parent call only.
		      ;; Might want to do something more fancy in the future.
		      nil))
                 (funcall fun ctx-or-node))
            (let ((run1 (get-internal-run-time))
		  (real1 (get-internal-real-time)))
              (push (list key (- run1 run0) (- real1 real0)) *samples*))))
	(funcall fun ctx-or-node))))

(defun group-and-sort-samples (group-identical-expressions)
  (let ((table (make-hash-table
		:test (if group-identical-expressions
			  'equal
			  'eql))))
    (loop
       for (key run real) in *samples*
       for accum = (or (gethash key table)
		       (setf (gethash key table) (list 0 0 0)))
       do
       (incf (first accum) run)
       (incf (second accum) real)
       (incf (third accum)))
    (loop
       for key being each hash-key in table using (hash-value accum)
       if (and (consp key) (eq (car key) 'pattern))
       collect (cons key accum) into patterns
       else collect (cons key accum) into expressions
       finally (return
		 (values
		  (sort expressions #'> :key #'second)
		  (sort patterns #'> :key #'second))))))

(defun report-xpath (grouped-samples)
  (format t "~&~D XPath expression~:P:~%~%"
	  (length grouped-samples))
  (format t "     runtime   realtime     #calls   avg.run    expression~%~%")
  (loop
     for (key run real calls) in grouped-samples do
     (format t "  ~10D ~10D ~10D ~10D   ~A~%"
	     run real calls (floor run calls) key)))

(defun report-xpattern (grouped-samples)
  (format t "~%~%~D pattern matcher~:P:~%~%" (length grouped-samples))
  (format t "     runtime   realtime     #calls   / patterns~%~%")
  (loop
     for (key run real calls) in grouped-samples do
     (format t "  ~10D ~10D ~10D~%~15T  ~{~A~^~%~15T| ~}~%~%"
	     run real calls (cdr key))))

(defun report (&key (group-identical-expressions t))
  "@arg[group-identical-expressions]{Boolean, indicates whether times
     recorded for closures that were compiled separately, but for the same
     expression, are to be summed together.  Default is T.}
   @short{Shows profiling output.}

   Shows cumulative run time and real time, number of calls, and average
   run time for each XPath expression or XPattern matcher that was invoked.
   @see{enable-profiling}
   @see{disable-profiling}"
  (format t "~&~D plexippus call~:P recorded~%~%" (length *samples*))
  (format t "1 second = ~D time units~%~%"
	  internal-time-units-per-second)
  (multiple-value-bind (grouped-xpath-samples
			grouped-xpattern-samples)
      (group-and-sort-samples group-identical-expressions)
    (report-xpath grouped-xpath-samples)
    (report-xpattern grouped-xpattern-samples)))
