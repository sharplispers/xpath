(in-package :xpath)

(defun mklist (value)
  (if (atom value) (list value) value))

(defun concat (&rest values)
  "Concatenate string representations (PRINC-TO-STRING) of VALUES"
  (apply #'concatenate 'string
	 (mapcar #'princ-to-string values)))

(defun string-replace (string substring replacement)
  "Replace occurences of SUBSTRING in STRING with REPLACEMENT."
  (if (or (zerop (length string)) (zerop (length substring)))
      string
      (with-output-to-string (out)
	(labels ((rec (start)
		   (let ((p (search substring string :start2 start)))
		     (cond (p (write-string string out :start start :end p)
			      (write-string replacement out)
			      (rec (+ p (length substring))))
			   (t (write-string string out :start start))))))
	  (rec 0)))))

(defun trim (str)
  (string-trim '(#\space #\tab #\newline #\return) (or str "")))

(defmacro with-gensyms (syms &rest body)
  "Execute the body binding syms to gensyms"
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (gensym ,(concat (string sym) "-"))))
		 syms)
     ,@body))

;; ONCE-ONLY was lifted from Practical Common Lisp (slightly modified)
(defmacro once-only ((&rest names) &body body)
  (assert (every #'symbolp names) () "all names should be symbols: ~s" names)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
		,@body)))))

(defun proper-list-p (list)
  "True if the LIST is proper list"
  (or (null list)
      (and (consp list)
	   (proper-list-p (cdr list)))))

(defun maybe-progn (body)
  "Wrap the BODY with PROGN if it contains more than one form.
Otherwise return the first form or NIL if the body is empty"
  (assert (proper-list-p body) () "body is not a proper list: ~s" body)
  (if (rest body)
      `(progn ,@body)
      (first body)))

(defun hypsym (&rest parts)
  "Assemble a symbol from PARTS interleaving them with hyphens"
  (intern
   (with-standard-io-syntax
     (format nil "~{~a~^-~}" parts))))
