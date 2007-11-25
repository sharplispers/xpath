(in-package :xpath)

(defvar *regex-cache* (make-hash-table :test #'equal))

(defun rx (regex &key case-insensitive-mode multi-line-mode
	   single-line-mode extended-mode destructive)
  (let ((key (list regex case-insensitive-mode multi-line-mode
		   single-line-mode extended-mode destructive)))
    (or (gethash key *regex-cache*)
	(setf (gethash key *regex-cache*)
	      (cl-ppcre:create-scanner regex
                                       :case-insensitive-mode case-insensitive-mode
				       :multi-line-mode multi-line-mode
				       :single-line-mode single-line-mode
				       :extended-mode extended-mode
				       :destructive destructive)))))

(defmacro with-match ((&rest binds) (regex target-string
                                           &rest rx-options
                                           &key start end
                                           &allow-other-keys)
                      &body body)
  (assert (every #'symbolp binds) () "invalid binds: ~s" binds)
  (with-gensyms (matches)
    (once-only (target-string)
      (let ((compiled-rx-options (copy-list rx-options)))
        (remf compiled-rx-options :start)
        (remf compiled-rx-options :end)
        `(multiple-value-bind (whole ,matches)
             (cl-ppcre:scan-to-strings
              (rx ,regex ,@compiled-rx-options)
              ,target-string
              :start ,(or start 0)
              :end ,(or end `(length ,target-string)))
           (declare (ignorable ,matches))
           (when whole
             ,(if (null binds)
                  `(locally ,@body)
                  `(let ,(loop for n from 0
                               for name in binds
                               when name
                                 collect `(,name (aref ,matches ,n)))
                     ,@body))))))))

(defmacro match-case ((target-string &key start end) &body cases)
  (let ((rx-cases (remove t cases :key #'first))
        (t-case (assoc t cases)))
    (with-gensyms (match-case-block)
      (once-only (target-string)
        `(block ,match-case-block
           ,@(loop for (expr binds . forms) in rx-cases
                   for rx-expr = (mklist expr)
                   collect `(with-match ,binds (,(first rx-expr) ,target-string
                                                 ,@(rest rx-expr)
                                                 ,@(when start `(:start ,start))
                                                 ,@(when end `(:end ,end)))
                              (return-from ,match-case-block
                                ,(maybe-progn forms))))
           ,@(rest t-case))))))

(deftest test-match-case
  (macrolet ((match (x &optional start end)
               (with-gensyms (word num)
                 `(match-case (,x ,@(when start `(:start ,start))
                                  ,@(when end `(:end ,end)))
                    ("^\\d+" () whole)
                    ("(\\w+?)(\\d+)" (,word ,num) (format nil "<~a-~a>" ,word ,num))
                    (t :none)))))
    (assert-equal* "123" (match "123abc")
                   "<abc-42>" (match "abc42")
                   :none (match "%%%")
                   "<a-1>" (match "a1 b42")
                   "<b-42>" (match "a1 b42" 3)
                   "<b-4>" (match "a1 b42" 3 5))))
