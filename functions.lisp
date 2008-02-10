(in-package :xpath)

;; function library

(defun xf-equal (u v) ;; FIXME: rm; use compare-values in tests
  #'(lambda (context)
      (compare-values 'equal
                      (funcall u context)
                      (funcall v context))))

(defun xf-and (exprs)
  (cond ((null exprs) t)
        ((null (rest exprs))
         (let ((first (first exprs)))
           #'(lambda (context)
               (boolean-value (funcall first context)))))
        (t
         (let ((first (first exprs))
               (rest (xf-and (rest exprs))))
           #'(lambda (context)
               (and (boolean-value (funcall first context))
                    (funcall rest context)))))))

(defun xf-or (exprs)
  (cond ((null exprs) nil)
        ((null (rest exprs))
         (let ((first (first exprs)))
           #'(lambda (context)
               (boolean-value (funcall first context)))))
        (t
         (let ((first (first exprs))
               (rest (xf-and (rest exprs))))
           #'(lambda (context)
               (or (boolean-value (funcall first context))
                   (funcall rest context)))))))

(define-xpath-function/eager = (a b) (compare-values 'equal a b))

(define-xpath-function/eager /= (a b) (not (compare-values 'equal a b)))

(define-xpath-function/eager < (a b) (compare-values '< a b))

(define-xpath-function/eager > (a b) (compare-values '> a b))

(define-xpath-function/eager <= (a b) (compare-values '<= a b))

(define-xpath-function/eager >= (a b) (compare-values '>= a b))

;; boolean functions

(define-xpath-function/lazy :and (&rest exprs) (xf-and exprs))

(define-xpath-function/lazy :or (&rest exprs) (xf-or exprs))

(define-xpath-function/single-type :not boolean (a) (not a))

(define-xpath-function/eager :true () t)

(define-xpath-function/eager :false () nil)

(define-xpath-function/single-type :boolean boolean (value) value)

;; node-set functions

(define-xpath-function/eager :position () (context-position context))

(define-xpath-function/eager :last () (context-size context))

(define-xpath-function/single-type :count node-set (node-set)
  (pipe-length (pipe-of node-set)))

(define-xpath-function/single-type :local-name node-set (&optional node-set)
  (cond ((null node-set) (xpath-protocol:local-name (context-node context))) ;; FIXME: root?
        ((pipe-empty-p (pipe-of node-set)) "")
        (t (xpath-protocol:local-name (pipe-head (pipe-of node-set))))))

;; helper function for the | operator, not in the keyword package:
(define-xpath-function/single-type union node-set (node-set-1 node-set-2)
  (make-node-set (append-pipes (pipe-of node-set-1) (pipe-of node-set-2))))

;; TODO: id, name, namespace-uri

;; string functions

(define-xpath-function/lazy :string (&optional string)
  (if string
      (lambda (ctx)
	(string-value (funcall string ctx)))
      (lambda (ctx)
	(string-value (context-node ctx)))))

(define-xpath-function/single-type :concat string (&rest strings)
  (reduce #'concat strings))

(define-xpath-function/single-type :contains string (needle haystack)
  (and (search needle haystack) t))

(define-xpath-function/eager :substring (string start &optional (len nil len-p))
  (let* ((string (string-value string))
         (start (xnum-round (number-value start)))
         (end (if len-p
                  (xnum-+ start (xnum-round (number-value len)))
                  (1+ (length string)))))
    (if (or (nan-p start)
            (nan-p end)
            (compare-numbers '> start end)
            (compare-numbers '> start (length string))
            (compare-numbers '< end 1))
        ""
        (subseq string
                (1- (if (inf-p start) 1 (max 1 start)))
                (1- (if (inf-p end)
                        (1+ (length string))
                        (min end (1+ (length string)))))))))

(define-xpath-function/eager :starts-with (string prefix)
  (let* ((string (string-value string))
         (prefix (string-value prefix))
	 (i (mismatch string prefix)))
    (and (or (null i)
	     (eql i (length prefix)))
	 t)))

;; FIXME: corner case: empty substring?
;; [looks correct to me.  XPath 2.0 agrees that the empty string is
;; returned if the second argument is the empty string. --dfl]
(define-xpath-function/single-type :substring-before string (string substring)
  (let ((p (search substring string)))
    (if (null p)
        ""
        (subseq string 0 p))))

;; FIXME: corner case: empty substring?
;; [looks correct to me.  XPath 2.0 agrees that the first argument is
;; returned if the second argument is the empty string. --dfl]
(define-xpath-function/single-type :substring-after string (string substring)
  (let ((p (search substring string)))
    (if (null p)
        ""
        (subseq string (+ p (length substring))))))

(define-xpath-function/lazy :string-length (&optional string)
  (if string
      (lambda (ctx)
	(length (string-value (funcall string ctx))))
      (lambda (ctx)
	(length (string-value (context-node ctx))))))

(define-xpath-function/lazy :normalize-space (&optional string)
  (lambda (ctx)
    (let ((string
	   (string-value (if string
			     (funcall string ctx)
			     (context-node ctx)))))
      (cl-ppcre::regex-replace-all "\\s+" (trim string) " "))))

(define-xpath-function/single-type :translate string (string from to)
  (map 'string
       (lambda (c)
	 (let ((i (position c from)))
	   (if i
	       (elt to i)
	       c)))
       string))

;; number functions

(define-xpath-function/single-type :number string (value)
  (number-value value))

(macrolet ((numop (op lisp-op)
             `(define-xpath-function/single-type ,op number (a b) (,lisp-op a b))))
  (numop + xnum-+)
  (numop * xnum-*)
  (numop / xnum-/)
  (numop mod xnum-mod))

(define-xpath-function/eager - (a &optional (b nil b-p))
  (if b-p
      (xnum-- (number-value a) (number-value b))
      (xnum-- (number-value a))))
