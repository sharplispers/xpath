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


#+nil
(defun compile-binary (op exprs)
  (assert (eq op '=)) ;; FIXME!!! (TBD)
  (destructuring-bind (u v) exprs
    (xf-equal (compile-xpath u) (compile-xpath v))))


(define-xpath-function/lazy :and (&rest exprs) (xf-and exprs))

(define-xpath-function/lazy :or (&rest exprs) (xf-or exprs))

(define-xpath-function/eager = (a b) (compare-values 'equal a b))

(define-xpath-function/eager /= (a b) (not (compare-values 'equal a b)))

(define-xpath-function/eager < (a b) (compare-values '< a b))

(define-xpath-function/eager > (a b) (compare-values '> a b))

(define-xpath-function/eager <= (a b) (compare-values '<= a b))

(define-xpath-function/eager >= (a b) (compare-values '>= a b))

(define-xpath-function/single-type :not boolean (a) (not a))

;; node-set functions

(define-xpath-function/eager :position () (context-position context))

(define-xpath-function/eager :last () (context-size context))

(define-xpath-function/single-type :count node-set (node-set)
  (pipe-length (pipe-of node-set)))

(define-xpath-function/single-type :local-name node-set (&optional node-set)
  (cond ((null node-set) (dom:local-name (context-node context))) ;; FIXME: root?
        ((pipe-empty-p (pipe-of node-set)) "")
        (t (or (dom:local-name (pipe-head (pipe-of node-set))) ;; FIXME: dom:local-name may return NIL as attribute name for some reason
               (dom:name (pipe-head (pipe-of node-set)))))))

;; helper function for the | operator, not in the keyword package:
(define-xpath-function/single-type union node-set (node-set-1 node-set-2)
  (make-node-set (append-pipes (pipe-of node-set-1) (pipe-of node-set-2))))

;; TODO: id, name, namespace-uri

;; string functions

(define-xpath-function/single-type :string string (string) string)

(define-xpath-function/single-type :concat string (&rest strings)
  (reduce #'concat strings))

(define-xpath-function/single-type :contains string (needle haystack)
  (and (search needle haystack) t))

;; FIXME: corner case: empty substring?
(define-xpath-function/single-type :substring-before string (string substring)
  (let ((p (search substring string)))
    (if (null p)
        ""
        (subseq string 0 p))))

;; FIXME: corner case: empty substring?
(define-xpath-function/single-type :substring-after string (string substring)
  (let ((p (search substring string)))
    (if (null p)
        ""
        (subseq string (+ p (length substring))))))

(define-xpath-function/single-type :string-length string (string) (length string))

;; number functions

(macrolet ((numop (op lisp-op)
             `(define-xpath-function/single-type ,op number (a b) (,lisp-op a b))))
  (numop + xnum-+)
  (numop - xnum--)
  (numop * xnum-*)
  (numop / xnum-/)
  (numop mod xnum-mod))
