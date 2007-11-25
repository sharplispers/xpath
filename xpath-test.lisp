(in-package :xpath)

;; XPath-related tests

(defun show-node-set (node-set)
  (enumerate (pipe-of node-set)
             :key #'(lambda (node)
                      (sx node)
                      (format t "~&-------~%"))))

(defparameter *sample-xml*
  (cxml:parse-rod
   (concat 
    "<div class='something'>"
    "<a href='zzz'>"
    "<span class='sample'>val1</span>"
    "val2"
    "</a>"
    "<a href='qqq' id='a2'>"
    "<span class='sample'>val3</span>"
    "<span><br/></span>"
    "</a>"
    "<span class='another'>another-value</span>"
    "<span class='yetanother' id='s5'>42<hr/></span>"
    "</div>")
   (cxml-dom:make-dom-builder)))

(defparameter *sample-xml-2*
  (cxml:parse-rod
   (concat 
    "<div class='something' xmlns:special='http://special'>"
    "<special:a href='zzz'>"
    "<span class='sample'>val1</span>"
    "val2"
    "</special:a>"
    "<a href='qqq' id='a2'>"
    "<span class='sample'>val3</span>"
    "<span><br/></span>"
    "</a>"
    "<special:span class='another'>another-value</special:span>"
    "<span class='yetanother' id='s5'>42<hr/></span>"
    "</div>")
   (cxml-dom:make-dom-builder)))

(defun join-xpath-result (result)
  (if (node-set-p result)
      (format nil "~{~a~^|||~}"
              (mapcar #'get-node-text (force (pipe-of result))))
      (string-value result)))

(defun sample-node-set (&optional (xml ""))
  (make-node-set
   (xpath-protocol:child-pipe
    (dom:document-element
     (cxml:parse-rod (format nil "<div>~a</div>" xml)
                     (cxml-dom:make-dom-builder))))))

(deftest test-values
  (assert-equal*
   t (boolean-value t)
   t (boolean-value 123)
   t (boolean-value "zzz")
   t (boolean-value (sample-node-set "<span/>"))
   nil (boolean-value nil)
   nil (boolean-value 0)
   nil (boolean-value "")
   nil (boolean-value (sample-node-set))
   0 (number-value nil)
   1 (number-value t)
   42 (number-value 42)
   4242 (number-value "4242")
   1234 (number-value (sample-node-set "<span>1234</span><span>5678</span>"))
   "false" (string-value nil)
   "true" (string-value t)
   "123" (string-value 123) ;; FIXME: more precise stuff for double-float needed
   "zzzz" (string-value (sample-node-set "<a href='qqq'>zzzz</a><span>pppp</span>")))
  (let ((node-set (sample-node-set "<span/>")))
    (assert (eq node-set (node-set-value node-set)))))

(deftest test-comparison
  (flet ((2values (a b)
           (sample-node-set (format nil "<a href='zzz'>~a</a><span>~a</span>" a b))))
    (assert* (compare-values 'equal 1 1)
             (not (compare-values 'equal 1 2))
             (compare-values 'equal t 1)
             (not (compare-values 'equal nil 1))
             (compare-values 'equal nil nil)
             (compare-values 'equal t t)
             (compare-values 'equal "a" "a")
             (not (compare-values 'equal "b" "a"))
             (compare-values 'equal "" nil)
             (not (compare-values 'equal "a" nil))
             (compare-values 'equal "a" t)
             (compare-values 'equal (sample-node-set "<a href='zzz'>qqq</a><span>fff</span>")
                             (sample-node-set "<span>fff</span><a href='epr'>st</a>"))
             (not (compare-values 'equal (sample-node-set "<a href='zzz'>qqq</a><span>fff</span>")
                                  (sample-node-set "<span>fff1</span><span>fff3</span>")))
             (compare-values 'equal t (sample-node-set "<span>q</span>"))
             (compare-values 'equal nil (sample-node-set "<span/>"))
             (compare-values 'equal 99 (sample-node-set "<span>123</span><span>99</span>"))
             (compare-values 'equal (sample-node-set "<span>123</span><span>99</span>") 99)
             (not (compare-values 'equal 42 (sample-node-set "<span>123</span><span>99</span>")))
             (not (compare-values 'equal (sample-node-set "<span>123</span><span>99</span>") 42))
             (compare-values 'equal "q" (sample-node-set "<span>q</span><span>ff</span>"))
             (compare-values 'equal (sample-node-set "<span>q</span><span>ff</span>") "q")
             (not (compare-values 'equal "qr" (sample-node-set "<span>q</span><span>ff</span>")))
             (not (compare-values 'equal (sample-node-set "<span>q</span><span>ff</span>") "qr"))
             (compare-values '< 1 2)
             (not (compare-values '< 2 1))
             (compare-values '<= 1 1)
             (not (compare-values '<= 2 1))
             (compare-values '< "1" "2")
             (compare-values '< "1" 2)
             (not (compare-values '< 2 "1"))
             (compare-values '< nil t)
             (compare-values '> t nil)
             (compare-values '<= nil nil)
             (compare-values '>= nil nil)
             (compare-values '<= t t)
             (compare-values '>= t t)
             (not (compare-values '< t nil))
             (not (compare-values '> nil t))
             (compare-values '> (2values 1 2) (2values 1 2))
             (compare-values '< (2values 1 2) (2values 1 2))
             (compare-values '>= (2values 1 2) (2values 1 2))
             (compare-values '<= (2values 1 2) (2values 1 2))
             (not (compare-values '> (2values 1 2) (2values 3 4)))
             (not (compare-values '>= (2values 1 2) (2values 3 5)))
             (not (compare-values '< (2values 9 10) (2values 3 4)))
             (not (compare-values '<= (2values 9 10) (2values 3 4))))))

(deftest test-xpath/internal
  (let ((path1 (make-location-path
                (list (make-location-step :descendant
                                          (node-test-name "a")
                                          nil)
                      (make-location-step :attribute
                                          (node-test-name "href")
                                          nil))))
        (path2 (make-location-path
                (list (make-location-step
                       :child
                       (node-test-name "div")
                       nil)
                      (make-location-step
                       :descendant
                       (node-test-principal)
                       (list
			(xf-equal
			 (xf-location-path
			  (make-location-path
			   (list (make-location-step :attribute
						     (node-test-name "class")
						     nil))))
			 (xf-value "another"))))))))
    (assert-equal (list "zzz" "qqq")
                  (force
                   (map-pipe #'dom:node-value
                             (funcall path1 *sample-xml*))))
    (assert-equal "zzz"
                  (string-value
                   (funcall (xf-location-path path1)
                            (make-context *sample-xml*))))
    (assert-equal "another-value"
                  (string-value
                   (funcall (xf-location-path path2)
                            (make-context *sample-xml*))))))

;; TODO: test * and :text node tests; test returning multiple items; add other funcs; fix xf-equal
(deftest test-xpath/unabbreviated
  (macrolet ((verify-xpath* (&rest pairs)
               (maybe-progn
                (loop for (expected xpath) on pairs by #'cddr
                      collect `(assert-equal ,expected
                                              (join-xpath-result
                                               (funcall (compile-xpath ,xpath (make-test-environment))
                                                        (make-context *sample-xml*))))))))
    (verify-xpath*
     "zzz|||qqq"
     '(:path 
       (:descendant "a")
       (:attribute "href"))
     "another-value"
     '(:path
       (:child "div")
       (:descendant *
        (= (:path (:attribute "class")) "another")))
     "val1val2"
     '(:path
       (:descendant "a"
        (= (:path (:attribute "href")) "zzz")))
     "another-value"
     '(:path
       (:child "div")
       (:child *
        (= (:path (:attribute "class")) "another")))
     "val3"
     '(:path
       (:descendant "a"
        (/= (:path (:attribute "href")) "zzz")))
     "val1|||val3|||another-value"
     '(:path
       (:descendant "span"
        (:or (= (:path (:attribute "class")) "sample")
         (= (:path (:attribute "class")) "another"))))
     "42"
     '(:path
       (:descendant "span"
        (:and (:path (:attribute "class"))
         (:not (:or (= (:path (:attribute "class")) "sample")
                    (= (:path (:attribute "class")) "another"))))))
     "42"
     '(:path
       (:child *)
       (:child * (:path (:child "hr"))))
     "val3"
     '(:path
       (:descendant "a"
        (= (:position) 2)))
     "42"
     '(:path
       (:descendant "span"
        (= (:position) (:last))))
     "val1"
     '(:path
       (:descendant "span" 1))
     "42"
     '(:path
       (:descendant "span" (:last)))
     "true" '(< 1 2)
     "false" '(> 1 2)
     "true" '(<= (:path (:child "div")
                  (:child "span" (= (:path (:attribute "class")) "yetanother")))
              42)
     "false" '(>= (:path (:child "div")
                   (:child "span" (= (:path (:attribute "class")) "yetanother")))
               43)
     "5"
     '(:count (:path (:descendant "span")))
     "val3"
     '(:path (:child "div")
       (:child "a" (= (:count (:path (:child "span"))) 2)))
     "2"
     '(:count (:path (:descendant * (= (:local-name) "a"))))
     "span"
     '(:local-name (:path (:descendant * (= (:path (:attribute "class")) "yetanother"))))
     ""
     '(:local-name (:path (:descendant "font")))
     "class"
     '(:local-name (:path (:descendant *) (:attribute "class")))
     "abc42qqq-false"
     '(:concat "abc" 42 "qqq-" nil)
     ;; test string(x)
     "val1val2|||val3"
     '(:path (:descendant "a"))
     "val1val2"
     '(:string (:path (:descendant "a")))
     "true"
     '(:contains "needle" "needle-in-haystack")
     "1999"
     '(:substring-before "1999/04/01" "/")
     "04/01"
     '(:substring-after "1999/04/01" "/")
     "99/04/01"
     '(:substring-after "1999/04/01" "19")
     "3"
     '(:string-length "abc")
     "42"
     '(:path (:descendant * (> (:path (:self *)) 41)))
     "42"
     '(* 2 (+ (- 100 90) 11)))))

(deftest test-with-namespaces-1
  (with-namespaces (("" ""))
    (eq (first-node (evaluate "/div" *sample-xml*))
	(dom:document-element *sample-xml*))))

(deftest test-with-namespaces-2
  (with-namespaces (("foo" "http://special"))
    (eql 1 (length (all-nodes (evaluate "//foo:a" *sample-xml-2*))))))

(deftest test-with-namespaces-3
  (with-namespaces (("foo" "http://special"))
    (eql 2 (length (all-nodes (evaluate "//foo:*" *sample-xml-2*))))))

(with-namespaces (("foo" "http://special"))
  (deftest test-with-namespaces-4
    (eql 2 (length (all-nodes (evaluate "//foo:*" *sample-xml-2*))))))

(deftest test-with-namespaces-5
  (handler-case
      (funcall (compile nil
			`(lambda ()
			   (with-namespaces (("foo" "http://special"))
			     (evaluate "//bar:*" *sample-xml-2*)))))
    (error ()
      t)
    (:no-error (x)
      (error "test failed with return value ~A" x))))
