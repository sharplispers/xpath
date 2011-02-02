;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Please note:
;;;
;;; When changing Plexippus XPath, also run the entire
;;; XSLT suite using Xuriella XSLT like this:
;;;
;;;   $ cd xuriella && git submodule update --init
;;;   > (xuriella::run-tests)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XPath-related tests

(defparameter *sample-xml*
  (concat
   "<?xml version=\"1.0\"?>"
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
   "<h4>5</h4>"
   "</div>"))

(defparameter *sample-xml-2*
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
   "</div>"))

(defparameter *sample-xml-3*
  "<?xml version='1.0'?>
<!DOCTYPE main [
  <!ELEMENT main (a|b)*>
  <!ELEMENT a (#PCDATA)>
  <!ATTLIST a id ID #REQUIRED>
  <!ELEMENT b (#PCDATA)>
]>
<main>
  <a id='w'>W</a>
  <a id='x'>X</a>
  <a id='y'>Y</a>
  <a id='z'>Z</a>
  <b>y</b>
  <b>w</b>
  <b>x</b>
</main>")

(defparameter *sample-xml-4*
  (concat
   "<?xml version='1.0'?>"
   "<doc>"
   "  <a>"
   "    <b><c/></b>"
   "    <b><c><d><e/></d></c></b>"
   "    <e><c/></e>"
   "    <e><b><c/></b></e>"
   "    <e><c><d/></c></e>"
   "  </a>"
   "<A level='1'>"
   "  <X level='2'>"
   "     <B level='3'>"
   "        <X level='4'>"
   "          <C level='5'>"
   "            <X level='6'/>"
   "          </C>"
   "        </X>"
   "     </B>"
   "  </X>"
   "</A>"
   "</doc>"))

(defparameter *dom-builder* (cxml-dom:make-dom-builder))
(defparameter *document-element* #'dom:document-element)

(defmacro define-xpath-test (name &body body)
  `(deftest ,name
     (let ((*sample-xml* (cxml:parse-rod *sample-xml* *dom-builder*))
           (*sample-xml-2* (cxml:parse-rod *sample-xml-2* *dom-builder*))
           (*sample-xml-3* (cxml:parse-rod *sample-xml-3* *dom-builder*))
           (*sample-xml-4* (cxml:parse-rod *sample-xml-4* *dom-builder*)))
       ,@body)))

(defun join-xpath-result (result)
  (if (node-set-p result)
      (format nil "~{~a~^|||~}" (mapcar #'get-node-text (force (pipe-of result))))
      (string-value result)))

(defun sample-node-set (&optional (xml ""))
  (make-node-set
   (xpath-protocol:child-pipe
    (funcall *document-element*
             (cxml:parse-rod (format nil "<div>~a</div>" xml)
                             *dom-builder*)))))

(define-xpath-test test-values
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

(define-xpath-test test-comparison
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
             (compare-values 'equal nil (make-node-set nil))
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
             (not (compare-values '<= (2values 9 10) (2values 3 4)))
             (compare-values 'equal t 4)
             (compare-values 'equal nil 0))))

(defmacro verify-xpath* (xml &rest items)
  (once-only (xml)
    (maybe-progn
     (loop for (expected . xpaths) in items
           append
           (loop for xpath in xpaths
                 collect
                 `(progn
                    (format *debug-io* "~&testing: ~s -> ~s" ',xpath ',expected)
                    (assert-equal
                     ,expected
                     (join-xpath-result
                      (evaluate
                       ,(if (stringp xpath)
                            xpath
                            `'(xpath ,xpath))
                       (make-context ,xml))))))))))

;; TODO: test * and :text node tests; test returning multiple items; add other funcs; fix xf-equal
(define-xpath-test test-xpath
  (with-namespaces ()
    (verify-xpath*
     *sample-xml*
     ("zzz|||qqq"
      (:path
       (:descendant "a")
       (:attribute "href"))
      "descendant::a/attribute::href"
      "descendant::a/@href"
      "descendant::a/@href"
      "descendant::a[@href]/@href")
     ("another-value"
      (:path
       (:child "div")
       (:descendant *
                    (= (:path (:attribute "class")) "another")))
      "child::div/descendant::*[attribute::class='another']"
      "div/descendant::*[@class='another']")
     ("val1val2"
      (:path
       (:descendant "a"
                    (= (:path (:attribute "href")) "zzz")))
      "descendant::a[attribute::href='zzz']"
      "descendant::a[@href='zzz']")
     ("another-value"
      (:path
       (:child "div")
       (:child *
               (= (:path (:attribute "class")) "another")))
      "child::div/child::*[attribute::class='another']"
      "div/*[@class='another']")
     ("val3"
      (:path
       (:descendant "a"
                    (/= (:path (:attribute "href")) "zzz")))
      "descendant::a[attribute::href != 'zzz']"
      "descendant::a[@href != 'zzz']")
     ("val1|||val3|||another-value"
      (:path
       (:descendant "span"
                    (:or (= (:path (:attribute "class")) "sample")
                         (= (:path (:attribute "class")) "another"))))
      "descendant::span[attribute::class='sample' or attribute::class='another']"
      "descendant::span[@class='sample' or @class='another']")
     ("42"
      (:path
       (:descendant "span"
                    (:and (:path (:attribute "class"))
                          (:not (:or (= (:path (:attribute "class")) "sample")
                                     (= (:path (:attribute "class")) "another"))))))
      "descendant::span[attribute::class and not(attribute::class='sample' or attribute::class='another')]"
      "descendant::span[@class and not(@class='sample' or @class='another')]")
     ("42"
      (:path
       (:child *)
       (:child * (:path (:child "hr"))))
      "child::*/child::*[child::hr]"
      "*/*[hr]")
     ("val3"
      (:path
       (:descendant "a"
                    (= (:position) 2)))
      "descendant::a[position()=2]")
     ("42"
      (:path
       (:descendant "span"
                    (= (:position) (:last))))
      "descendant::span[position()=last()]")
     ("val1"
      (:path
       (:descendant "span" 1))
      "descendant::span[1]")
     ("42"
      (:path
       (:descendant "span" (:last)))
      "descendant::span[last()]")
     ("true" (< 1 2) "1 < 2")
     ("false" (> 1 2) "1 > 2")
     ("true"
      (<= (:path (:child "div")
                 (:child "span" (= (:path (:attribute "class")) "yetanother")))
          42)
      "div/span[@class='yetanother'] <= 42")
     ("false"
      (>= (:path (:child "div")
                 (:child "span" (= (:path (:attribute "class")) "yetanother")))
          43)
      "div/span[@class='yetanother'] >= 43")
     ("5"
      (:count (:path (:descendant "span")))
      "count(descendant::span)")
     ("val3"
      (:path (:child "div")
             (:child "a" (= (:count (:path (:child "span"))) 2)))
      "div/a[count(span)=2]")
     ("2"
      (:count (:path (:descendant * (= (:local-name) "a"))))
      "count(descendant::*[local-name()='a'])")
     ("span"
      (:local-name (:path (:descendant * (= (:path (:attribute "class")) "yetanother"))))
      "local-name(descendant::*[@class='yetanother'])")
     (""
      (:local-name (:path (:descendant "font")))
      "local-name(descendant::font)")
     ("class"
      (:local-name (:path (:descendant *) (:attribute "class")))
      "local-name(descendant::*/@class)")
     ("abc42qqq-false"
      (:concat "abc" 42 "qqq-" (:false))
      "concat('abc',42, 'qqq-', false())")
     ("false" (:false) "false()")
     ("true" (:true) "true()")
     ("false" (:boolean "") "boolean('')")
     ("true" (:boolean "aa") "boolean('aa')")
     ("42" (+ (:number "20") 22) "number('20') + 22")
     ;; test string(x)
     ("val1val2|||val3"
      (:path (:descendant "a"))
      "descendant::a")
     ("val1val2"
      (:string (:path (:descendant "a")))
      "string(descendant::a)")
     ("true"
      (:contains "needle-in-haystack" "needle")
      "contains('needle-in-haystack', 'needle')")
     ("1999"
      (:substring-before "1999/04/01" "/")
      "substring-before('1999/04/01', '/')")
     ("04/01"
      (:substring-after "1999/04/01" "/")
      "substring-after('1999/04/01', '/')")
     ("99/04/01"
      (:substring-after "1999/04/01" "19")
      "substring-after('1999/04/01', '19')")
     ("3"
      (:string-length "abc")
      "string-length('abc')")
     ("42"
      (:path (:descendant * (> (:path (:self *)) 41)))
      "descendant::*[.>41]")
     ("42"
      (* 2 (+ (- 100 90) 11))
      "2 * (100 - 90 + 11)")
     ("def" (:substring "abcdef" 4) "substring('abcdef', 4)")
     ("abc" (:substring "abcdef" 1 3) "substring('abcdef', 1, 3)")
     ("abc" (:substring "abcdef" 0 4) "substring('abcdef', 0, 4)")
     ("234" "substring('12345', 1.5, 2.6)")
     ("12" "substring('12345', 0, 3)")
     ("" "substring('12345', 0 div 0, 3)")
     ("" "substring('12345', 1, 0 div 0)")
     ("12345" "substring('12345', -42, 1 div 0)")
     ("" "substring('12345', -1 div 0, 1 div 0)")
     ("" "normalize-space('')" "normalize-space('   ')")
     ("abc def" "normalize-space('abc def')"
                "normalize-space('  abc  def')"
                "normalize-space('  abc  def  ')")
     ("47" "sum(//span[@id='s5']|//h4)")
     ("0" "sum(//span[.!=.])")
     ("NaN" "sum(//span)")
     ("" "concat(local-name(//text()[1]), name(//text()[1]), namespace-uri(//text()[1]))")
     ("true" "true() = 4" (= (:true) 4)))
    (verify-xpath*
     *sample-xml-3*
     ("W" (:id "w") "id('w')")
     ("X" (:id "x") "id('x')")
     ("Y" (:id "y") "id('y')")
     ("Z" (:id "z") "id('z')")
     ("W|||X|||Y|||Z" (:id "z y w x") "id('z y w x')")
     ("W|||Y|||Z" (:id "  z y w  ") "id('  z y w  ')")
     ("a" (:local-name (:id "w")) "local-name(id('w'))")
     ("W|||X|||Y" (:id (:path (:child "main") (:child "b"))) "id(main/b)"))))

(define-xpath-test test-with-namespaces-0               ;empty namespace need not be declared
  (eq (first-node (evaluate "/div" *sample-xml*))
      (funcall *document-element* *sample-xml*)))

(define-xpath-test test-with-namespaces-1
  (with-namespaces (("" ""))            ;can declare empty namespace
    (eq (first-node (evaluate "/div" *sample-xml*))
        (funcall *document-element* *sample-xml*))))

(define-xpath-test test-with-namespaces-2
  (with-namespaces (("foo" "http://special"))
    (eql 1 (length (all-nodes (evaluate "//foo:a" *sample-xml-2*))))))

(define-xpath-test test-with-namespaces-3
  (with-namespaces (("foo" "http://special"))
    (eql 2 (length (all-nodes (evaluate "//foo:*" *sample-xml-2*))))))

(define-xpath-test test-with-namespaces-4
  (handler-case
      (funcall (compile nil
                        `(lambda ()
                           (with-namespaces (("foo" "http://special"))
                             (evaluate "//bar:*" *sample-xml-2*)))))
    (error ()
      t)
    (:no-error (x)
      (error "test failed with return value ~A" x))))

(define-xpath-test test-with-variables-1
  (with-namespaces (("" ""))
    (with-variables (("foo" 3))
      (eql (number-value (evaluate "$foo" *sample-xml*)) 3))))

(define-xpath-test test-with-variables-2
  (with-namespaces (("" "")
                    ("ns" "http://foo"))
    (with-variables (("foo" 2)
                     ("ns:foo" 3))
      (eql (number-value (evaluate "$foo + $ns:foo" *sample-xml*)) 5))))

(define-xpath-test test-with-variables-3
  (handler-case
      (funcall (compile nil
                        `(lambda ()
                           (with-namespaces (("" ""))
                             (with-variables (("foo" 2)
                                              ("ns:foo" 3))
                               (evaluate "$foo" *sample-xml*))))))
    (error ()
      t)
    (:no-error (x)
      (error "test failed with return value ~A" x))))

(define-xpath-test test-computed-with-variables
  (with-namespaces (("" ""))
    (with-variables (("foo" (* 3 5)))
      (eql (number-value (evaluate "$foo" *sample-xml*)) 15))))

(define-xpath-test test-eager-with-variable-evaluation
  (let ((n 0))
    (with-namespaces (("" ""))
      (with-variables (("foo" (incf n)))
        (evaluate "$foo" *sample-xml*)))
    (assert-equal n 1)))

(define-xpath-test test-following
  (xpath:with-namespaces (("" ""))
    (assert-equal* 0 (xpath:evaluate "count(html/following::text())"
                                     (cxml:parse-rod "<html></html>"
                                                     *dom-builder*))
                   11.0d0 (xpath:evaluate "count(//following::div) * 10 + count(//div|body/div)"
                                          (cxml:parse-rod
                                           "<html><body><span></span><br/><div></div></body></html>"
                                           *dom-builder*)))))

(define-xpath-test test-filtering
  (with-namespaces (("" ""))
    (with-variables (("somevar" (evaluate "/div" *sample-xml*)))
      (assert-equal "another-value"
                    (evaluate "string($somevar/span[@class='another'])"
                              *sample-xml*)))))

(define-xpath-test test-node-set-api
  (labels ((join (list)
             (format nil "~{~a~^|||~}" list))
           (verify-results (expected-str xml)
             (let ((node-set (sample-node-set xml)))
               (assert-equal*
                expected-str
                (join
                 (mapcar #'get-node-text
                         (force (pipe-of node-set))))
                expected-str
                (join
                 (map-node-set->list #'get-node-text node-set))
                expected-str
                (let ((l '()))
                  (assert-equal nil
                                (map-node-set #'(lambda (node)
                                                  (push (get-node-text node) l))
                                              node-set))
                  (join (nreverse l)))
                expected-str
                (let ((l '()))
                  (do-node-set (node node-set (join (nreverse l)))
                    (push (get-node-text node) l)))
                expected-str
                (let ((l '()))
                  (assert-equal nil
                                (do-node-set (node node-set)
                                  (push (get-node-text node) l)))
                  (join (nreverse l)))
                expected-str
                (join
                 (loop for iter = (make-node-set-iterator node-set)
                         then (node-set-iterator-next iter)
                       while (not (node-set-iterator-end-p iter))
                       collect (get-node-text
                                (node-set-iterator-current iter))))))))
    (verify-results "" "")
    (verify-results "test" "<span>test</span>")
    (verify-results "test1|||test2" "<span>test1</span><div>test2</div>")
    (verify-results "test1|||test2|||test3"
                    "<span>test1</span><div>test2</div><p>test3</p>")
    (verify-results "test1|||test2|||test3|||test4"
                    "<span>test1</span><div>test2</div><p>test3</p><h1>test4</h1>")))

(define-xpath-test test-plx-extensions
  (with-plx-extensions
    (verify-xpath*
     *sample-xml*
     ("another-value|||42"
      (:path (:descendant "span" ((:qfunc "plx" "matches")
                                  (:path (:attribute "class"))
                                  "other$")))
      "descendant::span[plx:matches(@class, 'other$')]"
      "descendant::span[plx:matches(@class, 'OTHER$', 'i')]")
     ("another-value" ;; make sure compiled regex caching doesn't break anything
      "descendant::span[plx:matches('another', concat('^', @class, '$'))]")
     (""
      "descendant::span[plx:matches(@class, 'OTHER$')]")
     ("def--abc"
      "plx:replace('abc--def', '(a.*)--(d.*)', '\\2--\\1')"
      "plx:replace('abc--def', '(A.*)--(D.*)', '\\2--\\1', 'i')")
     ("true"
      (= ((:qfunc "plx" "current")) (:path (:root :node)))
      "plx:current() = /")
     ("another-value"
      "//span[@class='another' and plx:current()/div/@class = 'something']"
      "//span[@class='another' and plx:current() = /]")
     ("true"
      "plx:generate-id(//span[1]) = plx:generate-id(//span[@class='sample'])"
      "plx:generate-id(//span) = plx:generate-id(//span[@class='sample'])"
      "plx:generate-id(//span[2]) != plx:generate-id(//span[@class='sample'])"
      "plx:generate-id(//span[2]) != plx:generate-id(/)"
      "plx:generate-id() = plx:generate-id(/)"))))

(define-xpath-test test-pattern-case
  (flet ((check (expected-value expression)
           (let* ((node (first-node (evaluate expression *sample-xml*)))
                  (actual-value
                   (pattern-case node
                     ("div" :div)
                     ("span[@class='another']" :another)
                     ("*" :element)
                     (t :otherwise))))
             (unless (eql expected-value actual-value)
               (error "expected ~A but got ~A for ~A"
                      expected-value
                      actual-value
                      expression)))))
    (check :div "//div")
    (check :element "//span")
    (check :another "(//span)[4]")
    (check :otherwise "//span/@class")))

(define-xpath-test test-node-matches-p
  (flet ((check (expected match select &optional (document *sample-xml*))
           (let* ((node (first-node (evaluate select document)))
                  (actual (node-matches-p node match)))
             (unless (eql expected actual)
               (error "expected ~A but got ~A for ~A on ~A"
                      expected
                      actual
                      match
                      node)))))
    (check t "div" "//div")
    (check t "//div" "//div")
    (check t "*" "//span")
    (check t "span[@class='another']" "(//span)[4]")
    (check t "@*" "//span/@class")
    (check t "a//c" "/doc/a/e/b/c" *sample-xml-4*)
    (check t "//a//c" "/doc/a/e/b/c" *sample-xml-4*)
    (check t "B//X" "/doc/A/X/B/X/C/X" *sample-xml-4*)
    (check t "B//X" "/doc/A/X/B/X" *sample-xml-4*)
    (check t "X//X" "/doc/A/X/B/X" *sample-xml-4*)
    (check nil "B//B" "/doc/A/X/B" *sample-xml-4*)
    (check nil "span[@class='another']" "//span")
    (check nil "div" "//span")
    (check nil "*" "//span/@class")))

(deftest test-xmls
  (let ((*navigator* (cxml-xmls:make-xpath-navigator))
        (d
         '("foo" (("a" "b"))
           " "
           ("a" (("id" "1")))
           " " ("b" (("id" "2")))
           " " ("c" (("id" "3")))
           " " ("a" (("id" "4")))
           " " ("b" (("id" "5")))
           " " ("c" (("id" "6")))
           " " ("a" (("id" "7")))
           " " ("b" (("id" "8")))
           " " ("c" (("id" "9")))
           " " ("last" NIL))))
    (assert-equal
     '(("a" (("id" "1"))) ("c" (("id" "6"))))
     (all-nodes (evaluate "//c[position()=2]|//a[@id='1']" d)))))

(deftest mappend-lazy
  (let* ((x nil)
         (pipe (mappend-pipe (lambda (x) (list (- x)))
                             (make-pipe 1 (progn (setf x t) '(2))))))
    (assert (not x))
    (assert (eql -1 (pipe-head pipe)))
    (assert (eql -2 (pipe-head (pipe-tail pipe))))
    (assert x)))
