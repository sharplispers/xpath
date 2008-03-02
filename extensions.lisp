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

;; XPath extensions (function groups)

(defvar *extensions* (make-hash-table :test #'equal))

(defstruct (extension
            (:constructor make-extension
                          (uri documentation &optional
                               (functions (make-hash-table :test #'equal)))))
  uri documentation functions)

(defstruct (xpath-function
            (:constructor make-xpath-function (compiler name min-args max-args)))
  compiler name min-args max-args)

(defun compile-xpath-function-call (xpath-function argument-thunks)
  (unless (<= (xpath-function-min-args xpath-function)
              (length argument-thunks)
              (xpath-function-max-args xpath-function))
   (xpath-error "invalid number of arguments -- ~a for function ~a"
                (length argument-thunks)
                (xpath-function-name xpath-function)))
  (let ((func (funcall (xpath-function-compiler xpath-function) argument-thunks))
        (name (xpath-function-name xpath-function)))
    #'(lambda (context)
        (funcall func context))))

(defun %define-extension (name uri documentation)
  (check-type uri string)
  (let* ((current-ext (get name 'xpath-extension))
         (new-ext
          (cond (current-ext
                 (setf (gethash (extension-uri current-ext) *extensions*)
                       (remove current-ext
                               (gethash (extension-uri current-ext)
                                        *extensions*))
                       (extension-uri current-ext) uri
                       (extension-documentation current-ext) documentation)
                 current-ext)
                (t
                 (setf (get name 'xpath-extension)
                       (make-extension uri documentation))))))
    (push new-ext (gethash uri *extensions*))))

(defmacro define-extension (name uri &optional documentation)
  "@arg[name]{the name of XPath extension (a symbol)}
   @arg[uri]{URI corresponding to XPath extension (a string)}
   @arg[documentation]{documentation string for the XPath extension}
   @short{Defines an XPath extension with specified @code{name} and @code{uri}.}

   An XPath extension is a collection of XPath functions that are defined
   using one of @fun{define-xpath-function/lazy},
   @fun{define-xpath-function/eager} or @fun{define-xpath-function/single-type}
   macros. In order to use the extension, one must bind a prefix string to
   its @code{uri} using @fun{with-namespaces} macro.

   Example:
   @begin{pre}
   (defparameter *my-namespace* \"http://example.net/my-xpath-extension\")

   (xpath-sys:define-extension
       my-ext *my-namespace*
     \"My Extension\")

   (xpath-sys:define-xpath-function/single-type my-ext add-quotes string (string)
     (concat \"\\\"\" string \"\\\"\"))

   (defun get-my-quoted-string(doc)
     (with-namespaces ((\"my\" *my-namespace*))
       (evaluate \"add-quotes(//some-element)\" doc)))
   @end{pre}"
  (check-type name symbol)
  `(%define-extension ',name ,uri ,documentation))

(defun find-xpath-function (local-name uri)
  "@arg[local-name]{local part of the function name}
   @arg[uri]{namespace URI of the function}
   @return[uri]{an XPath function object}
   @short{Performs an XPath function lookup using standard lookup rules}

   All defined extensions for the namespace specified by @code{uri}
   are scanned for function with specified @code{local-name}."
  (loop for ext in (gethash uri *extensions*)
        for match = (gethash local-name (extension-functions ext))
        when match
          do (return match)))

(defun arg-count (arglist)
  (let ((bad-kw (find-if #'(lambda (arg)
                             (and
                              (not (eq arg '&rest))
                              (not (eq arg '&optional))
                              (and (symbolp arg)
                                   (char= (char (symbol-name arg) 0) #\&))))
                         arglist)))
    (when bad-kw
      (error "lambda list keyword ~a isn't supported by XPath functions"
             bad-kw)))
  (let ((rest-pos (position '&rest (remove '&optional arglist)))
        (opt-pos (position '&optional arglist)))
    (cond (rest-pos
           (values (arg-count (subseq arglist 0 rest-pos))
                   most-positive-fixnum))
          (opt-pos
           (values
            (position '&optional arglist)
            (1- (length arglist))))
          (t
           (values
            (length arglist)
            (length arglist))))))

(defun add-xpath-function (ext name func arglist)
  (let ((real-name (etypecase name
                     (string name)
                     (symbol (string-downcase name)))))
    (multiple-value-bind (min-args max-args)
        (arg-count arglist)
      (setf (gethash real-name
                     (extension-functions
                      (or (get ext 'xpath-extension)
                          (error "no such extension: ~s" ext))))
            (make-xpath-function func real-name min-args max-args)))))

;; FIXME: atdoc: handle slashes properly
(defmacro define-xpath-function/lazy (ext name args &body body)
  "@arg[ext]{name of an XPath extension (a symbol)}
   @arg[name]{XPath function name}
   @arg[args]{XPath function arguments}
   @short{Defines an XPath function, \"lazy\" style.}

   The @code{body} is evaluated during compilation of XPath
   expressions each time the function being defined is referenced.
   It's passed a list of \"thunks\" corresponding to XPath function arguments
   and should return a new \"thunk\". A \"thunk\" is a function that takes
   an XPath @class{context} as argument and returns value of one of XPath
   types (string, boolean, number, node set).

   Example:
   @begin{pre}
   (define-xpath-function/lazy my-ext my-if (v if-part else-part)
     #'(lambda (ctx)
         (if (boolean-value (funcall v ctx))
             (funcall if-part ctx)
             (funcall else-part ctx))))
   @end{pre}
   @see{define-xpath-extension}
   @see{define-xpath-function/eager}
   @see{define-xpath-function/single-type}"
  (with-gensyms (thunks)
    (let ((func-name (hypsym 'xfd name)))
      `(progn
         (defun ,func-name (,thunks)
           (declare (ignorable ,thunks))
           ,(if (null args)
                `(locally ,@body)
                `(destructuring-bind ,args ,thunks ,@body)))
         (add-xpath-function ',ext ',name ',func-name ',args)))))

(defmacro %define-xpath-function/eager (ext name converter args &body body)
  (with-gensyms (thunks)
    (let ((func-name (hypsym 'xfd name)))
      `(progn
         (defun ,func-name (,thunks)
           (declare (ignorable ,thunks))
           #'(lambda (context)
               (declare (ignorable context))
               ,(if (null args)
                    `(locally ,@body)
                    `(destructuring-bind ,args (mapcar ,converter ,thunks)
                       ,@body))))
         (add-xpath-function ',ext ',name ',func-name ',args)))))

(defmacro define-xpath-function/eager (ext name args &body body)
  "@arg[ext]{name of an XPath extension (a symbol)}
   @arg[name]{XPath function name}
   @arg[args]{XPath function arguments}
   @short{Defines an XPath function, \"eager\" style.}

   The @code{body} is evaluated during evaluation of XPath
   expressions each time the function being defined is called.
   It's passed a list of values corresponding to XPath function arguments
   and should return a value of one of XPath types (string, boolean, number,
   node set).

   Example:
   @begin{pre}
   (define-xpath-function/eager my-ext join (delim node-set)
     (reduce (lambda (a b) (concatenate 'string a delim b))
             (map-node-set->list #'string-value node-set)))
   @end{pre}
   @see{define-xpath-extension}
   @see{define-xpath-function/lazy}
   @see{define-xpath-function/single-type}"
  (with-gensyms (thunk)
    `(%define-xpath-function/eager ,ext ,name
         #'(lambda (,thunk) (funcall ,thunk context))
         ,args ,@body)))

(defmacro define-xpath-function/single-type (ext name type args &body body)
  "@arg[ext]{name of an XPath extension (a symbol)}
   @arg[name]{XPath function name}
   @arg[args]{XPath function arguments}
   @short{Defines an XPath function, \"eager\" style with automatic type conversion.}

   The @code{body} is evaluated during evaluation of XPath
   expressions each time the function being defined is called.
   It's passed a list of values corresponding to XPath function arguments
   and should return a value of one of XPath types (string, boolean, number,
   node set). Argument values are automatically converted to specified XPath @code{type}.

   Example:
   @begin{pre}
   (xpath-sys:define-xpath-function/single-type my-ext add-quotes string (string)
     (concat \"\\\"\" string \"\\\"\"))
   @end{pre}
   @see{define-xpath-extension}
   @see{define-xpath-function/lazy}
   @see{define-xpath-function/eager}"
  (check-type type (member boolean number string node-set))
  (with-gensyms (thunk)
    (let ((value-func-name (intern (concat (string-upcase type) "-VALUE")
                                   :xpath)))
      `(%define-xpath-function/eager ,ext ,name
           #'(lambda (,thunk) (,value-func-name (funcall ,thunk context)))
           ,args ,@body))))

