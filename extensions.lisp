;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

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
  (check-type name symbol)
  `(%define-extension ',name ,uri ,documentation))

(defun find-xpath-function (local-name uri)
  "@arg[local-name]{local part of the function name}
   @arg[uri]{namespace URI of the function}
   @short{Performs an XPath function lookup using standard lookup rules}

   All defined extensions for the namespace specified by @code{uri}
   are scanned for function with specified @code{local-name}."
  (loop for ext in (gethash uri *extensions*)
        for match = (gethash local-name (extension-functions ext))
        when match
          do (return match)))

(defun add-xpath-function (ext name func)
  (let ((real-name (etypecase name
                     (string name)
                     (symbol (string-downcase name)))))
    (setf (gethash real-name
                   (extension-functions
                    (or (get ext 'xpath-extension)
                        (error "no such extension: ~s" ext))))
          func)))

(defmacro define-xpath-function/lazy (ext name args &body body)
  (with-gensyms (thunks)
    (let ((func-name (hypsym 'xfd name)))
      `(progn
         (defun ,func-name (,thunks)
           (declare (ignorable ,thunks))
           ,(if (null args)
                `(locally ,@body)
                `(destructuring-bind ,args ,thunks ,@body)))
         (add-xpath-function ',ext ',name ',func-name)))))

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
         (add-xpath-function ',ext ',name ',func-name)))))

(defmacro define-xpath-function/eager (ext name args &body body)
  (with-gensyms (thunk)
    `(%define-xpath-function/eager ,ext ,name
         #'(lambda (,thunk) (funcall ,thunk context))
         ,args ,@body)))

(defmacro define-xpath-function/single-type (ext name type args &body body)
  (check-type type (member boolean number string node-set))
  (with-gensyms (thunk)
    (let ((value-func-name (hypsym type 'value)))
      `(%define-xpath-function/eager ,ext ,name
           #'(lambda (,thunk) (,value-func-name (funcall ,thunk context)))
           ,args ,@body))))

