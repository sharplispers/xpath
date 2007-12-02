(in-package :cl-user)

(defpackage :xpath-protocol
  (:use)
  (:export #:child-pipe
	   #:attribute-pipe
	   #:namespace-pipe

           #:node-p
	   #:parent-node
	   #:local-name
	   #:qualified-name
	   #:namespace-prefix
	   #:namespace-uri
	   #:string-value
	   #:processing-instruction-target

	   #:node-type-p))

(defpackage :xpath
  (:use cl)
  (:export #:compile-xpath
	   #:parse-xpath

	   #:boolean-value
	   #:string-value
           #:number-value

	   #:with-namespaces
	   #:with-variables
	   #:evaluate
	   #:xpath
	   #:first-node
	   #:all-nodes
           #:map-node-set
           #:do-node-set
	   #:node-set

	   #:environment-find-namespace
	   #:environment-find-variable))
