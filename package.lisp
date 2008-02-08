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

	   #:node-type-p

	   #:base-uri))

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
	   #:evaluate-thunk
	   #:xpath
	   #:first-node
	   #:all-nodes
           #:map-node-set
	   #:map-node-set->list
           #:do-node-set
           #:make-node-set-iterator
           #:node-set-iterator-end-p
           #:node-set-iterator-next
           #:node-set-iterator-current
           #:node-set-p
           #:node-set-empty-p
	   #:node-set

	   #:context
	   #:make-context
	   #:context-node

	   #:environment-find-namespace
	   #:environment-find-variable

	   #:xpath-error))
