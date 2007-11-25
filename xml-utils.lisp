(in-package :xpath)

(defun get-node-text (node)
  (xpath-protocol:string-value node))

;; pipe-related

(defun vector->pipe (vector &optional (start 0))
  (if (>= start (length vector))
      empty-pipe
      (make-pipe (elt vector start)
		 (vector->pipe vector (1+ start)))))


;; some hairy hacks follow that make it easier for me to work with CXML DOM from REPL

(defun xml-serialize (xml &key skip-xmlns indent-p canonical-p)
  (dom:map-document (cxml:make-whitespace-normalizer
                     (cxml:make-string-sink :canonical canonical-p
                                            :indentation (and indent-p 4)
                                            :width 1000))
                    (ensure-document xml)
		    :include-xmlns-attributes (not skip-xmlns)))

(defun xml->string/headless (item &key skip-xmlns indent-p canonical-p)
  (cl-ppcre:regex-replace (rx "^<\\?.*?\\?>\\s+" :single-line-mode t) ;; FIXME: somewhat hairy approach
                          (xml-serialize item :skip-xmlns skip-xmlns :indent-p indent-p :canonical-p canonical-p) ""))

(defun sx (item) ;; for REPL -- hairy!
  (princ (xml->string/headless item :skip-xmlns t :indent-p t))
  (values))

(defun element->document (element)
  (let ((new-document (dom:create-document (dom:implementation (dom:owner-document element)) nil nil nil)))
    (dom:append-child new-document (dom:import-node new-document element t))
    new-document))

(defun ensure-document (item)
  (etypecase item
    (dom:element (element->document item))
    (dom:document item)))
