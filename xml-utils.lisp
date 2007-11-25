(in-package :xpath)

(defun get-node-text (node) ;; FIXME: support document and document-fragment
  (with-output-to-string (s)
    (labels ((write-text (node)
	       (let ((value (dom:node-value node)))
		 (when value (write-string value s))
                 (unless (dom:attribute-p node) ;; FIXME: verify CDATA sections
                   (dom:do-node-list (child (dom:child-nodes node))
                     (cond ((or (dom:element-p child)
                                (dom:entity-reference-p child))
                            (write-text child))
                           ((or (dom:text-node-p child)
                                (dom:attribute-p child)
                                (dom:cdata-section-p child))
                            (write-string (dom:node-value child) s))))))))
      (write-text node))))

;; pipe-related

(defun dom-node-list->pipe (node-list &optional (start 0))
  (if (>= start (dom:length node-list))
      empty-pipe
      (make-pipe (dom:item node-list start)
		 (dom-node-list->pipe node-list (1+ start)))))

(defun dom-child-nodes->pipe (node) (dom-node-list->pipe (dom:child-nodes node)))

(defun dom-attributes->pipe (node)
  (if (dom:attributes node)
      (dom-node-list->pipe (dom:attributes node)))) ;; fixme: is this correct? (node map)

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
