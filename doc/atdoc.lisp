(asdf:operate 'asdf:load-op :xpath)
(asdf:operate 'asdf:load-op :atdoc)
(let* ((base (asdf:component-pathname (asdf:find-system :xpath)))
       (atdoc-directory (merge-pathnames "doc/atdoc/" base)))
  (ensure-directories-exist atdoc-directory)
  (atdoc:generate-documentation
   '(:xpath :xpath-sys :xpattern)
   atdoc-directory
   :index-title "Plexippus XPath API reference"
   :heading "Plexippus XPath"
   :css (merge-pathnames "doc/atdoc.css" base)))
