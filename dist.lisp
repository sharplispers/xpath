(asdf:operate 'asdf:load-op :atdoc)
(asdf:operate 'asdf:load-op :xpath)

(atdoc:generate-html-documentation
 '(:xpath)
 (merge-pathnames
  "doc/atdoc/"
  (asdf:component-relative-pathname (asdf:find-system :xpath)))
 :heading "Plexippus XPath"
 :single-page-p t)
