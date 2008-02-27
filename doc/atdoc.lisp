(asdf:operate 'asdf:load-op :xpath)
(asdf:operate 'asdf:load-op :atdoc)
(atdoc:generate-documentation
 '(:xpath :xpath-sys)
 "/home/ivan4th/devlibs/plexippus-xpath/doc/atdoc/"
 :index-title "Plexippus XPath API reference"
 :heading "Plexippus XPath"
 :css "/home/ivan4th/devlibs/plexippus-xpath/doc/index.css")
