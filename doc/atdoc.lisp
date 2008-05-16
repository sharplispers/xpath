(asdf:operate 'asdf:load-op :xpath)
(asdf:operate 'asdf:load-op :atdoc)

;;
;; HTML (multipage)
;;
(let* ((base (asdf:component-pathname (asdf:find-system :xpath)))
       (atdoc-directory (merge-pathnames "doc/atdoc/" base)))
  (ensure-directories-exist atdoc-directory)
  (atdoc:generate-html-documentation
   '(:xpath :xpath-sys :xpattern)
   atdoc-directory
   :index-title "Plexippus XPath API reference"
   :heading "Plexippus XPath"

   ;; suppress pages for unexported symbols, which we don't really need.
   ;; ... and Xuriella is currently too slow for so many pages.
   :include-internal-symbols-p nil

   ;; For now, use atdoc's new default stylesheet
   ;; (I think it looks a little better than the old stuff.)
   ;; :css (merge-pathnames "doc/atdoc.css" base)
   ))

;;
;; HTML (single page)
;;
#+(or)
(let* ((base (asdf:component-pathname (asdf:find-system :xpath)))
       (atdoc-directory (merge-pathnames "doc/atdoc/" base)))
  (ensure-directories-exist atdoc-directory)
  (atdoc:generate-html-documentation
   '(:xpath :xpath-sys :xpattern)
   atdoc-directory
   :index-title "Plexippus XPath API reference"
   :heading "Plexippus XPath"
   :single-page-p t))

;;
;; LaTeX/PDF
;;

#+(or)
(let* ((base (asdf:component-pathname (asdf:find-system :xpath)))
       (atdoc-directory (merge-pathnames "doc/atdoc/" base)))
  (ensure-directories-exist atdoc-directory)
  (atdoc:generate-latex-documentation
   '(:xpath :xpath-sys :xpattern)
   atdoc-directory
   :title "Plexippus XPath"))
