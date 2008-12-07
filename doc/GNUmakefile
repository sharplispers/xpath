USER=ishvedunov

all: index.html installation.html examples.html

%.html: %.xml index.xsl
	xsltproc index.xsl $< >$@.tmp
	mv $@.tmp $@
	chmod -w $@

.PHONY: push
push: all
	scp index.html index.css $(USER)@common-lisp.net:/project/plexippus-xpath/public_html/plexippus-xpath/doc/
