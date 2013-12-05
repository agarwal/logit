INSTALL_DIR=$(HOME)/bin

all: logit.native

install: logit.native
	cp src/_build/logit.native $(INSTALL_DIR)/logit

logit.native:
	cd src; ocamlbuild $@

devel-doc: clean-devel-doc
	cd src; ocamlbuild logit.docdir/index.html; rm -f logit.docdir
	mkdir -p doc/html/logit
	cp -fR src/_build/logit.docdir/* doc/html/logit/

clean-devel-doc:
	rm -rf doc/html

clean: clean-devel-doc
	cd src; ocamlbuild -clean; rm -f logit.native
