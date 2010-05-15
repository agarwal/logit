all: doc

# for testing only, delete this
main.native:
	cd src; ocamlbuild $@

doc: clean-doc
	cd src; ocamlbuild logit.docdir/index.html; rm -f logit.docdir
	mkdir -p doc/html/logit
	cp -fR src/_build/logit.docdir/* doc/html/logit/

clean:
	cd src; ocamlbuild -clean

clean-doc:
	rm -rf doc/html

fresh: clean clean-doc
