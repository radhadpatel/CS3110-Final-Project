MODULES=schedule classRoster iCalParser requirements command main authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,oUnit,str,qcheck,curl,ansiterminal,yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

check:
	bash checkenv.sh

run: build
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
	
finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

zip:
	zip scheduler_src.zip *.ml* *.sh* temp.html example.json _tags Makefile INSTALL.md .ocamlinit LOC.txt example.ics
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report scheduler_src.zip bisect*.out test_case_save.json
