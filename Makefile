.PHONY: test check

build:
	dune build
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe