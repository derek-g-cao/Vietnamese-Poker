.PHONY: test check

build:
	dune build

clean:
	dune clean

count:
	cloc --by-file --include-lang=OCaml .
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe