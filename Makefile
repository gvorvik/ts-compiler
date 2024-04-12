.PHONY: all clean build run test coverage fmt doc

all: build coverage doc

install:
	@opam install . --deps-only --with-test

clean:
	@opam exec -- dune clean

build:
	@opam exec -- dune build

run:
	@opam exec -- dune exec bin/main.exe

test:
	@opam exec -- dune runtest

coverage:
	@opam exec -- dune runtest --instrument-with bisect_ppx --force
	@opam exec -- bisect-ppx-report html
	@echo "Coverage report generated in _coverage/index.html"
	@opam exec -- bisect-ppx-report summary

fmt:
	@opam exec -- dune fmt

doc:
	@opam exec -- dune build @doc
	@echo "Documentation generated in _build/default/_doc/_html/index.html"
