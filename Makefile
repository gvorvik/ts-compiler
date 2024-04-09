.PHONY: all clean build run test coverage fmt doc

all: build coverage doc

clean:
	@dune clean

build:
	@dune build

run:
	@dune exec bin/main.exe

test:
	@dune runtest

coverage:
	@dune runtest --instrument-with bisect_ppx --force
	@bisect-ppx-report html
	@echo "Coverage report generated in _coverage/index.html"
	@bisect-ppx-report summary

fmt:
	@dune fmt

doc:
	@dune build @doc
	@echo "Documentation generated in _build/default/_doc/_html/index.html"
