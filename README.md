# TypeScript Compiler

## Local Development

It's assumed a valid version of `ocaml` and `dune` are already installed in an OPAM switch

To install required dependencies, run the following for your `switch`:

```bash
opam install ounit2 qcheck odoc
```

To build the project, run:

```bash
dune build
```

To run the executable's entry point, run:

```bash
dune exec bin/main.exe
```

To run all tests, run:

```bash
dune test
```
