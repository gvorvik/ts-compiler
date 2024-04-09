# TypeScript Compiler

## Local Development

It's assumed a valid version of `ocaml` and `dune` are already installed in an OPAM switch

To install required dependencies, run the following for your `switch`:

```bash
opam install ounit2 qcheck odoc bisect_ppx
```

### Building the project

```bash
dune build
```

- Or `make build`

### Running the executable

```bash
dune exec bin/main.exe
```

- Or `make run`

### Running tests

```bash
dune test
```

- Or `make test`

### Generating coverage reports

```bash
make coverage
```

Then open the `.html` file in your browser

### Formatting files

```bash
dune fmt
```

- Or `make fmt`

### Generating doc page

```bash
make doc
```

Then open the `.html` file in your browser
