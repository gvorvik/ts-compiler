(lang dune 3.14)

(name ts_compiler)

(version 0.0.0)

(generate_opam_files true)

(source
 (github gvorvik/ts-compiler))

(authors "Greg Orvik" "Ismail Elshakankiry")

(maintainers "Greg Orvik" "Ismail Elshakankiry")

(license MIT)

(documentation https://github.com/gvorvik/ts-compiler)

(package
 (name ts_compiler)
 (synopsis "A TypeScript to machine code compiler")
 (description "A TypeScript to machine code compiler")
(depends
   (ocaml (>= "5.0.0"))
   dune
   (ounit2 (>= "2.2.7"))
   (qcheck (>= "0.21.3"))
   (odoc (>= "2.0.0")))
 (tags
  (compiler typescript machine-code)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
