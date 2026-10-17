(lang dune 3.23)
(name reforge)
(version 0.1.0)

(generate_opam_files true)

(package
 (name reforge)
 (synopsis "Compiler-aware refactoring engine for OCaml repositories")
 (description
  "reforge analyzes large OCaml codebases, models their architecture \
   (files, modules, dune scopes, dependency graphs) and performs safe, \
   repeatable refactorings through explicit, validatable change plans.")
 (depends
  ocaml
  dune
  cmdliner
  yojson
  compiler-libs
  alcotest))
