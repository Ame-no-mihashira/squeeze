(lang dune 3.17)

(name squeeze)

(generate_opam_files true)

(source
 (github Ame-no-mihashira/squeeze))

(authors "Ame-no-mihashira <>")

(maintainers "Ame-no-mihashira <>")

(license MIT)

(package
 (name squeeze)
 (synopsis "Class schedule visualization software")
 (depends ocaml ppx_deriving uucp sexplib ppx_sexp_conv))
