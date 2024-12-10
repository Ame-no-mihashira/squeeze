(lang dune 3.17)

(name squeeze)

(generate_opam_files true)

(authors "Gabriel Guevara <>")

(license MIT)

(package
 (name squeeze)
 (synopsis "Class schedule display library")
 (depends ocaml uucp ppx_deriving yojson ppx_yojson_conv))
