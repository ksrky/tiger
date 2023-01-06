(lang dune 3.6)

(name tiger)

(generate_opam_files true)

(source
 (github ksrky/tiger))

(authors "ksrk")

(maintainers "ksrk")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tiger)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.0)
