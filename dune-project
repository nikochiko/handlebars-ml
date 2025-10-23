(lang dune 3.18)

(name handlebars-ml)
(version 0.2.1)

(generate_opam_files true)

(source
 (github nikochiko/handlebars-ml))

(authors "Kaustubh M <ktvm42@gmail.com>")

(maintainers "Kaustubh M <ktvm42@gmail.com>")

(license MIT)

(documentation https://github.com/nikochiko/handlebars-ml)

(package
 (name handlebars-ml)
 (synopsis "Handlebars templating for OCaml")
 (description "OCaml port of the Handlebars.js templating engine")
 (depends (ocaml (>= 4.14))
          (yojson (>= 2.0))
          ppx_deriving
          (alcotest :with-test))
 (tags
  ("handlebars" "template")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
