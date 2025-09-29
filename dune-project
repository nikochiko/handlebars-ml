(lang dune 3.18)
(using menhir 3.0)

(name handlebars-ml)

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
 (depends ocaml
          yojson
          ISO8601)
 (tags
  ("handlebars" "template")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
