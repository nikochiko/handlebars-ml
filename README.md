# handlebars-ml

Handlebars templating for OCaml.

## Installation

Install with `opam`:

```bash
opam install handlebars-ml
```

## CLI usage

```bash
opam install handlebars-ml
handlebars-ml -d data.json template.hbs
```

Optionally, pass a partials directory with `-p partials_dir`.

Pass the `-h` (or `--help`) flag to see all options.


```bash
handlebars-ml -h
```

## Library usage

Use `Yojson` values for passing JSON data as template context.

```ocaml
let () =
    let data = Yojson.Safe.from_string {| { "name": "World" } |} in
    let template = "Hello, {{name}}!" in
    match Handlebars_ml.Compiler.compile template data with
    | Ok result -> print_endline result
    | Error err -> prerr_endline ("Error: " ^ err)
```

Pass `get_helper` and `get_partial` functions as named arguments to
`compile` to use custom helpers and partials. Use JSON types for
arguments and return values.

```ocaml
let custom_get_helper name =
    let shout = function
    | [ `String s ] -> Some (`String (String.uppercase_ascii arg))
    | _ -> None
    in
    match name with
    | "shout" -> Some shout
    | _ -> None

let () =
    let data = Yojson.Safe.from_string {| { "name": "World" } |} in
    let template = "Hello, {{shout name}}!" in
    match Handlebars_ml.Compiler.compile ~get_helper:custom_get_helper template data with
    | Ok result -> print_endline result
    | Error err -> prerr_endline ("Error: " ^ err)
```

`handlebars-ml` provides a `default_get_helper` function in
`Handlebars_ml.Compiler` that can be used in your custom implementation
to still keep the default helpers.

## License

MIT
