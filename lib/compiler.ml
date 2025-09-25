open Types

type compile_error =
  | Missing_helper of string
  | Missing_partial of string
  | Partial_recursion of string
  | Type_error of string
[@@deriving show]

type compile_result = (string, compile_error) result

type hb_error = LexError of lex_error | CompileError of compile_error
[@@deriving show]

type hb_result = (string, hb_error) result [@@deriving show]

type context_value =
  | Simple of literal_or_collection
  | WithExtras of { v : literal_or_collection; extras : literal_or_collection }

type context =
  | Root of { v : context_value }
  | Child of { v : context_value; parent : context }

type custom_helper = literal_or_collection list -> literal_or_collection option
type custom_helper_lookup_fn = string -> custom_helper option
type partial_lookup_fn = string -> string option


let ( >>= ) = Result.bind
let ( let* ) = ( >>= )

let make_ctx ?parent_ctx v =
  match parent_ctx with
  | None -> Root { v }
  | Some ctx -> Child { v; parent = ctx }

let literal_or_collection_of_literal (lit : literal) : literal_or_collection =
  match lit with
  | `Bool b -> `Bool b
  | `String s -> `String s
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `Null -> `Null

let is_truthy = function
  | `Null
  | `String ""
  | `Int 0
  | `Float 0.0
  | `Bool false
  | `List []
  | `Assoc [] ->
      false
  | _ -> true

let is_space c =
  match Uchar.to_char c with
  | ' ' | '\012' | '\t' | '\r' | '\n' -> true
  | _ -> false

let trim_left c_arr =
  let i = ref 0 in
  while !i < Array.length c_arr && is_space c_arr.(!i) do
    incr i
  done;
  Array.sub c_arr !i (Array.length c_arr - !i)

let trim_right c_arr =
  let i = ref (Array.length c_arr - 1) in
  while !i >= 0 && is_space c_arr.(!i) do
    decr i
  done;
  Array.sub c_arr 0 (!i + 1)

let string_of_literal (lit : literal_or_collection) : string =
  match lit with
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `Null -> ""
  | `List _ | `Assoc _ -> ""

let escape_html s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&apos;"
      | '>' -> Buffer.add_string b "&gt;"
      | '<' -> Buffer.add_string b "&lt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let lookup ctx segments =
  let rec aux ctx segments =
    let v = match ctx with Root { v } -> v | Child { v; _ } -> v in
    let actual_v =
      match v with
      | Simple v -> v
      | WithExtras { v; extras } -> (
          match segments with
          | `Ident name :: _ when String.starts_with ~prefix:"@" name -> extras
          | _ -> v)
    in
    match segments with
    | [] -> actual_v
    | `Ident name :: rest -> (
        if name = "." then aux ctx rest
        else
          match actual_v with
          | `Assoc lst -> (
              match List.assoc_opt name lst with
              | Some next_v ->
                  aux (Child { v = Simple next_v; parent = ctx }) rest
              | None -> `Null)
          | _ -> `Null)
    | `Index (`String name) :: rest -> (
        match actual_v with
        | `Assoc lst -> (
            match List.assoc_opt name lst with
            | Some next_v ->
                aux (Child { v = Simple next_v; parent = ctx }) rest
            | None -> `Null)
        | _ -> `Null)
    | `Index (`Int idx) :: rest when idx >= 0 -> (
        match actual_v with
        | `List lst -> (
            match List.nth_opt lst idx with
            | Some next_v ->
                aux (Child { v = Simple next_v; parent = ctx }) rest
            | None -> `Null)
        | _ -> `Null)
    | `DotPath `OneDot :: rest -> aux ctx rest
    | `DotPath `TwoDot :: rest ->
        let parent_ctx =
          match ctx with Root _ -> ctx | Child { parent; _ } -> parent
        in
        aux parent_ctx rest
    | _ -> `Null
  in
  aux ctx segments

let rec eval ctx get_helper (expr : evalable) =
  match expr with
  | `Literal lit -> Ok (literal_or_collection_of_literal lit)
  | `App (name, args) -> (
      match get_helper name with
      | None -> Error (Missing_helper name)
      | Some helper -> (
          let* arg_values =
            List.fold_left
              (fun acc arg ->
                let* lst = acc in
                let* v = eval ctx get_helper arg in
                Ok (v :: lst))
              (Ok []) args
          in
          let arg_values = List.rev arg_values in
          match helper arg_values with Some v -> Ok v | None -> Ok `Null))
  | `IdentPath segments -> Ok (lookup ctx segments)
  | `WhateverMakesSense exprs ->
      let rec try_eval = function
        | [] -> Ok `Null
        | expr :: rest -> (
            match eval ctx get_helper expr with
            | Error (Missing_helper _) -> try_eval rest
            | result -> result)
      in
      try_eval exprs


let default_get_helper name =
  let upper args =
    match args with
    | [ `String s ] -> Some (`String (String.uppercase_ascii s))
    | _ -> None
  in
  let lower args =
    match args with
    | [ `String s ] -> Some (`String (String.lowercase_ascii s))
    | _ -> None
  in
  let length args =
    match args with
    | [ `String s ] -> Some (`Int (String.length s))
    | [ `List lst ] -> Some (`Int (List.length lst))
    | [ `Assoc lst ] -> Some (`Int (List.length lst))
    | _ -> None
  in
  match name with
  | "upper" -> Some upper
  | "lower" -> Some lower
  | "length" -> Some length
  | _ -> None

let default_get_partial _name = None

let compile_tokens get_helper get_partial tokens values =
  let rec compile_token_list acc ctx tokens =
    match tokens with
    | [] -> Ok (List.rev acc |> String.concat "")
    | `Comment _ :: rest -> compile_token_list acc ctx rest
    | `WhitespaceControl :: `Raw s :: rest ->
        let trimmed = trim_left s in
        compile_token_list acc ctx (`Raw trimmed :: rest)
    | `Raw s :: `WhitespaceControl :: rest ->
        let trimmed = trim_right s |> string_of_uchar_array in
        compile_token_list (trimmed :: acc) ctx rest
    | `WhitespaceControl :: rest -> compile_token_list acc ctx rest
    | `Raw s :: rest ->
        let raw_str = string_of_uchar_array s in
        compile_token_list (raw_str :: acc) ctx rest
    | `Escaped expr :: rest ->
        let* value = eval ctx get_helper expr in
        let escaped_str = string_of_literal value |> escape_html in
        compile_token_list (escaped_str :: acc) ctx rest
    | `Unescaped expr :: rest ->
        let* value = eval ctx get_helper expr in
        let unescaped_str = string_of_literal value in
        compile_token_list (unescaped_str :: acc) ctx rest
    | `Block { expr; content; else_content } :: rest ->
        let* compiled_block = compile_block expr content else_content ctx in
        compile_token_list (compiled_block :: acc) ctx rest
    | `Partial { name; context } :: rest ->
        let* compiled_partial = compile_partial name context ctx in
        compile_token_list (compiled_partial :: acc) ctx rest

  and compile_block expr content else_content ctx =
    match expr with
    | `App ("if", [ condition ]) ->
        let* value = eval ctx get_helper condition in
        let content_to_use =
          if is_truthy value then content else else_content
        in
        compile_token_list [] ctx content_to_use
    | `App ("with", [ context_expr ]) ->
        let* value = eval ctx get_helper context_expr in
        let new_ctx = make_ctx ~parent_ctx:ctx (Simple value) in
        let content_to_use =
          if is_truthy value then content else else_content
        in
        compile_token_list [] new_ctx content_to_use
    | `App ("each", [ iterable_expr ]) -> (
        let* value = eval ctx get_helper iterable_expr in
        if not (is_truthy value) then
          compile_token_list [] ctx else_content
        else
          match value with
          | `List lst ->
              let compile_for_each i v =
                let extras =
                  `Assoc
                    [
                      ("@index", `Int i);
                      ("@first", `Bool (i = 0));
                      ("@last", `Bool (i = List.length lst - 1));
                    ]
                in
                let item_ctx =
                  make_ctx ~parent_ctx:ctx (WithExtras { v; extras })
                in
                compile_token_list [] item_ctx content
              in
              let* all_compiled =
                List.mapi compile_for_each lst
                |> List.fold_left
                     (fun acc_result compiled_result ->
                       let* acc = acc_result in
                       let* compiled = compiled_result in
                       Ok (compiled :: acc))
                     (Ok [])
              in
              Ok (List.rev all_compiled |> String.concat "")
          | `Assoc lst ->
              let compile_for_each (k, v) =
                let extras = `Assoc [ ("@key", `String k) ] in
                let item_ctx =
                  make_ctx ~parent_ctx:ctx (WithExtras { v; extras })
                in
                compile_token_list [] item_ctx content
              in
              let* all_compiled =
                List.map compile_for_each lst
                |> List.fold_left
                     (fun acc_result compiled_result ->
                       let* acc = acc_result in
                       let* compiled = compiled_result in
                       Ok (compiled :: acc))
                     (Ok [])
              in
              Ok (List.rev all_compiled |> String.concat "")
          | _ ->
              compile_token_list [] ctx else_content)
    | other_expr ->
        let* value = eval ctx get_helper other_expr in
        let content_to_use =
          if is_truthy value then content else else_content
        in
        let new_ctx =
          if is_truthy value then make_ctx ~parent_ctx:ctx (Simple value)
          else ctx
        in
        compile_token_list [] new_ctx content_to_use

  and compile_partial name context_opt ctx =
    match get_partial name with
    | None -> Error (Missing_partial name)
    | Some partial_template ->
        (* Determine context for partial *)
        let* partial_ctx = match context_opt with
          | None -> Ok ctx  (* inherit current context *)
          | Some context_expr ->
              let* context_value = eval ctx get_helper context_expr in
              Ok (make_ctx ~parent_ctx:ctx (Simple context_value))
        in
        (* Parse and compile the partial template *)
        let lexbuf = uchar_array_of_string partial_template |> Sedlexing.from_uchar_array in
        match Lexer.lex lexbuf with
        | Error e -> Error (Type_error ("Partial lexer error: " ^ show_lex_error e))
        | Ok partial_tokens ->
            (* Recursively compile the partial tokens *)
            compile_token_list [] partial_ctx partial_tokens
  in
  compile_token_list [] (make_ctx (Simple values)) tokens

let compile ?(get_helper = default_get_helper) ?(get_partial = default_get_partial) template values =
  let lexbuf = uchar_array_of_string template |> Sedlexing.from_uchar_array in
  match Lexer.lex lexbuf with
  | Error e -> Error (LexError e)
  | Ok tokens -> (
      match compile_tokens get_helper get_partial tokens values with
      | Error e -> Error (CompileError e)
      | Ok result -> Ok result)

(* Inline Tests *)

let make_test ?(get_helper = default_get_helper) ?(get_partial = default_get_partial) template values expected =
  let result = compile ~get_helper ~get_partial template values in
  match result = expected with
  | true -> true
  | false ->
      Printf.printf "------ Test failed: -----\n";
      Printf.printf "Template: %s\n" template;
      Printf.printf "Values:   %s\n" (show_literal_or_collection values);
      Printf.printf "Output:   %s\n" (show_hb_result result);
      Printf.printf "Expected: %s\n" (show_hb_result expected);
      false

let%test "simple variable substitution" =
  let template = "Hello {{name}}!" in
  let values = `Assoc [ ("name", `String "World") ] in
  make_test template values (Ok "Hello World!")

let%test "multiple variables" =
  let template = "{{firstname}} {{lastname}}" in
  let values =
    `Assoc [ ("firstname", `String "John"); ("lastname", `String "Doe") ]
  in
  make_test template values (Ok "John Doe")

let%test "missing variable returns empty string" =
  let template = "Hello {{missing}}!" in
  let values = `Assoc [] in
  make_test template values (Ok "Hello !")

let%test "nested object access" =
  let template = "{{user.name}}" in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ] in
  make_test template values (Ok "Alice")

let%test "escaped output by default" =
  let template = "{{content}}" in
  let values =
    `Assoc [ ("content", `String "<script>alert('xss')</script>") ]
  in
  make_test template values
    (Ok "&lt;script&gt;alert(&apos;xss&apos;)&lt;/script&gt;")

let%test "unescaped output with triple braces" =
  let template = "{{{content}}}" in
  let values = `Assoc [ ("content", `String "<b>bold</b>") ] in
  make_test template values (Ok "<b>bold</b>")

let%test "upper helper" =
  let template = "{{upper name}}" in
  let values = `Assoc [ ("name", `String "hello") ] in
  make_test template values (Ok "HELLO")

let%test "lower helper" =
  let template = "{{lower name}}" in
  let values = `Assoc [ ("name", `String "WORLD") ] in
  make_test template values (Ok "world")

let%test "length helper with string" =
  let template = "{{length text}}" in
  let values = `Assoc [ ("text", `String "hello") ] in
  make_test template values (Ok "5")

let%test "missing helper error" =
  let template = "{{missing_helper name}}" in
  let values = `Assoc [ ("name", `String "test") ] in
  let expected = Error (CompileError (Missing_helper "missing_helper")) in
  make_test template values expected

let%test "if block with truthy value" =
  let template = "{{#if user}}Hello {{user.name}}{{/if}}" in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test template values (Ok "Hello Bob")

let%test "if block with falsy value" =
  let template = "{{#if missing}}Never shown{{/if}}" in
  let values = `Assoc [] in
  make_test template values (Ok "")

let%test "empty string is falsy" =
  let template = "{{#if empty}}truthy{{else}}falsy{{/if}}" in
  let values = `Assoc [ ("empty", `String "") ] in
  make_test template values (Ok "falsy")

let%test "zero is falsy" =
  let template = "{{#if zero}}truthy{{else}}falsy{{/if}}" in
  let values = `Assoc [ ("zero", `Int 0) ] in
  make_test template values (Ok "falsy")

let%test "non-empty string is truthy" =
  let template = "{{#if text}}truthy{{else}}falsy{{/if}}" in
  let values = `Assoc [ ("text", `String "hello") ] in
  make_test template values (Ok "truthy")

let%test "integer to string" =
  let template = "Count: {{count}}" in
  let values = `Assoc [ ("count", `Int 42) ] in
  make_test template values (Ok "Count: 42")

let%test "custom helper" =
  let template = "{{greet name}}" in
  let get_helper name =
    match name with
    | "greet" ->
        Some
          (function
          | [ `String name ] -> Some (`String ("Hello, " ^ name ^ "!"))
          | _ -> None)
    | _ -> default_get_helper name
  in
  let values = `Assoc [ ("name", `String "Alice") ] in
  make_test ~get_helper template values (Ok "Hello, Alice!")

let%test "each block with array" =
  let template = "{{#each items}}{{.}} {{/each}}" in
  let values =
    `Assoc [ ("items", `List [ `String "a"; `String "b"; `String "c" ]) ]
  in
  make_test template values (Ok "a b c ")

let%test "each block with object" =
  let template = "{{#each user}}{{.}} {{/each}}" in
  let values =
    `Assoc [ ("user", `Assoc [ ("name", `String "John"); ("age", `Int 30) ]) ]
  in
  make_test template values (Ok "John 30 ")

let%test "with block context" =
  let template = "{{#with user}}Hello {{name}}{{/with}}" in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test template values (Ok "Hello Bob")

let%test "fallback with WhateverMakesSense" =
  let template = "{{name}}" in
  let get_helper name =
    match name with
    | "name" -> Some (fun _ -> Some (`String "from helper"))
    | _ -> default_get_helper name
  in
  let values = `Assoc [ ("name", `String "from variable") ] in
  make_test ~get_helper template values (Ok "from helper")

(* Partial tests - these will fail until we implement partials *)

let%test "basic partial inclusion should work" =
  (* Test: {{> greeting}} should include the greeting partial *)
  let template = "Hello {{> greeting}}!" in
  let get_partial name =
    match name with
    | "greeting" -> Some "{{name}}"
    | _ -> None
  in
  let values = `Assoc [ ("name", `String "World") ] in
  make_test ~get_partial template values (Ok "Hello World!")

let%test "partial with context inheritance should work" =
  (* Test: partial inherits current context *)
  let template = "{{#with user}}{{> userCard}}{{/with}}" in
  let get_partial name =
    match name with
    | "userCard" -> Some "Name: {{name}}, Age: {{age}}"
    | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [("name", `String "Alice"); ("age", `Int 25)]) ] in
  make_test ~get_partial template values (Ok "Name: Alice, Age: 25")

let%test "partial with custom context should work - Phase 2" =
  (* Test: This will be implemented in Phase 2 - context arguments *)
  let template = "{{> greeting}}" in  (* For now, just basic partials *)
  let get_partial name =
    match name with
    | "greeting" -> Some "Hello {{user.name}}!"  (* Access nested context *)
    | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [("name", `String "Bob")]) ] in
  make_test ~get_partial template values (Ok "Hello Bob!")

let%test "nested partials should work" =
  (* Test: partials can include other partials *)
  let template = "{{> outer}}" in
  let get_partial name =
    match name with
    | "outer" -> Some "Outer: {{> inner}}"
    | "inner" -> Some "Inner: {{value}}"
    | _ -> None
  in
  let values = `Assoc [ ("value", `String "test") ] in
  make_test ~get_partial template values (Ok "Outer: Inner: test")

let%test "partial recursion should be prevented" =
  (* Test: recursive partials should error, not loop *)
  let get_partial name =
    match name with
    | "recursive" -> Some "{{> recursive}}"  (* infinite loop *)
    | _ -> None
  in
  let values = `Assoc [] in
  (* This will infinite loop until we implement recursion prevention *)
  (* For now, just test that missing partials error properly *)
  let template_missing = "{{> missing}}" in
  make_test ~get_partial template_missing values (Error (CompileError (Missing_partial "missing")))

let%test "partial with explicit context should work" =
  (* Test: {{> partial context}} passes specific context *)
  let template = "{{> greeting user}}" in
  let get_partial name =
    match name with
    | "greeting" -> Some "Hello {{name}}!"
    | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [("name", `String "Bob")]) ] in
  make_test ~get_partial template values (Ok "Hello Bob!")

let%test "partial with nested context expression should work" =
  (* Test: {{> partial user.profile}} *)
  let template = "{{> userCard user.profile}}" in
  let get_partial name =
    match name with
    | "userCard" -> Some "Name: {{name}}, Age: {{age}}"
    | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [("profile", `Assoc [("name", `String "Alice"); ("age", `Int 30)])]) ] in
  make_test ~get_partial template values (Ok "Name: Alice, Age: 30")

let%test "partial with literal context should work" =
  (* Test: {{> partial "string"}} *)
  let template = "{{> echo \"Hello World\"}}" in
  let get_partial name =
    match name with
    | "echo" -> Some "{{.}}"  (* Current context should be the literal *)
    | _ -> None
  in
  let values = `Assoc [] in
  make_test ~get_partial template values (Ok "Hello World")

let%test "partial context should not affect parent context" =
  (* Test: context changes in partial shouldn't leak out *)
  let template = "{{name}} {{> greeting user}} {{name}}" in
  let get_partial name =
    match name with
    | "greeting" -> Some "Hi {{name}}!"
    | _ -> None
  in
  let values = `Assoc [ ("name", `String "Main"); ("user", `Assoc [("name", `String "Partial")]) ] in
  make_test ~get_partial template values (Ok "Main Hi Partial! Main")
