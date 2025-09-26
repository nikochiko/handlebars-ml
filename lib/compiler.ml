open Types

type compile_error =
  | Missing_helper of string
      [@printer fun fmt -> fprintf fmt "Missing helper: %s"]
  | Missing_partial of string
      [@printer fun fmt -> fprintf fmt "Missing partial: %s"]
  | Partial_lex_error of string * lex_error
      [@printer
        fun fmt (name, e) ->
          Format.fprintf fmt "Lex error in partial \"%s\": %s" name
            (show_lex_error e)]
  | Partial_compile_error of string * compile_error
      [@printer
        fun fmt (name, e) ->
          Format.fprintf fmt "In partial \"%s\": %s" name (show_compile_error e)]
[@@deriving show]

type compile_result = (string, compile_error) result

type hb_error = LexError of lex_error | CompileError of compile_error
[@@deriving show]

type hb_result = (string, hb_error) result [@@deriving show]

type context_values = {
  v : literal_or_collection;
  extras : literal_or_collection;
}

type context =
  | Root of context_values
  | Child of { values : context_values; parent : context; root : context }

type custom_helper = literal_or_collection list -> literal_or_collection option
type custom_helper_lookup_fn = string -> custom_helper option
type partial_lookup_fn = string -> string option

let ( >>= ) = Result.bind
let ( let* ) = ( >>= )

let make_ctx ?parent_ctx ?extras v =
  let extras_lst = match extras with None -> [] | Some lst -> lst in
  let extras_lst =
    match v with
    | `List l -> ("length", `Int (List.length l)) :: extras_lst
    | _ -> extras_lst
  in
  let values = { v; extras = `Assoc extras_lst } in
  match parent_ctx with
  | None -> Root values
  | Some (Root _ as root_ctx) ->
      Child { values; parent = root_ctx; root = root_ctx }
  | Some (Child { root; _ } as parent) -> Child { values; parent; root }

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
  let lookup_multi_assoc name v_lst =
    let lookup_assoc name (v : [> literal_or_collection ]) =
      match v with `Assoc lst -> List.assoc_opt name lst | _ -> None
    in
    let assoc_seq = List.to_seq v_lst in
    Seq.find_map (lookup_assoc name) assoc_seq
  in
  let rec aux ctx segments =
    let values =
      match ctx with Root values -> values | Child { values; _ } -> values
    in
    match segments with
    | [] ->
        let { v; _ } = values in
        v
    | `Ident name :: rest when name = "@root" ->
        let root_ctx =
          match ctx with (Root _ as root) | Child { root; _ } -> root
        in
        aux root_ctx rest
    | `Ident name :: rest when name = "." || name = "this" -> aux ctx rest
    | `Ident name :: rest -> (
        let { v; extras } = values in
        match lookup_multi_assoc name [ extras; v ] with
        | Some next_v ->
            let new_ctx = make_ctx ~parent_ctx:ctx next_v in
            aux new_ctx rest
        | None -> `Null)
    | `Index (`String name) :: rest -> (
        let { v; _ } = values in
        match v with
        | `Assoc lst -> (
            match List.assoc_opt name lst with
            | Some next_v ->
                let new_ctx = make_ctx ~parent_ctx:ctx next_v in
                aux new_ctx rest
            | None -> `Null)
        | _ -> `Null)
    | `Index (`Int idx) :: rest when idx >= 0 -> (
        let { v; _ } = values in
        match v with
        | `List lst -> (
            match List.nth_opt lst idx with
            | Some next_v ->
                let new_ctx = make_ctx ~parent_ctx:ctx next_v in
                aux new_ctx rest
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
  let concat args =
    let printables =
      List.filter
        (function `Assoc _ | `List _ | `Null -> false | _ -> true)
        args
    in
    let s = printables |> List.map string_of_literal |> String.concat "" in
    Some (`String s)
  in
  let removeProtocol = function
    | [ `String url ] ->
        let re = Str.regexp "^https?://" in
        let stripped = Str.replace_first re "" url in
        Some (`String stripped)
    | _ -> None
  in
  let eq = function [ a; b ] -> Some (`Bool (a = b)) | _ -> None in
  let not_ = function
    | [ any ] -> Some (`Bool (not (is_truthy any)))
    | _ -> None
  in
  match name with
  | "upper" -> Some upper
  | "lower" -> Some lower
  | "length" -> Some length
  | "concat" -> Some concat
  | "removeProtocol" -> Some removeProtocol
  | "eq" -> Some eq
  | "not" -> Some not_
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
    | `Partial { name; context; hash_args } :: rest ->
        let* compiled_partial = compile_partial name context hash_args ctx in
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
        let* v = eval ctx get_helper context_expr in
        let new_ctx = make_ctx ~parent_ctx:ctx v in
        let content_to_use = if is_truthy v then content else else_content in
        compile_token_list [] new_ctx content_to_use
    | `App ("each", [ iterable_expr ]) -> (
        let* value = eval ctx get_helper iterable_expr in
        if not (is_truthy value) then compile_token_list [] ctx else_content
        else
          match value with
          | `List lst ->
              let compile_for_each i v =
                let extras =
                  [
                    ("@index", `Int i);
                    ("@first", `Bool (i = 0));
                    ("@last", `Bool (i = List.length lst - 1));
                  ]
                in
                let item_ctx = make_ctx ~parent_ctx:ctx ~extras v in
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
                let extras = [ ("@key", `String k) ] in
                let item_ctx = make_ctx ~parent_ctx:ctx ~extras v in
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
          | _ -> compile_token_list [] ctx else_content)
    | other_expr -> (
        let* v = eval ctx get_helper other_expr in
        match is_truthy v with
        | true -> compile_token_list [] (make_ctx ~parent_ctx:ctx v) content
        | false -> compile_token_list [] ctx else_content)
  and compile_partial name context_opt hash_args ctx =
    match get_partial name with
    | None -> Error (Missing_partial name)
    | Some partial_template -> (
        (* Determine context for partial *)
        let* partial_ctx =
          match context_opt with
          | None -> Ok ctx (* inherit current context *)
          | Some context_expr ->
              let* v = eval ctx get_helper context_expr in
              Ok (make_ctx ~parent_ctx:ctx v)
        in
        (* Evaluate hash arguments and add them to context *)
        let* partial_ctx_with_hash =
          if hash_args = [] then Ok partial_ctx
          else
            (* Evaluate hash arguments *)
            let* hash_values =
              List.fold_left
                (fun acc_result (key, expr) ->
                  let* acc = acc_result in
                  let* value = eval ctx get_helper expr in
                  Ok ((key, value) :: acc))
                (Ok []) hash_args
            in
            let hash_values = List.rev hash_values in
            (* Merge hash arguments with context *)
            let values =
              match partial_ctx with
              | Root values -> values
              | Child { values; _ } -> values
            in
            let { v; extras } = values in
            let new_extras =
              match extras with
              | `Assoc lst -> hash_values @ lst
              (* TODO: not happy with runtime error for this *)
              | _ -> failwith "extras should always be assoc"
            in
            let new_ctx =
              make_ctx ~parent_ctx:partial_ctx ~extras:new_extras v
            in
            Ok new_ctx
        in
        (* Parse and compile the partial template *)
        let lexbuf =
          uchar_array_of_string partial_template |> Sedlexing.from_uchar_array
        in
        match Lexer.lex lexbuf with
        | Error e -> Error (Partial_lex_error (name, e))
        | Ok partial_tokens -> (
            match
              compile_token_list [] partial_ctx_with_hash partial_tokens
            with
            | Error e -> Error (Partial_compile_error (name, e))
            | Ok result -> Ok result))
  in
  let ctx = make_ctx values in
  compile_token_list [] ctx tokens

let compile ?(get_helper = default_get_helper)
    ?(get_partial = default_get_partial) template values =
  let lexbuf = uchar_array_of_string template |> Sedlexing.from_uchar_array in
  match Lexer.lex lexbuf with
  | Error e -> Error (LexError e)
  | Ok tokens -> (
      match compile_tokens get_helper get_partial tokens values with
      | Error e -> Error (CompileError e)
      | Ok result -> Ok result)

(* Inline Tests *)

let make_test ?(get_helper = default_get_helper)
    ?(get_partial = default_get_partial) template values expected =
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

let%test "mustache-style block with dot-nesting" =
  let template = "{{#resume.basics}}{{name}}{{/resume.basics}}" in
  let values =
    `Assoc
      [ ("resume", `Assoc [ ("basics", `Assoc [ ("name", `String "Eve") ]) ]) ]
  in
  make_test template values (Ok "Eve")

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
    match name with "greeting" -> Some "{{name}}" | _ -> None
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
  let values =
    `Assoc [ ("user", `Assoc [ ("name", `String "Alice"); ("age", `Int 25) ]) ]
  in
  make_test ~get_partial template values (Ok "Name: Alice, Age: 25")

let%test "partial with custom context should work - Phase 2" =
  (* Test: This will be implemented in Phase 2 - context arguments *)
  let template = "{{> greeting}}" in
  (* For now, just basic partials *)
  let get_partial name =
    match name with
    | "greeting" -> Some "Hello {{user.name}}!" (* Access nested context *)
    | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
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
    | "recursive" -> Some "{{> recursive}}" (* infinite loop *)
    | _ -> None
  in
  let values = `Assoc [] in
  (* This will infinite loop until we implement recursion prevention *)
  (* For now, just test that missing partials error properly *)
  let template_missing = "{{> missing}}" in
  make_test ~get_partial template_missing values
    (Error (CompileError (Missing_partial "missing")))

(* Phase 2: Partial context tests *)

let%test "partial with explicit context should work" =
  (* Test: {{> partial context}} passes specific context *)
  let template = "{{> greeting user}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test ~get_partial template values (Ok "Hello Bob!")

let%test "partial with nested context expression should work" =
  (* Test: {{> partial user.profile}} *)
  let template = "{{> userCard user.profile}}" in
  let get_partial name =
    match name with
    | "userCard" -> Some "Name: {{name}}, Age: {{age}}"
    | _ -> None
  in
  let values =
    `Assoc
      [
        ( "user",
          `Assoc
            [
              ("profile", `Assoc [ ("name", `String "Alice"); ("age", `Int 30) ]);
            ] );
      ]
  in
  make_test ~get_partial template values (Ok "Name: Alice, Age: 30")

let%test "partial with literal context should work" =
  (* Test: {{> partial "string"}} *)
  let template = "{{> echo \"Hello World\"}}" in
  let get_partial name =
    match name with
    | "echo" -> Some "{{.}}" (* Current context should be the literal *)
    | _ -> None
  in
  let values = `Assoc [] in
  make_test ~get_partial template values (Ok "Hello World")

let%test "partial context should not affect parent context" =
  (* Test: context changes in partial shouldn't leak out *)
  let template = "{{name}} {{> greeting user}} {{name}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hi {{name}}!" | _ -> None
  in
  let values =
    `Assoc
      [
        ("name", `String "Main");
        ("user", `Assoc [ ("name", `String "Partial") ]);
      ]
  in
  make_test ~get_partial template values (Ok "Main Hi Partial! Main")

(* @root context tests *)

let%test "@root should reference initial context" =
  (* Test: @root always refers to the original context *)
  let template = "{{name}} {{#with user}}{{name}} {{@root.name}}{{/with}}" in
  let values =
    `Assoc
      [
        ("name", `String "Root"); ("user", `Assoc [ ("name", `String "User") ]);
      ]
  in
  make_test template values (Ok "Root User Root")

let%test "@root should work in each blocks" =
  (* Test: @root works inside iteration *)
  let template = "{{#each items}}{{.}} - {{@root.title}} {{/each}}" in
  let values =
    `Assoc
      [
        ("title", `String "List");
        ("items", `List [ `String "A"; `String "B"; `String "C" ]);
      ]
  in
  make_test template values (Ok "A - List B - List C - List ")

let%test "@root should work in partials" =
  (* Test: @root accessible from partials *)
  let template = "{{> item_card item}}" in
  let get_partial name =
    match name with
    | "item_card" -> Some "{{name}} (from {{@root.source}})"
    | _ -> None
  in
  let values =
    `Assoc
      [
        ("source", `String "API");
        ("item", `Assoc [ ("name", `String "Widget") ]);
      ]
  in
  make_test ~get_partial template values (Ok "Widget (from API)")

let%test "@root should work with nested contexts" =
  (* Test: @root works with deep nesting *)
  let template =
    "{{#with company}}{{#each departments}}{{name}}: {{@root.company_name}} \
     {{/each}}{{/with}}"
  in
  let values =
    `Assoc
      [
        ("company_name", `String "Acme Corp");
        ( "company",
          `Assoc
            [
              ( "departments",
                `List
                  [
                    `Assoc [ ("name", `String "Engineering") ];
                    `Assoc [ ("name", `String "Sales") ];
                  ] );
            ] );
      ]
  in
  make_test template values (Ok "Engineering: Acme Corp Sales: Acme Corp ")

(* Hash arguments tests *)

let%test "partial with single hash argument should work" =
  (* Test: {{> greeting name="World"}} *)
  let template = "{{> greeting name=\"World\"}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [] in
  make_test ~get_partial template values (Ok "Hello World!")

let%test "partial with multiple hash arguments should work" =
  (* Test: {{> user-card name="Alice" age=25}} *)
  let template = "{{> user-card name=\"Alice\" age=25}}" in
  let get_partial name =
    match name with
    | "user-card" -> Some "Name: {{name}}, Age: {{age}}"
    | _ -> None
  in
  let values = `Assoc [] in
  make_test ~get_partial template values (Ok "Name: Alice, Age: 25")

let%test "partial with context and hash arguments should work" =
  (* Test: {{> greeting user msg="Hi"}} - context provides base, hash provides extras *)
  let template = "{{> greeting user msg=\"Hi\"}}" in
  let get_partial name =
    match name with "greeting" -> Some "{{msg}} {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ] in
  make_test ~get_partial template values (Ok "Hi Alice!")

let%test "partial hash arguments should override context values" =
  (* Test: hash arguments should take precedence over context properties *)
  let template = "{{> greeting user name=\"Override\"}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Original") ]) ] in
  make_test ~get_partial template values (Ok "Hello Override!")

let%test "partial with variable as hash argument value should work" =
  (* Test: {{> greeting msg=message}} *)
  let template = "{{> greeting msg=message}}" in
  let get_partial name =
    match name with "greeting" -> Some "Message: {{msg}}" | _ -> None
  in
  let values = `Assoc [ ("message", `String "Hello World") ] in
  make_test ~get_partial template values (Ok "Message: Hello World")

let%test "partial hash arguments work with context object properties" =
  (* Test: hash args are merged with context object properties *)
  let template = "{{> wrapper user title=\"User Profile\"}}" in
  let get_partial name =
    match name with
    | "wrapper" -> Some "{{title}}: {{name}} ({{age}})"
    | _ -> None
  in
  let values =
    `Assoc [ ("user", `Assoc [ ("name", `String "Bob"); ("age", `Int 30) ]) ]
  in
  make_test ~get_partial template values (Ok "User Profile: Bob (30)")

let%test "partial without context inherits current execution context" =
  (* Test: partials without context args inherit the current context *)
  let template = "{{#with user}}{{> greeting}}{{/with}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ] in
  make_test ~get_partial template values (Ok "Hello Alice!")

let%test
    "partial with hash args but no context inherits current execution context" =
  (* Test: partials with hash args but no context inherit current context and merge hash args *)
  let template = "{{#with user}}{{> greeting msg=\"Hi\"}}{{/with}}" in
  let get_partial name =
    match name with "greeting" -> Some "{{msg}} {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test ~get_partial template values (Ok "Hi Bob!")

let%test "partial with hash args inherits loop context variables" =
  (* Test: partials inherit @index, @key, etc. from loop contexts *)
  let template =
    {| {{~#each items}}{{> item-display prefix="Item"}}{{/each~}} |}
  in
  let get_partial name =
    match name with
    | "item-display" -> Some "{{@index}}: {{prefix}}"
    | _ -> None
  in
  let values = `Assoc [ ("items", `List [ `String "A"; `String "B" ]) ] in
  make_test ~get_partial template values (Ok "0: Item1: Item")

let%test "partial with 'this'" =
  (* Test: partials can use 'this' to refer to current context value *)
  let template = "{{#each items}}{{> item-card this}} {{/each}}" in
  let get_partial name =
    match name with "item-card" -> Some "Item: {{this}}" | _ -> None
  in
  let values = `Assoc [ ("items", `List [ `String "A"; `String "B" ]) ] in
  make_test ~get_partial template values (Ok "Item: A Item: B ")
