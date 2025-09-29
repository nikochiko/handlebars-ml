open Handlebars_lexer

let (let*) = Result.bind
let (>>=) = Result.bind

let equal_pos a b =
  a.Lexing.pos_cnum = b.Lexing.pos_cnum
  && a.Lexing.pos_bol = b.Lexing.pos_bol
  && a.Lexing.pos_lnum = b.Lexing.pos_lnum
let equal_buf _ _ = true
type parse_error = { msg : string; pos : Lexing.position [@equal equal_pos]; buf : Lexing.lexbuf [@equal equal_buf]} [@@deriving eq]

let string_of_path_segment segment =
  match segment with
  | `Ident name -> name
  | `DotPath `OneDot -> "."
  | `DotPath `TwoDot -> ".."
  | `Index (`String s) -> Printf.sprintf "['%s']" s
  | `Index (`Int i) -> Printf.sprintf "[%d]" i
  | _ -> failwith "not implemented"

let string_of_ident_path (path : Types.ident_path) =
  let aux path =
    let (`IdentPath segments) = path in
    match segments with
    | [] -> failwith "ident path shouldn't be empty"
    | _ ->
        segments
        |> List.fold_left
             (fun (prev, acc) this ->
               let s_this = string_of_path_segment this in
               match (prev, this) with
               | Some (`DotPath _), _ -> (Some this, acc ^ "/" ^ s_this)
               | Some _, `DotPath _ -> (Some this, acc ^ "/" ^ s_this)
               | Some _, `Ident _ -> (Some this, acc ^ "." ^ s_this)
               | _, _ -> (Some this, acc ^ s_this))
             (None, "")
  in
  let _, result = aux path in
  result

let string_of_literal lit : string =
  match lit with
  | `String s -> s
  | `Int i -> string_of_int i
  | _ -> failwith "not implemented"

let rec string_of_evalable (evalable : Types.evalable) =
  match evalable with
  | `IdentPath path -> string_of_ident_path (`IdentPath path)
  | `App (name, args) -> (
      match args with
      | [] -> name
      | _ ->
          let args_str =
            args |> List.map string_of_evalable |> String.concat ", "
          in
          Printf.sprintf "%s(%s)" name args_str)
  | `WhateverMakesSense exprs -> string_of_evalable (List.hd exprs)
  | `Literal l -> string_of_literal l

let mk_err msg buf : parse_error =
  { msg; pos = buf.Lexing.lex_curr_p; buf }

let pp_position fmt pos =
  Format.fprintf fmt "line %d, column %d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let pp_parse_error fmt { msg; pos; _ } =
  pp_position fmt pos;
  Format.fprintf fmt ": %s" msg

let show_parse_error e =
  pp_parse_error Format.str_formatter e;
  Format.flush_str_formatter ()

type parse_result = (Types.token list, parse_error) result [@@deriving show, eq]

let mlex f buf =
  try
    Ok (f buf)
  with Failure msg -> Error (mk_err msg buf)

type container =
  | Root of Types.token list
  | Unclosed of {
      parent: container;
      block: Types.block;
  }
  | UnclosedInverted of {
      parent: container;
      block: Types.block;
  }

let mk_block evalable = { Types.expr = evalable; content = []; else_content = [] }

let add_token container token =
  match container with
  | Root acc -> Root (token :: acc)
  | Unclosed { parent; block } ->
      let block = { block with content = token :: block.content } in
      Unclosed { parent; block }
  | UnclosedInverted { parent; block } ->
      let block = { block with else_content = token :: block.else_content } in
      UnclosedInverted { parent; block }

let (++) = add_token

let mature_unclosed container =
  match container with
  | Unclosed { parent; block } | UnclosedInverted { parent; block } ->
    let block = { block with content = List.rev block.content; else_content = List.rev block.else_content } in
    parent ++ `Block block
  | _ -> failwith "Cannot mature a non-unclosed block"

let invert_unclosed container =
  match container with
  | Unclosed { parent; block } ->
      UnclosedInverted { parent; block }
  | UnclosedInverted _ -> failwith "This looks like an extra 'else' block in the template"
  | _ -> failwith "Cannot invert a non-unclosed block"

let parse_literal (lit : Yojson.Basic.t) : Types.literal =
  match lit with
  | (`String s) -> `String s
  | (`Int i) -> `Int i
  | (`Float f) -> `Float f
  | _ -> failwith "Cannot lift non-literal to literal"

let parse_path_segment token : Types.ident_path_segment =
  match token with
  | IDENT id -> `Ident id
  | INDEX (`String s) -> `Index (`String s)
  | INDEX (`Int i) -> `Index (`Int i)
  | ONEDOT -> `DotPath `OneDot
  | TWODOT -> `DotPath `TwoDot

let mk_closing_path expr =
  let rec aux = function
    | `IdentPath path -> Some (`IdentPath path)
    | `App (name, _) -> Some (`IdentPath [ `Ident name ])
    | `WhateverMakesSense exprs ->
        let expr_seq = List.to_seq exprs in
        Seq.find_map aux expr_seq
    | _ -> None
  in
  match aux expr with
  | Some path -> path
  | None -> failwith "mk_closing_path: no path found in expr"

let rec parse_root container buf =
let* lexres = mlex lex buf in
match lexres with
| EOF -> (match container with
    | Root acc -> Ok (List.rev acc)
    | Unclosed _ | UnclosedInverted _ -> Error (mk_err "unmatched block (did you forget an {{/}}?)" buf))
| WHITESPACE s -> parse_root (container ++ `Whitespace s) buf
| RAW s -> parse_root (container ++ `Raw s) buf
| TEMPL_OPEN { ws_control; kind } -> (
    let container = if ws_control then
      container ++ `WhitespaceControl
    else container in
    match kind with
    | Escaped -> parse_escaped container buf
    | Unescaped -> parse_unescaped container buf
    | Partial ->
        let* partial_info, ws_control = parse_partial buf in
        let container = container ++ `Partial partial_info in
        let container = if ws_control then
          container ++ `WhitespaceControl
        else container in
        parse_root container buf
    | Section -> parse_section container buf
    | InvertedSection -> parse_inverted_section container buf
    | CloseSection -> parse_close_section container buf)
| COMMENT -> parse_root (container ++ `Comment) buf
| _ -> Error (mk_err "unexpected token in root" buf)

and parse_escaped container buf =
  let* inner, ws_control = parse_templ buf in
  let* container = match inner with
    | `App ("else", []) -> (try Ok (invert_unclosed container) with Failure e -> Error (mk_err e buf))
    | _ -> Ok (container ++ `Escaped inner)
  in
  let container = if ws_control then
    container ++ `WhitespaceControl
  else container in
  parse_root container buf

and parse_unescaped container buf =
  let* inner, ws_control = parse_templ ~is_unescaped:true buf in
  let container = container ++ `Unescaped inner in
  let container = if ws_control then
    container ++ `WhitespaceControl
  else container in
  parse_root container buf

and parse_templ ?(is_unescaped = false) buf =
  let expect_templ_close result = function
    | TEMPL_CLOSE { ws_control; is_unescaped = t_is_unescaped; raw } ->
        if is_unescaped <> t_is_unescaped then
          let expected = if is_unescaped then templ_open ^ String.make 1 templ_open_char else templ_open in
          let msg = Printf.sprintf "expected closing tag \"%s\" but found \"%s\"" expected raw in
          Error (mk_err msg buf)
        else Ok (result, ws_control)
    | _ -> Error (mk_err "expected closing tag" buf)
  in
  mlex lex_in_templ buf >>= function
  | IDENT_PATH [ IDENT name ] when name = "else" ->
      let expr = `App ("else", []) in
      mlex lex_in_templ buf >>= expect_templ_close expr
  | IDENT_PATH [ IDENT name ] | IDENT_PATH [ INDEX (`String name) ] | LITERAL (`String name) -> (
      let until = function | TEMPL_CLOSE _ -> true | _ -> false in
      (* TODO: add support for hash args for helpers *)
      let* (pos_args, _hash_args, close_token) = parse_arguments ~until buf in
      let expr = match pos_args with
      | [] ->`WhateverMakesSense [ `App (name, []); `IdentPath [ `Ident name ] ]
      | _ -> `App (name, pos_args)
      in
      close_token |> expect_templ_close expr)
  | LPAREN ->
      let* (name, pos_args, _hash_args, _last) = parse_application ~until:(equal_token RPAREN) buf in
      mlex lex_in_templ buf >>= expect_templ_close (`App (name, pos_args))
  | IDENT_PATH path ->
      let expr = `IdentPath (List.map parse_path_segment path) in
      mlex lex_in_templ buf >>= expect_templ_close expr
  | LITERAL (`Int i) ->
      let expr = `IdentPath [`Index (`Int i)] in
      mlex lex_in_templ buf >>= expect_templ_close expr
  | token ->
      let msg = Printf.sprintf "unexpected token: %s" (show_token token) in
      Error (mk_err msg buf)

and parse_application ~until buf =
  mlex lex_in_templ buf >>= function
  | IDENT_PATH [ IDENT name ] | IDENT_PATH [ INDEX (`String name) ] | LITERAL (`String name) ->
      let* (pos_args, hash_args, last_token) = parse_arguments ~until buf in
      Ok (name, pos_args, hash_args, last_token)
  | _ -> Error (mk_err "expected function name in application" buf)

and parse_arguments ~until buf =
  let parse_arg = function
    | IDENT_PATH path -> Ok (`IdentPath (List.map parse_path_segment path))
    | LITERAL lit -> Ok (`Literal (parse_literal lit))
    | LPAREN ->
        let* (name, pos_args, _hash_args, _t) = parse_application ~until:(equal_token RPAREN) buf in
        Ok (`App (name, pos_args))
    | token ->
        let msg = Printf.sprintf "unexpected token: \"%s\"" (show_token token) in
        Error (mk_err msg buf)
  in
  let rec aux pos_args hash_args buf =
    mlex lex_in_templ buf >>= function
    | t when until t -> Ok (List.rev pos_args, List.rev hash_args, t)
    | START_HASH_ARG name ->
        let* v = mlex lex_in_templ buf >>= parse_arg in
        aux pos_args ((name, v) :: hash_args) buf
    | t ->
        let* arg = parse_arg t in
        aux (arg :: pos_args) hash_args buf
  in
  aux [] [] buf

and parse_section container buf =
  let* evalable, ws_control = parse_templ buf in
  let block = mk_block evalable in
  let child = Unclosed { parent = container; block } in
  let child = if ws_control then
    child ++ `WhitespaceControl
  else child in
  parse_root child buf

and parse_inverted_section container buf =
  let* evalable, ws_control = parse_templ buf in
  let block = mk_block evalable in
  let child = UnclosedInverted { parent = container; block } in
  let child = if ws_control then
    child ++ `WhitespaceControl
  else child in
  parse_root child buf

and parse_close_section container buf =
  let* inner, ws_control = parse_templ buf in
  match container with
  | Root _ -> Error (mk_err "unexpected close block without matching open" buf)
  | Unclosed { block; _ } | UnclosedInverted { block; _ } -> (
      let expected_closing = mk_closing_path block.expr in
      let rec make_sense = function
        | `IdentPath path -> Some (`IdentPath path)
        | `WhateverMakesSense exprs ->
            let expr_seq = List.to_seq exprs in
            Seq.find_map make_sense expr_seq
        | _ -> None
      in
      match make_sense inner with
      | Some path when Types.equal_evalable path expected_closing ->
          let container = mature_unclosed container in
          let container = if ws_control then
            container ++ `WhitespaceControl
          else container in
          parse_root container buf
      | _ ->
          let msg = Printf.sprintf "unexpected close block: \"%s\"; does not match \"%s\""
            (string_of_evalable inner)
            (string_of_evalable expected_closing)
          in
          Error (mk_err msg buf))

and parse_partial buf =
  let until = function | TEMPL_CLOSE _ -> true | _ -> false in
  let* (name, pos_args, hash_args, last_token) = parse_application ~until buf in
  let* ws_control = match last_token with
    | TEMPL_CLOSE { ws_control; _ } -> Ok ws_control
    | _ -> Error (mk_err "expected closing tag" buf)
  in
  let partial_info = match pos_args with
  | [] -> { Types.name; context = None; hash_args }
  | [ context ] -> { Types.name; context = Some context; hash_args }
  | _ -> failwith "A partial takes exactly 1 positional argument for context" 
  in
  Ok (partial_info, ws_control)


let parse lexbuf : parse_result =
  parse_root (Root []) lexbuf

(* *)

let make_test input expected =
  let buf = Lexing.from_string input in
  let result = parse buf in
  match equal_parse_result result expected with
  | true -> true
  | false ->
      Printf.printf "------ Test failed: -----\n";
      Printf.printf "Input:    \t%s\n" input;
      Printf.printf "Tokens:   \t%s\n" (show_parse_result result);
      Printf.printf "Expected: \t%s\n" (show_parse_result expected);
      false

let mk_test_err msg lnum cnum =
  Error ({ msg; pos = { Lexing.pos_fname = ""; pos_lnum = lnum; pos_bol = 0; pos_cnum = cnum }; buf = Lexing.from_string "" })

(* *)

let%test "parses simple template substitution correctly" =
  let input = "Hello {{name}}!" in
  let expected = Ok([`Raw "Hello";
    `Whitespace " ";
    `Escaped (`WhateverMakesSense [ `App ("name", []); `IdentPath [ `Ident "name" ]  ]);
    `Raw "!";
  ]) in
  make_test input expected

let%test "parses simple comments correctly" =
  let input = "Hello, {{! this is a comment }} world" in
  let expected = Ok([
    `Raw "Hello,";
    `Whitespace " ";
    `Comment;
    `Whitespace " ";
    `Raw "world";
  ]) in
  make_test input expected

let%test "errors when parsing an unclosed template" =
  let input = "Hello, {{name world" in
  let expected = mk_test_err "unexpected EOF" 1 19 in
  make_test input expected

let%test "errors when unexpected token in template" =
  let input = "Hello, {{name !" in
  let expected = mk_test_err "unexpected token: !" 1 15 in
  make_test input expected

let%test "parses nested paths correctly" =
  let input = "{{a.b.c.[0].[\"hello\"].d}}" in
  let expected = Ok([
    `Escaped (`IdentPath [
      `Ident "a";
      `Ident "b";
      `Ident "c";
      `Index (`Int 0);
      `Index (`String "\"hello\"");
      `Ident "d";
    ]);
  ]) in
  make_test input expected

let%test "parses function application correctly" =
  let input = "{{func arg1 arg2.[0] nested.func2.[\"hello\"]}}" in
  let expected = Ok([
    `Escaped (`App ("func", [
      `IdentPath [ `Ident "arg1" ];
      `IdentPath [ `Ident "arg2"; `Index (`Int 0) ];
      `IdentPath [ `Ident "nested"; `Ident "func2"; `Index (`String "\"hello\"") ];
    ]));
  ]) in
  make_test input expected

let%test "parses whitespace control correctly" =
  let input = "Hello, {{~name~}}!" in
  let expected = Ok([
    `Raw "Hello,";
    `Whitespace " ";
    `WhitespaceControl;
    `Escaped (`WhateverMakesSense [ `App ("name", []); `IdentPath [ `Ident "name" ];
       ]);
    `WhitespaceControl;
    `Raw "!";
  ]) in
  make_test input expected

let%test "parses unescaped templates correctly" =
  let input = "Hello, {{{name}}}!" in
  let expected = Ok([
    `Raw "Hello,";
    `Whitespace " ";
    `Unescaped (`WhateverMakesSense [ `App ("name", []); `IdentPath [ `Ident "name" ];
       ]);
    `Raw "!";
  ]) in
  make_test input expected

let%test "parsed unescaped templates with whitespace control correctly" =
  let input = "Hello, {{~{name}}}!" in
  let expected = Ok([
    `Raw "Hello,";
    `Whitespace " ";
    `WhitespaceControl;
    `Unescaped (`WhateverMakesSense [ `App ("name", []); `IdentPath [ `Ident "name" ];
       ]);
    `Raw "!";
  ]) in
  make_test input expected

let%test "parses partials correctly" =
  let input = "Hello, {{> partialName context}}" in
  let expected = Ok([
    `Raw "Hello,";
    `Whitespace " ";
    `Partial { Types.name = "partialName"; context = Some (`IdentPath [ `Ident "context" ]); hash_args = [] };
  ]) in
  make_test input expected

let%test "parses partials with whitespace control correctly" =
  let input = "Hello, {{~> partialName ~}}" in
  let expected = Ok([
    `Raw "Hello,";
    `Whitespace " ";
    `WhitespaceControl;
    `Partial { Types.name = "partialName"; context = None; hash_args = [] };
    `WhitespaceControl;
  ]) in
  make_test input expected

let%test "parses template with escaped chars" =
  make_test "hello \\{{world}}"
    (Ok [ `Raw "hello"; `Whitespace " "; `Raw "{"; `Raw "{"; `Raw "world}}" ])

let%test "parses template with substitution block" =
  make_test "hello, {{world}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ])

let%test "parses template with substitution block and strip before" =
  make_test "hello, {{~world}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ])

let%test "parses template with substitution block and strip before + after" =
  make_test "hello, {{~ world ~}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
         `WhitespaceControl;
       ])

let%test "parses template substitution block with nested ident" =
  make_test "hello, {{~a.b.c}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ])

let%test "parses parenthesis expressions" =
  make_test "hello, {{~(fncall a.b.c)}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App
              ("fncall", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ])

let%test "parses substitution with indexing" =
  make_test "hello, {{~a.['world'] }}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Index (`String "'world'") ]);
       ])

let%test "parses nested fn calls and primitive literals" =
  make_test "hello, {{~fncall a.b.c (fn2 1 2.3) }}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App
              ( "fncall",
                [
                  `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ];
                  `App ("fn2", [ `Literal (`Int 1); `Literal (`Float 2.3) ]);
                ] ));
       ])

let%test "parses comments" =
  make_test "hello, {{! this is a comment }} world"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `Comment;
         `Whitespace " ";
         `Raw "world";
       ])

let%test "parses comments containing mustache syntax" =
  make_test
    {|
    hello, {{!--
      {{# this is a
        multiline comment }}
    --}} world
  |}
    (Ok
       [
         `Whitespace "\n    ";
         `Raw "hello,";
         `Whitespace " ";
         `Comment;
         `Whitespace " ";
         `Raw "world";
         `Whitespace "\n  ";
       ])

let%test "parses unescaped substitution" =
  make_test "hello, {{~{ a.b.c }}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ])

let%test "lexing unclosed '{{{' block throws error" =
  let buf = Lexing.from_string "hello, {{{ ~a.b.c }}" in
  let result = parse buf in
  match result with Ok _ -> false | Error _ -> true

let%test "parses fn application without parenthesis" =
  make_test "hello, {{~fn a.b.c}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App ("fn", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ])

let%test "parses literal-looking values correctly" =
  make_test "hello, {{~true}} and {{~false}} and substitute {{true_looking}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("true", []); `IdentPath [ `Ident "true" ] ]);
         `Whitespace " ";
         `Raw "and";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("false", []); `IdentPath [ `Ident "false" ] ]);
         `Whitespace " ";
         `Raw "and";
         `Whitespace " ";
         `Raw "substitute";
         `Whitespace " ";
         `Escaped
           (`WhateverMakesSense
              [
                `App ("true_looking", []); `IdentPath [ `Ident "true_looking" ];
              ]);
       ])

let%test "parses StripAfter in unescaped substitution" =
  make_test "hello, {{~{ a.b.c }~}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
         `WhitespaceControl;
       ])

let%test "parses else block" =
  make_test "hello, {{#if a}}yes{{else}}no{{/if}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `Block
           {
             expr = `App ("if", [ `IdentPath [ `Ident "a" ] ]);
             content = [ `Raw "yes" ];
             else_content = [ `Raw "no" ];
           };
       ])

let%test "parses else looking things as something else" =
  make_test "hello, {{~else1}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("else1", []); `IdentPath [ `Ident "else1" ] ]);
       ])

let%test "parses else block without open block as Error" =
  let buf =
    Lexing.from_string "{{#if a}}ok{{/if}} {{else}} no business here"
  in
  let result = parse buf in
  match result with Ok _ -> false | Error _ -> true

let%test "parses mismatching close block as Error" =
  let buf = Lexing.from_string "{{#if a}}ok{{/each}}" in
  let result = parse buf in
  match result with Ok _ -> false | Error _ -> true

let%test "parses mustache-style open & close blocks" =
  make_test "{{#a}}{{ . }}{{/a}}"
    (Ok
       [
         `Block
           {
             expr =
               `WhateverMakesSense [ `App ("a", []); `IdentPath [ `Ident "a" ] ];
             content = [ `Escaped (`IdentPath [ `DotPath `OneDot ]) ];
             else_content = [];
           };
       ])

let%test "parses mustache style open/close with dot-index path" =
  make_test "{{#resume.basics}}{{name}}{{/resume.basics}}"
    (Ok
       [
         `Block
           {
             expr = `IdentPath [ `Ident "resume"; `Ident "basics" ];
             content =
               [
                 `Escaped
                   (`WhateverMakesSense
                      [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
               ];
             else_content = [];
           };
       ])

let%test "parses inverted blocks" =
  make_test "{{^a}}{{ . }}{{/a}}"
    (Ok
       [
         `Block
           {
             expr =
               `WhateverMakesSense [ `App ("a", []); `IdentPath [ `Ident "a" ] ];
             content = [];
             else_content = [ `Escaped (`IdentPath [ `DotPath `OneDot ]) ];
           };
       ])

let%test "parses example 1 from handlebarsjs docs" =
  let input = {|
{{#with person}}
{{firstname}} {{lastname}}
{{/with}}
|} in
  make_test input
    (Ok
       [
         `Whitespace "\n";
         `Block
           {
             expr = `App ("with", [ `IdentPath [ `Ident "person" ] ]);
             content =
               [
                 `Whitespace "\n";
                 `Escaped
                   (`WhateverMakesSense
                      [
                        `App ("firstname", []);
                        `IdentPath [ `Ident "firstname" ];
                      ]);
                 `Whitespace " ";
                 `Escaped
                   (`WhateverMakesSense
                      [
                        `App ("lastname", []); `IdentPath [ `Ident "lastname" ];
                      ]);
                 `Whitespace "\n";
               ];
             else_content = [];
           };
         `Whitespace "\n";
       ])

let%test "parses literal string as key for substitution" =
  make_test {| {{#with obj}}{{ "key" }}{{/with}} |}
    (Ok
       [
         `Whitespace " ";
         `Block
           {
             expr = `App ("with", [ `IdentPath [ `Ident "obj" ] ]);
             content =
               [
                 `Escaped
                   (`WhateverMakesSense
                      [ `App ("key", []); `IdentPath [ `Ident "key" ] ]);
               ];
             else_content = [];
           };
         `Whitespace " ";
       ])

let%test "parses literal int as index for substitution" =
  make_test {| {{#with arr}}{{ 0 }}{{/with}} |}
    (Ok
       [
         `Whitespace " ";
         `Block
           {
             expr = `App ("with", [ `IdentPath [ `Ident "arr" ] ]);
             content =
               [
                 `Escaped ( `IdentPath [ `Index (`Int 0) ] );
               ];
             else_content = [];
           };
         `Whitespace " ";
       ])

let%test "parses multiple index arguments" =
  make_test {| {{#with arr}}{{concat [0] [1] "two"}}{{/with}} |}
    (Ok
       [
         `Whitespace " ";
         `Block
           {
             expr = `App ("with", [ `IdentPath [ `Ident "arr" ] ]);
             content =
               [
                 `Escaped
                   (`App
                      ( "concat",
                        [
                          `IdentPath [ `Index (`Int 0) ];
                          `IdentPath [ `Index (`Int 1) ];
                          `Literal (`String "two");
                        ] ));
               ];
             else_content = [];
           };
         `Whitespace " ";
       ])

let%test "parses basic partial syntax" =
  make_test "Hello {{> greeting}}!"
    (Ok
       [
         `Raw "Hello";
         `Whitespace " ";
         `Partial { name = "greeting"; context = None; hash_args = [] };
         `Raw "!";
       ])

let%test "parses partial with whitespace control" =
  make_test "{{~> greeting ~}}"
    (Ok
       [
         `WhitespaceControl;
         `Partial { name = "greeting"; context = None; hash_args = [] };
         `WhitespaceControl;
       ])

let%test "parses partial with context" =
  make_test "{{> greeting user}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = Some (`IdentPath [ `Ident "user" ]);
             hash_args = [];
           };
       ])

let%test "parses parital name with hyphen" =
  make_test "{{> my-partial}}"
    (Ok [ `Partial { name = "my-partial"; context = None; hash_args = [] } ])

let%test "parses partial with single hash argument" =
  make_test "{{> greeting name=\"World\"}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = None;
             hash_args = [ ("name", `Literal (`String "World")) ];
           };
       ])

let%test "parses partial with multiple hash arguments" =
  make_test "{{> user-card name=\"Alice\" age=25}}"
    (Ok
       [
         `Partial
           {
             name = "user-card";
             context = None;
             hash_args =
               [
                 ("name", `Literal (`String "Alice"));
                 ("age", `Literal (`Int 25));
               ];
           };
       ])

let%test "parses partial with context and hash arguments" =
  make_test "{{> greeting user name=\"Hello\"}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = Some (`IdentPath [ `Ident "user" ]);
             hash_args = [ ("name", `Literal (`String "Hello")) ];
           };
       ])

let%test "parses partial with variable as hash argument value" =
  make_test "{{> greeting msg=message}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = None;
             hash_args = [ ("msg", `IdentPath [ `Ident "message" ]) ];
           };
       ])

let%test "parses if section" =
  make_test "{{#if condition}}Yes{{else}}No{{/if}}"
    (Ok
       [
         `Block
           {
             expr = `App ("if", [ `IdentPath [ `Ident "condition" ] ]);
             content = [ `Raw "Yes" ];
             else_content = [ `Raw "No" ];
           };
       ])