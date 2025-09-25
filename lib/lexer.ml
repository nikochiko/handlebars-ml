open Sedlexing
open Types

type 'a partial_lex_result = ('a * Sedlexing.lexbuf, lex_error) result
(* | Ok ([tokens], buf)
   | Error { msg; pos; buf } *)

let templ_open = [%sedlex.regexp? "{{"]
let templ_close = [%sedlex.regexp? "}}"]
let letters = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digits = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? (letters | '_'), Star (letters | '_' | digits)]

let start_of_literal =
  [%sedlex.regexp?
    ( '"' | '\''
    | Plus '0' .. '9'
    | ("true" | "false" | "null"), Compl (letters | digits | '.' | '_') )]

let drop_left n c_arr = Array.sub c_arr n (Array.length c_arr - n)
let drop_right n c_arr = Array.sub c_arr 0 (Array.length c_arr - n)

let string_of_path_segment segment =
  match segment with
  | `Ident name -> name
  | `DotPath `OneDot -> "."
  | `DotPath `TwoDot -> ".."
  | `Index (`String s) -> Printf.sprintf "['%s']" s
  | `Index (`Int i) -> Printf.sprintf "[%d]" i
  | _ -> failwith "unexpected ident path segment"

let string_of_ident_path (path : ident_path) =
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

let string_of_literal (lit : literal) : string =
  match lit with
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b
  | _ -> failwith "not implemented"

let rec string_of_evalable (evalable : evalable) =
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
  | `Literal l -> show_literal l

let norm tokens =
  let normed, last =
    tokens
    |> List.fold_left
         (fun (acc, cur) token ->
           match token with
           | `Raw c_arr -> (acc, Array.append cur c_arr)
           | _ ->
               if Array.length cur > 0 then (acc @ [ `Raw cur; token ], [||])
               else (acc @ [ token ], [||]))
         ([], [||])
  in
  if Array.length last > 0 then normed @ [ `Raw last ] else normed

let mkerr msg buf : lex_error =
  let pos = lexing_position_curr buf in
  { msg; pos; buf }

let ( >>= ) = Result.bind
let ( let* ) = ( >>= )

type container =
  | Root of token list
  | Child of { parent : container; block : block }
  | ElseChild of { parent : container; block : block }

let add_token container tok =
  match container with
  | Root acc -> Root (acc @ [ tok ])
  | Child { parent; block } ->
      let { content; _ } = block in
      let block = { block with content = content @ [ tok ] } in
      Child { parent; block }
  | ElseChild { parent; block } ->
      let { else_content; _ } = block in
      let block = { block with else_content = else_content @ [ tok ] } in
      ElseChild { parent; block }

let mature_child container =
  match container with
  | Root _ -> failwith "cannot mature root container"
  | Child { parent; block } | ElseChild { parent; block } ->
      add_token parent (`Block block)

let invert_child child =
  match child with
  | Root _ -> failwith "cannot invert root container"
  | Child { parent; block } -> ElseChild { parent; block }
  | ElseChild { parent; block } -> Child { parent; block }

let mk_child parent expr =
  let block = { expr; content = []; else_content = [] } in
  Child { parent; block }

let mk_closing_path expr : ident_path =
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

let rec lex ?(container = Root []) buf : lex_result =
  match%sedlex buf with
  | '\\', templ_open ->
      let token = `Raw (lexeme buf |> drop_left 1) in
      lex ~container:(add_token container token) buf
  | templ_open -> lex_templ ~container buf
  | Plus (Compl ('\\' | '{')) | any ->
      let container = add_token container (`Raw (lexeme buf)) in
      lex ~container buf
  | eof -> (
      match container with
      | Root acc -> Ok (norm acc)
      | Child { block; _ } | ElseChild { block; _ } ->
          let expected_close = mk_closing_path block.expr in
          let msg =
            Printf.sprintf "unclosed block: %s" (show_ident_path expected_close)
          in
          Error (mkerr msg buf))
  | _ -> Error (mkerr "unexpected input" buf)

and lex_templ ~container buf : lex_result =
  let container =
    match%sedlex buf with
    | '~' -> add_token container `WhitespaceControl
    | _ -> container
  in
  match%sedlex buf with
  | eof -> Error (mkerr "unexpected end of input" buf)
  | Star white_space, "else", Star white_space, Opt '~', templ_close -> (
      match container with
      | Root _ -> Error (mkerr "unexpected 'else' without an open block" buf)
      | _ ->
          let container = invert_child container in
          let container =
            if lexeme buf |> Array.exists (( = ) (Uchar.of_char '~')) then
              add_token container `WhitespaceControl
            else container
          in
          lex ~container buf)
  | '!' ->
      let* comment, buf = lex_in_comment buf in
      let container = add_token container (`Comment comment) in
      lex ~container buf
  | '#' ->
      let* (strip_after, block), buf = lex_open_block lex_templ_close buf in
      let container = mk_child container block.expr in
      let container =
        if strip_after then add_token container `WhitespaceControl
        else container
      in
      lex ~container buf
  | '^' ->
      let* (strip_after, block), buf = lex_open_block lex_templ_close buf in
      let container = mk_child container block.expr |> invert_child in
      let container =
        if strip_after then add_token container `WhitespaceControl
        else container
      in
      lex ~container buf
  | '/' -> (
      let* (strip_after, evalable), buf = lex_close_block lex_templ_close buf in
      match container with
      | Root _ -> Error (mkerr "unexpected close block without open" buf)
      | Child { block; _ } | ElseChild { block; _ } -> (
          let expected = mk_closing_path block.expr in
          let rec make_sense = function
            | `IdentPath path -> Some (`IdentPath path)
            | `WhateverMakesSense exprs ->
                let expr_seq = List.to_seq exprs in
                Seq.find_map make_sense expr_seq
            | _ -> None
          in
          match make_sense evalable with
          | Some path when path = expected ->
              let container = mature_child container in
              let container =
                if strip_after then add_token container `WhitespaceControl
                else container
              in
              lex ~container buf
          | _ ->
              let msg =
                Printf.sprintf "unexpected close block: %s; does not match %s"
                  (string_of_evalable evalable)
                  (string_of_ident_path expected)
              in
              Error (mkerr msg buf)))
  | '{' ->
      let lex_stop buf =
        match%sedlex buf with
        | Star white_space, '}' -> Ok (false, buf)
        | _ -> Error (mkerr "expected closing brace" buf)
      in
      let* (_, evalable), buf = lex_eval_or_apply lex_stop buf in
      let* strip_after, buf = lex_templ_close buf in
      let container = add_token container (`Unescaped evalable) in
      let container =
        if strip_after then add_token container `WhitespaceControl
        else container
      in
      lex ~container buf
  | '>' ->
      let* (strip_after, partial_name), buf = lex_partial lex_templ_close buf in
      let container = add_token container (`Partial partial_name) in
      let container =
        if strip_after then add_token container `WhitespaceControl
        else container
      in
      lex ~container buf
  | _ ->
      let* (strip_after, evalable), buf =
        lex_eval_or_apply lex_templ_close buf
      in
      let container = add_token container (`Escaped evalable) in
      let container =
        if strip_after then add_token container `WhitespaceControl
        else container
      in
      lex ~container buf

and lex_templ_close buf : bool partial_lex_result =
  match%sedlex buf with
  | Star white_space, '~', templ_close -> Ok (true, buf)
  | Star white_space, templ_close -> Ok (false, buf)
  | _ -> Error (mkerr "expected template close" buf)

and lex_open_block lex_stop buf =
  match%sedlex buf with
  | white_space -> lex_open_block lex_stop buf
  | _ ->
      let* (stop_result, expr), buf = lex_eval_or_apply lex_stop buf in
      Ok ((stop_result, { expr; content = []; else_content = [] }), buf)

and lex_close_block lex_stop buf =
  match%sedlex buf with
  | white_space -> lex_close_block lex_stop buf
  | _ ->
      let* (stop_result, evalable), buf = lex_eval_or_apply lex_stop buf in
      Ok ((stop_result, evalable), buf)

and lex_in_comment buf =
  let rec aux acc buf =
    match%sedlex buf with
    | templ_close -> Ok (acc, buf)
    | any -> aux (Array.append acc (lexeme buf)) buf
    | _ -> Error (mkerr "expected template close" buf)
  in
  aux [||] buf

and lex_eval_or_apply (lex_stop : lexbuf -> 'a partial_lex_result) buf :
    ('a * evalable) partial_lex_result =
  let* evalable, buf = lex_eval buf in
  let evalable =
    match evalable with
    | `Literal literal -> `IdentPath [ `Ident (string_of_literal literal) ]
    | _ -> evalable
  in
  match evalable with
  | `Literal l ->
      let msg =
        Printf.sprintf "expected lookup or helper expression. got literal: %s"
          (show_literal l)
      in
      Error (mkerr msg buf)
  | `IdentPath [ `Ident name ] ->
      (* e.g. {{ f }} could refer to the fn call f or the substitution variable f *)
      let* (stop_result, args), buf = lex_args lex_stop [] buf in
      if args = [] then
        Ok
          ((stop_result, `WhateverMakesSense [ `App (name, []); evalable ]), buf)
      (* {{ f a b c }} - with arguments - can only be a fn call *)
        else Ok ((stop_result, `App (name, args)), buf)
  | _ ->
      let* stop_result, buf = lex_stop buf in
      Ok ((stop_result, evalable), buf)

and lex_eval buf : evalable partial_lex_result =
  (* lex for one evalable expression *)
  match%sedlex buf with
  | white_space -> lex_eval buf
  | '(', Star white_space ->
      let lex_close_paren buf =
        match%sedlex buf with
        | Star white_space, ')' -> Ok (false, buf)
        | _ -> Error (mkerr "expected closing paren" buf)
      in
      let* (_, evalable), buf = lex_apply lex_close_paren buf in
      Ok (evalable, buf)
  | start_of_literal ->
      rollback buf;
      let* lit, buf = lex_literal buf in
      Ok (`Literal lit, buf)
  | eof -> Error (mkerr "unexpected end of input" buf)
  | _ ->
      let* ident_path, buf = lex_ident_path buf in
      Ok (`IdentPath ident_path, buf)

and lex_apply (lex_stop : lexbuf -> 'a partial_lex_result) buf :
    ('a * evalable) partial_lex_result =
  match%sedlex buf with
  | white_space -> lex_apply lex_stop buf
  | ident ->
      let name = lexeme buf |> string_of_uchar_array in
      let* (stop_result, args), buf = lex_args lex_stop [] buf in
      Ok ((stop_result, `App (name, args)), buf)
  | _ -> Error (mkerr "expected function call" buf)

and lex_args (lex_stop : lexbuf -> 'a partial_lex_result) acc buf :
    ('a * evalable list) partial_lex_result =
  match%sedlex buf with
  | white_space -> lex_args lex_stop acc buf
  | _ -> (
      match lex_stop buf with
      | Ok (stop_result, buf) -> Ok ((stop_result, acc), buf)
      | Error { buf; _ } ->
          let* evalable, buf = lex_eval buf in
          lex_args lex_stop (acc @ [ evalable ]) buf)

and lex_literal buf : literal partial_lex_result =
  match%sedlex buf with
  | '"' | '\'' ->
      let* s, buf = lex_string ~closing_char:(lexeme_char buf 0) [||] buf in
      Ok (`String s, buf)
  | Plus '0' .. '9', '.', Opt '0' .. '9' -> (
      let num = lexeme buf |> string_of_uchar_array in
      match float_of_string_opt num with
      | Some f -> Ok (`Float f, buf)
      | None -> Error (mkerr "invalid float literal" buf))
  | Plus '0' .. '9' -> (
      let num = lexeme buf |> string_of_uchar_array in
      match int_of_string_opt num with
      | Some n -> Ok (`Int n, buf)
      | None -> Error (mkerr "invalid integer literal" buf))
  | "true" -> Ok (`Bool true, buf)
  | "false" -> Ok (`Bool false, buf)
  | "null" -> Ok (`Null, buf)
  | _ -> Error (mkerr "expected literal" buf)

and lex_string ~closing_char acc buf : string partial_lex_result =
  let aux acc buf =
    match%sedlex buf with
    | '\\', any ->
        lex_string ~closing_char (Array.append acc [| lexeme_char buf 1 |]) buf
    | any ->
        lex_string ~closing_char (Array.append acc [| lexeme_char buf 0 |]) buf
    | eof -> Error (mkerr "unterminated string literal" buf)
    | _ -> assert false
  in
  match%sedlex buf with
  | '"' | '\'' ->
      let matched = lexeme_char buf 0 in
      if matched = closing_char then Ok (string_of_uchar_array acc, buf)
      else lex_string ~closing_char (Array.append acc [| matched |]) buf
  | _ -> aux acc buf

and lex_partial lex_stop buf : (bool * string) partial_lex_result =
  match%sedlex buf with
  | white_space -> lex_partial lex_stop buf
  | ident ->
      let name = lexeme buf |> string_of_uchar_array in
      let* stop_result, buf = lex_stop buf in
      Ok ((stop_result, name), buf)
  | _ -> Error (mkerr "expected partial name" buf)

and lex_ident_path buf : ident_path_segment list partial_lex_result =
  let rec aux acc buf =
    match%sedlex buf with
    | '.', '/' -> aux (acc @ [ `DotPath `OneDot ]) buf
    | "..", '/' -> aux (acc @ [ `DotPath `TwoDot ]) buf
    | '.' -> Ok (acc @ [ `DotPath `OneDot ], buf)
    | ".." -> Ok (acc @ [ `DotPath `TwoDot ], buf)
    | _ -> lex_nested_ident acc buf
  and lex_nested_ident acc buf =
    let* tok, buf =
      match%sedlex buf with
      | ident ->
          let name = lexeme buf |> string_of_uchar_array in
          Ok (`Ident name, buf)
      | '[' ->
          rollback buf;
          lex_index buf
      | _ -> Error (mkerr "expected identifier or index" buf)
    in
    match%sedlex buf with
    | '.' -> lex_nested_ident (acc @ [ tok ]) buf
    | _ -> Ok (acc @ [ tok ], buf)
  and lex_index buf =
    match%sedlex buf with
    | '[', Star white_space -> (
        let* token, buf =
          match%sedlex buf with
          | start_of_literal ->
              rollback buf;
              let* lit, buf = lex_literal buf in
              Ok (`Index lit, buf)
          | ident ->
              let name = lexeme buf |> string_of_uchar_array in
              Ok (`Ident name, buf)
          | _ -> Error (mkerr "expected literal or identifier" buf)
        in
        match%sedlex buf with
        | Star white_space, ']' -> Ok (token, buf)
        | _ -> Error (mkerr "expected closing bracket" buf))
    | _ -> Error (mkerr "expected index" buf)
  in
  aux [] buf

let make_test input expected =
  let buf = Sedlexing.Utf8.from_string input in
  let result = lex buf in
  match result = expected with
  | true -> true
  | false ->
      Printf.printf "------ Test failed: -----\n";
      Printf.printf "Input:    \t%s\n" input;
      Printf.printf "Tokens:   \t%s\n" (show_lex_result result);
      Printf.printf "Expected: \t%s\n" (show_lex_result expected);
      false

let%test "lexes template with escaped chars" =
  make_test "hello \\{{world}}"
    (Ok [ `Raw (uchar_array_of_string "hello {{world}}") ])

let%test "lexes template with substitution block" =
  make_test "hello, {{world}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ])

let%test "lexes template with substitution block and strip before" =
  make_test "hello, {{~world}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ])

let%test "lexes template with substitution block and strip before + after" =
  make_test "hello, {{~ world ~}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
         `WhitespaceControl;
       ])

let%test "lexes template substitution block with nested ident" =
  make_test "hello, {{~a.b.c}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ])

let%test "lexes template substitution block with dot path" =
  make_test "hello, {{~.././a.b.c }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`IdentPath
              [
                `DotPath `TwoDot;
                `DotPath `OneDot;
                `Ident "a";
                `Ident "b";
                `Ident "c";
              ]);
       ])

let%test "lexes parenthesis expressions" =
  make_test "hello, {{~(fncall a.b.c)}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`App
              ("fncall", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ])

let%test "lexes substitution with indexing" =
  make_test "hello, {{~a.['world'] }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Index (`String "world") ]);
       ])

let%test "lexes nested fn calls and primitive literals" =
  make_test "hello, {{~fncall a.b.c (fn2 1 2.3) }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`App
              ( "fncall",
                [
                  `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ];
                  `App ("fn2", [ `Literal (`Int 1); `Literal (`Float 2.3) ]);
                ] ));
       ])

let%test "lexes comments" =
  make_test "hello, {{! this is a comment }} world"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Comment (uchar_array_of_string " this is a comment ");
         `Raw (uchar_array_of_string " world");
       ])

let%test "lexes unescaped substitution" =
  make_test "hello, {{~{ a.b.c }}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ])

let%test "lexing unclosed '{{{' block throws error" =
  let buf = Sedlexing.Utf8.from_string "hello, {{{ ~a.b.c }}" in
  let result = lex buf in
  match result with Ok _ -> false | Error _ -> true

let%test "lexes fn application without parenthesis" =
  make_test "hello, {{~fn a.b.c}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`App ("fn", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ])

let%test "lexes literal-looking values correctly" =
  make_test "hello, {{~true}} and {{~false}} and substitute {{true_looking}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("true", []); `IdentPath [ `Ident "true" ] ]);
         `Raw (uchar_array_of_string " and ");
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("false", []); `IdentPath [ `Ident "false" ] ]);
         `Raw (uchar_array_of_string " and substitute ");
         `Escaped
           (`WhateverMakesSense
              [
                `App ("true_looking", []); `IdentPath [ `Ident "true_looking" ];
              ]);
       ])

let%test "lexes StripAfter in unescaped substitution" =
  make_test "hello, {{~{ a.b.c }~}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
         `WhitespaceControl;
       ])

let%test "lexes else block" =
  make_test "hello, {{#if a}}yes{{else}}no{{/if}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Block
           {
             expr = `App ("if", [ `IdentPath [ `Ident "a" ] ]);
             content = [ `Raw (uchar_array_of_string "yes") ];
             else_content = [ `Raw (uchar_array_of_string "no") ];
           };
       ])

let%test "lexes else looking things as something else" =
  make_test "hello, {{~else1}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("else1", []); `IdentPath [ `Ident "else1" ] ]);
       ])

let%test "lexes else block without open block as Error" =
  let buf =
    Sedlexing.Utf8.from_string "{{#if a}}ok{{/if}} {{else}} no business here"
  in
  let result = lex buf in
  match result with Ok _ -> false | Error _ -> true

let%test "lexes mismatching close block as Error" =
  let buf = Sedlexing.Utf8.from_string "{{#if a}}ok{{/each}}" in
  let result = lex buf in
  match result with Ok _ -> false | Error _ -> true

let%test "lexes mustache-style open & close blocks" =
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

let%test "lexes inverted blocks" =
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

let%test "lexes example 1 from handlebarsjs docs" =
  let input = {|
{{#with person}}
{{firstname}} {{lastname}}
{{/with}}
|} in
  make_test input
    (Ok
       [
         `Raw (uchar_array_of_string "\n");
         `Block
           {
             expr = `App ("with", [ `IdentPath [ `Ident "person" ] ]);
             content =
               [
                 `Raw (uchar_array_of_string "\n");
                 `Escaped
                   (`WhateverMakesSense
                      [
                        `App ("firstname", []);
                        `IdentPath [ `Ident "firstname" ];
                      ]);
                 `Raw (uchar_array_of_string " ");
                 `Escaped
                   (`WhateverMakesSense
                      [
                        `App ("lastname", []); `IdentPath [ `Ident "lastname" ];
                      ]);
                 `Raw (uchar_array_of_string "\n");
               ];
             else_content = [];
           };
         `Raw (uchar_array_of_string "\n");
       ])

let%test "lexes literal string as key for substitution" =
  make_test {| {{#with obj}}{{ "key" }}{{/with}} |}
    (Ok
       [
         `Raw (uchar_array_of_string " ");
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
         `Raw (uchar_array_of_string " ");
       ])

let%test "lexes literal int as index for substitution" =
  make_test {| {{#with arr}}{{ 0 }}{{/with}} |}
    (Ok
       [
         `Raw (uchar_array_of_string " ");
         `Block
           {
             expr = `App ("with", [ `IdentPath [ `Ident "arr" ] ]);
             content =
               [
                 `Escaped
                   (`WhateverMakesSense
                      [ `App ("0", []); `IdentPath [ `Ident "0" ] ]);
               ];
             else_content = [];
           };
         `Raw (uchar_array_of_string " ");
       ])

let%test "lexes multiple index arguments" =
  make_test {| {{#with arr}}{{concat [0] [1] "two"}}{{/with}} |}
    (Ok
       [
         `Raw (uchar_array_of_string " ");
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
         `Raw (uchar_array_of_string " ");
       ])

let%test "lexes basic partial syntax" =
  make_test "Hello {{> greeting}}!"
    (Ok
       [
         `Raw (uchar_array_of_string "Hello ");
         `Partial "greeting";
         `Raw (uchar_array_of_string "!");
       ])

let%test "lexes partial with whitespace control" =
  make_test "{{~> greeting ~}}"
    (Ok
       [
         `WhitespaceControl;
         `Partial "greeting";
         `WhitespaceControl;
       ])
