open Sedlexing
open Types

type 'a partial_lex_result = ('a * Sedlexing.lexbuf, lex_error) result
(* | Ok ([tokens], buf)
   | Error { msg; pos; buf } *)

let templ_open = [%sedlex.regexp? "{{"]
let templ_close = [%sedlex.regexp? "}}"]
let drop_left n c_arr = Array.sub c_arr n (Array.length c_arr - n)
let drop_right n c_arr = Array.sub c_arr 0 (Array.length c_arr - n)
let letters = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digits = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? (letters | '_'), Star (letters | '_' | digits)]

let start_of_literal =
  [%sedlex.regexp?
    ( '"' | '\''
    | Plus '0' .. '9'
    | ("true" | "false" | "null"), Compl (letters | digits | '.' | '_')
    | '{', Compl '{'
    | '[' )]

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

let string_of_close_block (block : [> close_block ]) =
  match block with
  | `If -> "if"
  | `Unless -> "unless"
  | `Each -> "each"
  | `With -> "with"
  | `Mustache path -> Printf.sprintf "%s" (string_of_ident_path path)

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
  | Child of { parent : container; block : open_block }
  | ElseChild of { parent : container; block : open_block }

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
      add_token parent (`OpenBlock (block, []))

let mk_child parent kind expr =
  let block = { kind; expr; content = []; else_content = [] } in
  Child { parent; block }

let invert_child child =
  match child with
  | Root _ -> failwith "cannot invert root container"
  | Child { parent; block } -> ElseChild { parent; block }
  | ElseChild { parent; block } -> Child { parent; block }

let rec lex ?(expected_close_stack : close_block list = [])
    ?(container = Root []) buf : lex_result =
  match%sedlex buf with
  | '\\', templ_open ->
      let token = `Raw (lexeme buf |> drop_left 1) in
      lex ~expected_close_stack ~container:(add_token container token) buf
  | templ_open -> lex_templ ~expected_close_stack ~container buf
  | Plus (Compl ('\\' | '{')) | any ->
      let container = add_token container (`Raw (lexeme buf)) in
      lex ~expected_close_stack ~container buf
  | eof -> (
      match container with
      | Root acc -> Ok (norm acc)
      | Child { block; _ } | ElseChild { block; _ } ->
          let msg =
            Printf.sprintf "unclosed block: %s" (show_close_block block.kind)
          in
          Error (mkerr msg buf))
  | _ -> Error (mkerr "unexpected input" buf)

and lex_templ ~expected_close_stack ~container buf : lex_result =
  let container =
    match%sedlex buf with
    | '~' -> add_token container `WhitespaceControl
    | _ -> container
  in
  match%sedlex buf with
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
          lex ~expected_close_stack ~container buf)
  | '!' ->
      let* comment, buf = lex_in_comment buf in
      let container = add_token container (`Comment comment) in
      lex ~expected_close_stack ~container buf
  | '{' ->
      let attrs = [ `Unescaped ] in
      let lex_stop buf =
        match%sedlex buf with
        | Star white_space, '}' -> Ok ([], buf)
        | _ -> Error (mkerr "expected closing brace" buf)
      in
      let* (_, evalable), buf = lex_eval_or_apply lex_stop buf in
      let* attrs', buf = lex_templ_close buf in
      let container = add_token container (`Substitution (evalable, attrs)) in
      let container =
        if List.mem `StripAfter attrs' then
          add_token container `WhitespaceControl
        else container
      in
      lex ~expected_close_stack ~container buf
  | '#' ->
      let* (attrs, block), buf = lex_open_block lex_templ_close buf in
      let expected_close = block.kind in
      let expected_close_stack = expected_close :: expected_close_stack in
      let container = mk_child container block.kind block.expr in
      let container =
        if List.mem `StripAfter attrs then
          add_token container `WhitespaceControl
        else container
      in
      lex ~expected_close_stack ~container buf
  | '^' ->
      let* (attrs, block), buf = lex_open_block lex_templ_close buf in
      let expected_close = block.kind in
      let expected_close_stack = expected_close :: expected_close_stack in
      let container =
        mk_child container block.kind block.expr |> invert_child
      in
      let container =
        if List.mem `StripAfter attrs then
          add_token container `WhitespaceControl
        else container
      in
      lex ~expected_close_stack ~container buf
  | '/' -> (
      let* (attrs, block), buf = lex_close_block lex_templ_close buf in
      match expected_close_stack with
      | [] -> Error (mkerr "unexpected close block without open" buf)
      | head :: tail when block = head ->
          let container = mature_child container in
          let container =
            if List.mem `StripAfter attrs then
              add_token container `WhitespaceControl
            else container
          in
          lex ~expected_close_stack:tail ~container buf
      | head :: _ ->
          let msg =
            Printf.sprintf "unexpected close block: %s does not match %s"
              (string_of_close_block block)
              (string_of_close_block head)
          in
          Error (mkerr msg buf))
  | eof -> Error (mkerr "unexpected end of input" buf)
  | _ ->
      let* (attrs, evalable), buf = lex_eval_or_apply lex_templ_close buf in
      let container = add_token container (`Substitution (evalable, [])) in
      let container =
        if List.mem `StripAfter attrs then
          add_token container `WhitespaceControl
        else container
      in
      lex ~expected_close_stack ~container buf

and lex_templ_close buf : blockattr list partial_lex_result =
  match%sedlex buf with
  | Star white_space, '~', templ_close -> Ok ([ `StripAfter ], buf)
  | Star white_space, templ_close -> Ok ([], buf)
  | _ -> Error (mkerr "expected template close" buf)

and lex_open_block lex_stop buf =
  match%sedlex buf with
  | white_space -> lex_open_block lex_stop buf
  | _ ->
      let* (attrs, expr), buf = lex_eval_or_apply lex_stop buf in
      let rec make_sense expr =
        match expr with
        | `WhateverMakesSense exprs ->
            exprs
            |> List.fold_left
                 (fun acc expr ->
                   match acc with Ok _ -> acc | Error _ -> make_sense expr)
                 (Error (mkerr "invalid open block expression" buf))
        | `App (name, args)
          when List.exists (( = ) name) [ "if"; "unless"; "each"; "with" ] -> (
            match args with
            | [ expr ] ->
                let content = [] in
                let else_content = [] in
                let kind =
                  match name with
                  | "if" -> `If
                  | "unless" -> `Unless
                  | "each" -> `Each
                  | "with" -> `With
                  | _ -> failwith "unexpected block kind"
                in
                let block = { kind; expr; content; else_content } in
                Ok ((attrs, block), buf)
            | _ ->
                rollback buf;
                Error
                  (mkerr
                     (Printf.sprintf "expected exactly one argument for #%s"
                        name)
                     buf))
        | `IdentPath path ->
            Ok
              ( ( attrs,
                  {
                    kind = `Mustache (`IdentPath path);
                    expr = `IdentPath path;
                    content = [];
                    else_content = [];
                  } ),
                buf )
        | _ -> Error (mkerr "invalid open block expression" buf)
      in
      make_sense expr

and lex_close_block lex_stop buf =
  match%sedlex buf with
  | white_space -> lex_close_block lex_stop buf
  | _ ->
      let* path, buf = lex_ident_path buf in
      let block =
        match path with
        | [ `Ident "if" ] -> `If
        | [ `Ident "unless" ] -> `Unless
        | [ `Ident "each" ] -> `Each
        | [ `Ident "with" ] -> `With
        | _ -> `Mustache (`IdentPath path)
      in
      let* stop_result, buf = lex_stop buf in
      Ok ((stop_result, block), buf)

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
  match evalable with
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
        | Star white_space, ')' -> Ok ([], buf)
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

and lex_apply lex_stop buf : ('a * evalable) partial_lex_result =
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

and lex_literal buf : [> literal ] partial_lex_result =
  match%sedlex buf with
  | '[' -> lex_list buf
  | '{' -> lex_assoc buf
  | _ ->
      (* TODO: this can be made prettier? *)
      let* lit, buf = lex_primitive_literal buf in
      let v =
        match lit with
        | `String s -> `String s
        | `Int i -> `Int i
        | `Float f -> `Float f
        | `Bool b -> `Bool b
        | `Null -> `Null
      in
      Ok (v, buf)

and lex_list buf : literal partial_lex_result =
  match%sedlex buf with
  | Star white_space, ']' -> Ok (`List [], buf)
  | _ ->
      let* lit, buf = lex_literal buf in
      finish_list [ lit ] buf

and finish_list acc buf =
  match%sedlex buf with
  | Star white_space, ']' -> Ok (`List acc, buf)
  | ',', Star white_space ->
      let* lit, buf = lex_literal buf in
      finish_list (acc @ [ lit ]) buf
  | _ -> Error (mkerr "expected closing bracket or comma" buf)

and lex_assoc buf : literal partial_lex_result =
  match%sedlex buf with
  | Star white_space, '}' -> Ok (`Assoc [], buf)
  | _ ->
      let* (k, v), buf = lex_assoc_item buf in
      finish_assoc [ (k, v) ] buf

and finish_assoc acc buf =
  match%sedlex buf with
  | white_space -> finish_assoc acc buf
  | '}' -> Ok (`Assoc acc, buf)
  | ',', Star white_space ->
      let* (k, v), buf = lex_assoc_item buf in
      finish_assoc (acc @ [ (k, v) ]) buf
  | _ -> Error (mkerr "expected closing brace or comma" buf)

and lex_assoc_item buf : (string * literal) partial_lex_result =
  let* key, buf = lex_assoc_key buf in
  match%sedlex buf with
  | Star white_space, ':', Star white_space ->
      let* lit, buf = lex_literal buf in
      Ok ((key, lit), buf)
  | _ -> Error (mkerr "expected ':' after assoc key" buf)

and lex_assoc_key buf : string partial_lex_result =
  match%sedlex buf with
  | white_space -> lex_assoc_key buf
  | ident ->
      let key = lexeme buf |> string_of_uchar_array in
      Ok (key, buf)
  | '"' ->
      let* s, buf = lex_string ~closing_char:(Uchar.of_char '"') [||] buf in
      Ok (s, buf)
  | _ -> Error (mkerr "expected assoc key" buf)

and lex_primitive_literal buf : primitive_literal partial_lex_result =
  match%sedlex buf with
  | '"' | '\'' ->
      let* s, buf = lex_string ~closing_char:(lexeme_char buf 0) [||] buf in
      Ok (`String s, buf)
  | Plus '0' .. '9', '.', Opt '0' .. '9' -> (
      let num = lexeme buf |> string_of_uchar_array in
      match float_of_string_opt num with
      | Some f -> Ok (`Float f, buf)
      | None -> Error (mkerr "invalid float primitive_literal" buf))
  | Plus '0' .. '9' -> (
      let num = lexeme buf |> string_of_uchar_array in
      match int_of_string_opt num with
      | Some n -> Ok (`Int n, buf)
      | None -> Error (mkerr "invalid integer primitive_literal" buf))
  | "true" -> Ok (`Bool true, buf)
  | "false" -> Ok (`Bool false, buf)
  | "null" -> Ok (`Null, buf)
  | _ -> Error (mkerr "expected primitive_literal" buf)

and lex_string ~closing_char acc buf : string partial_lex_result =
  let aux acc buf =
    match%sedlex buf with
    | '\\', any ->
        lex_string ~closing_char (Array.append acc [| lexeme_char buf 1 |]) buf
    | any ->
        lex_string ~closing_char (Array.append acc [| lexeme_char buf 0 |]) buf
    | eof -> Error (mkerr "unterminated string primitive_literal" buf)
    | _ -> assert false
  in
  match%sedlex buf with
  | '"' | '\'' ->
      let matched = lexeme_char buf 0 in
      if matched = closing_char then Ok (string_of_uchar_array acc, buf)
      else lex_string ~closing_char (Array.append acc [| matched |]) buf
  | _ -> aux acc buf

and lex_ident_path buf : ident_path_segment list partial_lex_result =
  let rec aux acc buf =
    match%sedlex buf with
    | ident ->
        let name = lexeme buf |> string_of_uchar_array in
        lex_nested_ident (acc @ [ `Ident name ]) buf
    | '.', '/' -> aux (acc @ [ `DotPath `OneDot ]) buf
    | "..", '/' -> aux (acc @ [ `DotPath `TwoDot ]) buf
    | '.' -> Ok (acc @ [ `DotPath `OneDot ], buf)
    | ".." -> Ok (acc @ [ `DotPath `TwoDot ], buf)
    | _ -> Error (mkerr "expected evalable expression" buf)
  and lex_nested_ident acc buf =
    match%sedlex buf with
    | '.', ident ->
        let name =
          (* drop '.' *)
          lexeme buf |> drop_left 1 |> string_of_uchar_array
        in
        lex_nested_ident (acc @ [ `Ident name ]) buf
    | '.', '[', Star white_space -> (
        let* lit, buf = lex_primitive_literal buf in
        match%sedlex buf with
        | Star white_space, ']' -> Ok (acc @ [ `Index lit ], buf)
        | _ -> Error (mkerr "expected closing bracket" buf))
    | _ -> Ok (acc, buf)
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
         `Substitution
           ( `WhateverMakesSense
               [ `App ("world", []); `IdentPath [ `Ident "world" ] ],
             [] );
       ])

let%test "lexes template with substitution block and strip before" =
  make_test "hello, {{~world}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution
           ( `WhateverMakesSense
               [ `App ("world", []); `IdentPath [ `Ident "world" ] ],
             [] );
       ])

let%test "lexes template with substitution block and strip before + after" =
  make_test "hello, {{~ world ~}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution
           ( `WhateverMakesSense
               [ `App ("world", []); `IdentPath [ `Ident "world" ] ],
             [] );
         `WhitespaceControl;
       ])

let%test "lexes template substitution block with nested ident" =
  make_test "hello, {{~a.b.c}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ], []);
       ])

let%test "lexes template substitution block with dot path" =
  make_test "hello, {{~.././a.b.c }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution
           ( `IdentPath
               [
                 `DotPath `TwoDot;
                 `DotPath `OneDot;
                 `Ident "a";
                 `Ident "b";
                 `Ident "c";
               ],
             [] );
       ])

let%test "lexes parenthesis expressions" =
  make_test "hello, {{~(fncall a.b.c)}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution
           ( `App
               ("fncall", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]),
             [] );
       ])

let%test "lexes substitution with indexing" =
  make_test "hello, {{~a.['world'] }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution (`IdentPath [ `Ident "a"; `Index (`String "world") ], []);
       ])

let%test "lexes nested fn calls and primitive literals" =
  make_test "hello, {{~fncall a.b.c (fn2 1 2.3) }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution
           ( `App
               ( "fncall",
                 [
                   `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ];
                   `App ("fn2", [ `Literal (`Int 1); `Literal (`Float 2.3) ]);
                 ] ),
             [] );
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
         `Substitution
           (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ], [ `Unescaped ]);
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
         `Substitution
           ( `App ("fn", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]),
             [] );
       ])

let%test "lexes booleans correctly" =
  make_test "hello, {{~true}} and {{~false}} and substitute {{true_looking}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution (`Literal (`Bool true), []);
         `Raw (uchar_array_of_string " and ");
         `WhitespaceControl;
         `Substitution (`Literal (`Bool false), []);
         `Raw (uchar_array_of_string " and substitute ");
         `Substitution
           ( `WhateverMakesSense
               [
                 `App ("true_looking", []); `IdentPath [ `Ident "true_looking" ];
               ],
             [] );
       ])

let%test "lexes StripAfter in unescaped substitution" =
  make_test "hello, {{~{ a.b.c }~}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution
           (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ], [ `Unescaped ]);
         `WhitespaceControl;
       ])

let%test "lexes else block" =
  make_test "hello, {{#if a}}yes{{else}}no{{/if}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `OpenBlock
           ( {
               kind = `If;
               expr = `IdentPath [ `Ident "a" ];
               content = [ `Raw (uchar_array_of_string "yes") ];
               else_content = [ `Raw (uchar_array_of_string "no") ];
             },
             [] );
       ])

let%test "lexes else looking things as something else" =
  make_test "hello, {{~else1}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `WhitespaceControl;
         `Substitution
           ( `WhateverMakesSense
               [ `App ("else1", []); `IdentPath [ `Ident "else1" ] ],
             [] );
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
         `OpenBlock
           ( {
               kind = `Mustache (`IdentPath [ `Ident "a" ]);
               expr = `IdentPath [ `Ident "a" ];
               content = [ `Substitution (`IdentPath [ `DotPath `OneDot ], []) ];
               else_content = [];
             },
             [] );
       ])

let%test "lexes inverted blocks" =
  make_test "{{^a}}{{ . }}{{/a}}"
    (Ok
       [
         `OpenBlock
           ( {
               kind = `Mustache (`IdentPath [ `Ident "a" ]);
               expr = `IdentPath [ `Ident "a" ];
               content = [];
               else_content =
                 [ `Substitution (`IdentPath [ `DotPath `OneDot ], []) ];
             },
             [] );
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
         `OpenBlock
           ( {
               kind = `With;
               expr = `IdentPath [ `Ident "person" ];
               content =
                 [
                   `Raw (uchar_array_of_string "\n");
                   `Substitution
                     ( `WhateverMakesSense
                         [
                           `App ("firstname", []);
                           `IdentPath [ `Ident "firstname" ];
                         ],
                       [] );
                   `Raw (uchar_array_of_string " ");
                   `Substitution
                     ( `WhateverMakesSense
                         [
                           `App ("lastname", []);
                           `IdentPath [ `Ident "lastname" ];
                         ],
                       [] );
                   `Raw (uchar_array_of_string "\n");
                 ];
               else_content = [];
             },
             [] );
         `Raw (uchar_array_of_string "\n");
       ])

let%test "lexes JSON object" =
  let input =
    {|
{{#with
  { "name"     : "John", "age": 30, "isEmployed": true,
    "skills": ["JavaScript", "Python", "OCaml"] }
}}{{ name }}{{/with}}
|}
  in
  make_test input
    (Ok
       [
         `Raw (uchar_array_of_string "\n");
         `OpenBlock
           ( {
               kind = `With;
               expr =
                 `Literal
                   (`Assoc
                      [
                        ("name", `String "John");
                        ("age", `Int 30);
                        ("isEmployed", `Bool true);
                        ( "skills",
                          `List
                            [
                              `String "JavaScript";
                              `String "Python";
                              `String "OCaml";
                            ] );
                      ]);
               content =
                 [
                   `Substitution
                     ( `WhateverMakesSense
                         [ `App ("name", []); `IdentPath [ `Ident "name" ] ],
                       [] );
                 ];
               else_content = [];
             },
             [] );
         `Raw (uchar_array_of_string "\n");
       ])
