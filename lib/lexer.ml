open Sedlexing

module Print_utils = struct
  let ocaml_escape c = c |> Char.chr |> Char.escaped

  let escape_ucode = function
    | (10 | 13 | 9 | 8) as c -> ocaml_escape c (* \n \r \t \b *)
    | c when c < 0x20 || c >= 127 -> Printf.sprintf "\\u{%04x}" c
    | c -> ocaml_escape c

  let escape_uchar u = escape_ucode @@ Uchar.to_int u

  let escape_uchar_in_string u =
    let code = Uchar.to_int u in
    match code with 34 -> "\\\"" | _ -> escape_ucode code

  let escape_ustring us =
    Array.to_seq us
    |> Seq.fold_left (fun s u -> s ^ escape_uchar_in_string u) ""

  let ustring_printer fprintf fmt (us : Uchar.t array) =
    let fs = format_of_string "(Ustring \"%s\")" in
    fprintf fmt fs (escape_ustring us)
end

let uchar_array_of_string str =
  Array.init (String.length str) (fun i -> Uchar.of_char (String.get str i))

let string_of_uchar_array c_arr =
  Array.to_seq c_arr |> Seq.map Uchar.to_char |> String.of_seq

type dot_path = [ `OneDot | `TwoDot ] [@@deriving show, eq]

type literal =
  [ `String of string | `Int of int | `Float of float | `Bool of bool ]
[@@deriving show, eq]

type ident_path_segment =
  [ `Ident of string | `DotPath of dot_path | `Index of literal ]
[@@deriving show, eq]

type ident_path = [ `IdentPath of ident_path_segment list ]
[@@deriving show, eq]

type evalable =
  [ ident_path
  | `Literal of literal
  | `App of string * evalable list
  | `WhateverMakesSense of evalable list ]
[@@deriving show]

type blockattr = [ `StripBefore | `StripAfter | `Unescaped ] [@@deriving show]

(* handlebarsjs supports function applications too here,
   but the semantics of it scare me very much.
   choosing not to support them for anyone's sanity. *)
type open_block_kind =
  [ `If of evalable
  | `Unless of evalable
  | `Each of evalable
  | `With of evalable
  | ident_path ]
[@@deriving show]

type close_block = [ `If | `Unless | `Each | `With | ident_path ]
[@@deriving show, eq]

type token =
  [ `Comment of (Uchar.t array[@printer Print_utils.ustring_printer fprintf])
  | `Substitution of evalable * blockattr list
  | `OpenBlock of open_block_kind * blockattr list
  | `OpenInvertedBlock of open_block_kind * blockattr list
  | `Else of blockattr list
  | `CloseBlock of close_block * blockattr list
  | `Raw of (Uchar.t array[@printer Print_utils.ustring_printer fprintf]) ]
[@@deriving show]

let templ_open = [%sedlex.regexp? "{{"]
let templ_close = [%sedlex.regexp? "}}"]
let drop_left n c_arr = Array.sub c_arr n (Array.length c_arr - n)
let drop_right n c_arr = Array.sub c_arr 0 (Array.length c_arr - n)
let letters = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digits = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? letters, Star (letters | digits | '_')]

let start_of_a_literal =
  [%sedlex.regexp?
    ( '"' | '\''
    | Plus '0' .. '9'
    | ("true" | "false"), Compl (letters | digits | '.' | '_') )]

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
  | `IdentPath path ->
      Printf.sprintf "%s" (string_of_ident_path (`IdentPath path))

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

type lex_error = { msg : string; pos : Lexing.position; buf : lexbuf }

let pp_position fmt pos =
  Format.fprintf fmt "line %d, column %d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let pp_lex_error fmt { msg; pos; _ } =
  pp_position fmt pos;
  Format.fprintf fmt ": %s" msg

let show_lex_error e =
  pp_lex_error Format.str_formatter e;
  Format.flush_str_formatter ()

type lex_result = (token list, lex_error) result [@@deriving show]
type 'a partial_lex_result = ('a * lexbuf, lex_error) result

let mkerr msg buf =
  let pos = lexing_position_curr buf in
  { msg; pos; buf }

let ( >>= ) = Result.bind
let ( let* ) = ( >>= )

let rec lex ?(expected_close_stack : close_block list = []) acc buf : lex_result
    =
  match%sedlex buf with
  | '\\', templ_open ->
      lex ~expected_close_stack (acc @ [ `Raw (lexeme buf |> drop_left 1) ]) buf
  | templ_open -> lex_templ ~expected_close_stack acc buf
  | Plus (Compl ('\\' | '{')) | any ->
      lex ~expected_close_stack (acc @ [ `Raw (lexeme buf) ]) buf
  | eof ->
      if expected_close_stack = [] then Ok (norm acc)
      else
        let top_unclosed = List.hd expected_close_stack in
        let msg =
          Printf.sprintf "unclosed block: %s" (show_close_block top_unclosed)
        in
        Error (mkerr msg buf)
  | _ -> Error (mkerr "unexpected token" buf)

and lex_templ ~expected_close_stack acc buf : lex_result =
  let attrs = match%sedlex buf with '~' -> [ `StripBefore ] | _ -> [] in
  match%sedlex buf with
  | Opt white_space, "else", Opt white_space, Opt '~', templ_close ->
      if expected_close_stack = [] then
        Error (mkerr "unexpected 'else' without an open block" buf)
      else
        let attrs =
          if lexeme buf |> Array.exists (( = ) (Uchar.of_char '~')) then
            `StripAfter :: attrs
          else attrs
        in
        lex ~expected_close_stack (acc @ [ `Else attrs ]) buf
  | '!' ->
      let* comment, buf = lex_in_comment buf in
      lex ~expected_close_stack (acc @ [ `Comment comment ]) buf
  | '{' ->
      let attrs = `Unescaped :: attrs in
      let lex_stop buf =
        match%sedlex buf with
        | Opt white_space, '}' -> Ok ([], buf)
        | _ -> Error (mkerr "expected closing brace" buf)
      in
      let* (_, evalable), buf = lex_eval_or_apply lex_stop buf in
      let* attrs', buf = lex_templ_close buf in
      lex ~expected_close_stack
        (acc @ [ `Substitution (evalable, attrs @ attrs') ])
        buf
  | '#' ->
      let* (attrs', block), buf = lex_open_block lex_templ_close buf in
      let expected_close =
        match block with
        | `If _ -> `If
        | `Unless _ -> `Unless
        | `Each _ -> `Each
        | `With _ -> `With
        | `IdentPath path -> `IdentPath path
      in
      let expected_close_stack = expected_close :: expected_close_stack in
      lex ~expected_close_stack
        (acc @ [ `OpenBlock (block, attrs @ attrs') ])
        buf
  | '^' ->
      let* (attrs', block), buf = lex_open_block lex_templ_close buf in
      let expected_close =
        match block with
        | `If _ -> `If
        | `Unless _ -> `Unless
        | `Each _ -> `Each
        | `With _ -> `With
        | `IdentPath path -> `IdentPath path
      in
      let expected_close_stack = expected_close :: expected_close_stack in
      lex ~expected_close_stack
        (acc @ [ `OpenInvertedBlock (block, attrs @ attrs') ])
        buf
  | '/' -> (
      let* (attrs, block), buf = lex_close_block lex_templ_close buf in
      match expected_close_stack with
      | [] -> Error (mkerr "unexpected close block without open" buf)
      | head :: tail when block = head ->
          lex ~expected_close_stack:tail
            (acc @ [ `CloseBlock (block, attrs) ])
            buf
      | head :: _ ->
          let msg =
            Printf.sprintf "unexpected close block: %s does not match %s"
              (string_of_close_block block)
              (string_of_close_block head)
          in
          Error (mkerr msg buf))
  | eof -> Error (mkerr "unexpected end of input" buf)
  | _ ->
      let* (attrs', evalable), buf = lex_eval_or_apply lex_templ_close buf in
      let attrs = attrs @ attrs' in
      lex ~expected_close_stack (acc @ [ `Substitution (evalable, attrs) ]) buf

and lex_templ_close buf : blockattr list partial_lex_result =
  match%sedlex buf with
  | Opt white_space, '~', templ_close -> Ok ([ `StripAfter ], buf)
  | Opt white_space, templ_close -> Ok ([], buf)
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
            | [ arg ] ->
                let block_kind =
                  match name with
                  | "if" -> `If arg
                  | "unless" -> `Unless arg
                  | "each" -> `Each arg
                  | "with" -> `With arg
                  | _ -> failwith "unexpected block kind"
                in
                Ok ((attrs, block_kind), buf)
            | _ ->
                rollback buf;
                Error
                  (mkerr
                     (Printf.sprintf "expected exactly one argument for #%s"
                        name)
                     buf))
        | `IdentPath path -> Ok ((attrs, `IdentPath path), buf)
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
        | _ -> `IdentPath path
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
  | '(', Opt white_space ->
      let lex_close_paren buf =
        match%sedlex buf with
        | Opt white_space, ')' -> Ok ([], buf)
        | _ -> Error (mkerr "expected closing paren" buf)
      in
      let* (_, evalable), buf = lex_apply lex_close_paren buf in
      Ok (evalable, buf)
  | start_of_a_literal ->
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

and lex_literal buf : literal partial_lex_result =
  match%sedlex buf with
  | '"' | '\'' -> lex_string_literal ~closing_char:(lexeme_char buf 0) [||] buf
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
  | _ -> Error (mkerr "expected literal" buf)

and lex_string_literal ~closing_char acc buf :
    [> `String of string ] partial_lex_result =
  let aux acc buf =
    match%sedlex buf with
    | '\\', any ->
        lex_string_literal ~closing_char
          (Array.append acc [| lexeme_char buf 1 |])
          buf
    | any ->
        lex_string_literal ~closing_char
          (Array.append acc [| lexeme_char buf 0 |])
          buf
    | eof -> Error (mkerr "unterminated string literal" buf)
    | _ -> assert false
  in
  match%sedlex buf with
  | '"' | '\'' ->
      let matched = lexeme_char buf 0 in
      if matched = closing_char then
        Ok (`String (string_of_uchar_array acc), buf)
      else lex_string_literal ~closing_char (Array.append acc [| matched |]) buf
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
    | '.', '[', Opt white_space -> (
        let* lit, buf = lex_literal buf in
        match%sedlex buf with
        | Opt white_space, ']' -> Ok (acc @ [ `Index lit ], buf)
        | _ -> Error (mkerr "expected closing bracket" buf))
    | _ -> Ok (acc, buf)
  in
  aux [] buf

let make_test input expected =
  let buf = Sedlexing.Utf8.from_string input in
  let result = lex [] buf in
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
         `Substitution
           ( `WhateverMakesSense
               [ `App ("world", []); `IdentPath [ `Ident "world" ] ],
             [ `StripBefore ] );
       ])

let%test "lexes template with substitution block and strip before + after" =
  make_test "hello, {{~ world ~}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           ( `WhateverMakesSense
               [ `App ("world", []); `IdentPath [ `Ident "world" ] ],
             [ `StripBefore; `StripAfter ] );
       ])

let%test "lexes template substitution block with nested ident" =
  make_test "hello, {{~a.b.c}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ], [ `StripBefore ]);
       ])

let%test "lexes template substitution block with dot path" =
  make_test "hello, {{~.././a.b.c }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           ( `IdentPath
               [
                 `DotPath `TwoDot;
                 `DotPath `OneDot;
                 `Ident "a";
                 `Ident "b";
                 `Ident "c";
               ],
             [ `StripBefore ] );
       ])

let%test "lexes parenthesis expressions" =
  make_test "hello, {{~(fncall a.b.c)}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           ( `App
               ("fncall", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]),
             [ `StripBefore ] );
       ])

let%test "lexes substitution with indexing" =
  make_test "hello, {{~a.['world'] }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           ( `IdentPath [ `Ident "a"; `Index (`String "world") ],
             [ `StripBefore ] );
       ])

let%test "lexes nested fn calls ad literals" =
  make_test "hello, {{~fncall a.b.c (fn2 1 2.3) }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           ( `App
               ( "fncall",
                 [
                   `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ];
                   `App ("fn2", [ `Literal (`Int 1); `Literal (`Float 2.3) ]);
                 ] ),
             [ `StripBefore ] );
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
         `Substitution
           ( `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ],
             [ `Unescaped; `StripBefore ] );
       ])

let%test "lexing unclosed '{{{' block throws error" =
  let buf = Sedlexing.Utf8.from_string "hello, {{{ ~a.b.c }}" in
  let result = lex [] buf in
  match result with Ok _ -> false | Error _ -> true

let%test "lexes fn application without parenthesis" =
  make_test "hello, {{~fn a.b.c}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           ( `App ("fn", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]),
             [ `StripBefore ] );
       ])

let%test "lexes booleans correctly" =
  make_test "hello, {{~true}} and {{~false}} and substitute {{true_looking}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution (`Literal (`Bool true), [ `StripBefore ]);
         `Raw (uchar_array_of_string " and ");
         `Substitution (`Literal (`Bool false), [ `StripBefore ]);
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
         `Substitution
           ( `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ],
             [ `Unescaped; `StripBefore; `StripAfter ] );
       ])

let%test "lexes else block" =
  make_test "hello, {{#if a}}yes{{else}}no{{/if}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `OpenBlock (`If (`IdentPath [ `Ident "a" ]), []);
         `Raw (uchar_array_of_string "yes");
         `Else [];
         `Raw (uchar_array_of_string "no");
         `CloseBlock (`If, []);
       ])

let%test "lexes else looking things as something else" =
  make_test "hello, {{~else1}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `Substitution
           ( `WhateverMakesSense
               [ `App ("else1", []); `IdentPath [ `Ident "else1" ] ],
             [ `StripBefore ] );
       ])

let%test "lexes else block without open block as Error" =
  let buf =
    Sedlexing.Utf8.from_string "{{#if a}}ok{{/if}} {{else}} no business here"
  in
  let result = lex [] buf in
  match result with Ok _ -> false | Error _ -> true

let%test "lexes mismatching close block as Error" =
  let buf = Sedlexing.Utf8.from_string "{{#if a}}ok{{/each}}" in
  let result = lex [] buf in
  match result with Ok _ -> false | Error _ -> true

let%test "lexes mustache-style open & close blocks" =
  make_test "{{#a}}{{ . }}{{/a}}"
    (Ok
       [
         `OpenBlock (`IdentPath [ `Ident "a" ], []);
         `Substitution (`IdentPath [ `DotPath `OneDot ], []);
         `CloseBlock (`IdentPath [ `Ident "a" ], []);
       ])

let%test "lexes inverted blocks" =
  make_test "{{^a}}{{ . }}{{/a}}"
    (Ok
       [
         `OpenInvertedBlock (`IdentPath [ `Ident "a" ], []);
         `Substitution (`IdentPath [ `DotPath `OneDot ], []);
         `CloseBlock (`IdentPath [ `Ident "a" ], []);
       ])
