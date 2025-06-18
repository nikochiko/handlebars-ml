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

type dot_path = [ `OneDot | `TwoDot ] [@@deriving show]

type literal = [ `String of string | `Int of int | `Float of float ]
[@@deriving show]

type ident_path_segment =
  [ `Ident of string | `DotPath of dot_path | `Index of literal ]
[@@deriving show]

type evalable =
  [ `InParen of evalable
  | `IdentPath of ident_path_segment list
  | `Literal of literal ]
  list
[@@deriving show]

type special_block =
  [ `If of evalable | `Else | `Each of evalable | `With of evalable ]

type token =
  [ `Substitution of evalable
  | `StripBefore
  | `StripAfter
  | `Raw of (Uchar.t array[@printer Print_utils.ustring_printer fprintf]) ]
[@@deriving show]

let templ_open = [%sedlex.regexp? "{{"]
let templ_close = [%sedlex.regexp? "}}"]
let drop_left n c_arr = Array.sub c_arr n (Array.length c_arr - n)
let letters = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digits = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? letters, Star (letters | digits | '_')]

let uchar_array_of_string str =
  Array.init (String.length str) (fun i -> Uchar.of_char (String.get str i))

let string_of_uchar_array c_arr =
  Array.to_seq c_arr |> Seq.map Uchar.to_char |> String.of_seq

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

type lex_error = { msg : string; pos : Lexing.position }

let pp_position fmt pos =
  Format.fprintf fmt "line %d, column %d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let pp_lex_error fmt { msg; pos } =
  pp_position fmt pos;
  Format.fprintf fmt ": %s" msg

let show_lex_error e =
  pp_lex_error Format.str_formatter e;
  Format.flush_str_formatter ()

type lex_result = (token list, lex_error) result [@@deriving show]
type 'a lex_partial_result = ('a * lexbuf, lex_error) result

let mkerr msg buf =
  let pos = lexing_position_curr buf in
  { msg; pos }

let ( >>= ) = Result.bind
let ( let* ) = ( >>= )

let rec lex acc buf : lex_result =
  match%sedlex buf with
  | '\\', templ_open -> lex (acc @ [ `Raw (lexeme buf |> drop_left 1) ]) buf
  | templ_open ->
      let* acc', buf = lex_templ buf in
      lex (acc @ acc') buf
  | Plus (Compl ('\\' | '{')) | any -> lex (acc @ [ `Raw (lexeme buf) ]) buf
  | eof -> Ok (norm acc)
  | _ -> Error (mkerr "unexpected token" buf)

and lex_templ buf : token list lex_partial_result =
  match%sedlex buf with
  | '~' ->
      let* evalable, buf = lex_templ_inner [] buf in
      lex_templ_close ([ `StripBefore ] @ [ `Substitution evalable ]) buf
  | _ ->
      let* evalable, buf = lex_templ_inner [] buf in
      lex_templ_close [ `Substitution evalable ] buf

and lex_templ_close acc buf : token list lex_partial_result =
  match%sedlex buf with
  | white_space -> lex_templ_close acc buf
  | '~', templ_close -> Ok (acc @ [ `StripAfter ], buf)
  | templ_close -> Ok (acc, buf)
  | _ -> Error (mkerr "expected template close" buf)

and lex_templ_inner acc buf : evalable lex_partial_result =
  match%sedlex buf with
  | white_space -> lex_templ_inner acc buf
  | '~', templ_close | templ_close -> (
      match acc with
      | [] -> Error (mkerr "empty substitution block" buf)
      | _ ->
          rollback buf;
          Ok (acc, buf))
  | '(' ->
      let* evalable, buf = lex_inside_parens [] buf in
      lex_templ_inner (acc @ [ `InParen evalable ]) buf
  | _ ->
      let* ident_path, buf = lex_ident_path [] buf in
      lex_templ_inner (acc @ [ `IdentPath ident_path ]) buf

and lex_inside_parens acc buf : evalable lex_partial_result =
  let rec lex_after_fn_name acc buf =
    match%sedlex buf with
    | ')' -> (
        match acc with
        | [] -> Error (mkerr "empty parenthesis block" buf)
        | _ -> Ok (acc, buf))
    | white_space -> lex_after_fn_name acc buf
    | _ ->
        let* ident_path, buf = lex_ident_path [] buf in
        lex_after_fn_name (acc @ [ `IdentPath ident_path ]) buf
  in
  match%sedlex buf with
  | white_space -> lex_inside_parens acc buf
  | ident ->
      let name = lexeme buf |> string_of_uchar_array in
      lex_after_fn_name [ `IdentPath [ `Ident name ] ] buf
  | _ -> Error (mkerr "expected identifier or closing parenthesis" buf)

and lex_ident_path acc buf : ident_path_segment list lex_partial_result =
  match%sedlex buf with
  | ident ->
      let name = lexeme buf |> string_of_uchar_array in
      lex_nested_ident (acc @ [ `Ident name ]) buf
  | '.', '/' -> lex_ident_path (acc @ [ `DotPath `OneDot ]) buf
  | "..", '/' -> lex_ident_path (acc @ [ `DotPath `TwoDot ]) buf
  | '.' -> Ok (acc @ [ `DotPath `OneDot ], buf)
  | ".." -> Ok (acc @ [ `DotPath `TwoDot ], buf)
  | _ -> Error (mkerr "expected evalable expression" buf)

and lex_nested_ident acc buf : ident_path_segment list lex_partial_result =
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

and lex_literal buf : literal lex_partial_result =
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
  | _ -> Error (mkerr "expected literal" buf)

and lex_string_literal ~closing_char acc buf :
    [> `String of string ] lex_partial_result =
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
         `Substitution [ `IdentPath [ `Ident "world" ] ];
       ])

let%test "lexes template with substitution block and strip before" =
  make_test "hello, {{~world}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `StripBefore;
         `Substitution [ `IdentPath [ `Ident "world" ] ];
       ])

let%test "lexes template with substitution block and strip before + after" =
  make_test "hello, {{~ world ~}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `StripBefore;
         `Substitution [ `IdentPath [ `Ident "world" ] ];
         `StripAfter;
       ])

let%test "lexes template substitution block with nested ident" =
  make_test "hello, {{~a.b.c}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `StripBefore;
         `Substitution [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ];
       ])

let%test "lexes template substitution block with dot path" =
  make_test "hello, {{~.././a.b.c }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `StripBefore;
         `Substitution
           [
             `IdentPath
               [
                 `DotPath `TwoDot;
                 `DotPath `OneDot;
                 `Ident "a";
                 `Ident "b";
                 `Ident "c";
               ];
           ];
       ])

let%test "lexes parenthesis expressions" =
  make_test "hello, {{~(fncall a.b.c)}}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `StripBefore;
         `Substitution
           [
             `InParen
               [
                 `IdentPath [ `Ident "fncall" ];
                 `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ];
               ];
           ];
       ])

let%test "lexes substitution with indexing" =
  make_test "hello, {{~a.['world'] }}"
    (Ok
       [
         `Raw (uchar_array_of_string "hello, ");
         `StripBefore;
         `Substitution [ `IdentPath [ `Ident "a"; `Index (`String "world") ] ];
       ])
