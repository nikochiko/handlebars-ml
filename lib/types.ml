module Print_utils = struct
  let ocaml_escape c = c |> Char.chr |> Char.escaped

  let escape_ucode = function
    | (10 | 13 | 9 | 8) as c -> ocaml_escape c (* \n \r \t \b *)
    | c when c < 0x20 || c >= 127 -> Printf.sprintf "\\u{%04x}" c
    | c -> ocaml_escape c

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

type dot_path = [ `OneDot | `TwoDot ] [@@deriving show, eq]

type primitive_literal =
  [ `String of string | `Int of int | `Float of float | `Bool of bool | `Null ]
[@@deriving show, eq]

type literal = [ primitive_literal | Yojson.Basic.t ] [@@deriving show, eq]

type ident_path_segment =
  [ `Ident of string | `DotPath of dot_path | `Index of primitive_literal ]
[@@deriving show, eq]

type ident_path = [ `IdentPath of ident_path_segment list ]
[@@deriving show, eq]

type evalable =
  [ ident_path
  | `Literal of literal
  | `App of string * evalable list
  | `WhateverMakesSense of evalable list ]
[@@deriving show]

type blockattr = [ `StripBefore | `StripAfter | `Unescaped | `Inverted ]
[@@deriving show]

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
  | `Else of blockattr list
  | `CloseBlock of close_block * blockattr list
  | `Raw of (Uchar.t array[@printer Print_utils.ustring_printer fprintf]) ]
[@@deriving show]

type lex_error = { msg : string; pos : Lexing.position; buf : Sedlexing.lexbuf }

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
