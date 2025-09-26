type dot_path = [ `OneDot | `TwoDot ] [@@deriving show, eq]

type literal =
  [ `String of string | `Int of int | `Float of float | `Bool of bool | `Null ]
[@@deriving show, eq]

type literal_or_collection =
  [ literal
  | `Assoc of (string * literal_or_collection) list
  | `List of literal_or_collection list ]
[@@deriving show, eq]

type ident_path_segment =
  [ `Ident of string | `DotPath of dot_path | `Index of literal ]
[@@deriving show, eq]

type ident_path = [ `IdentPath of ident_path_segment list ]
[@@deriving show, eq]

type evalable =
  [ ident_path
  | `App of string * evalable list
  | `WhateverMakesSense of evalable list
  | `Literal of literal ]
[@@deriving show, eq]

(* handlebarsjs supports function applications too here,
   but the semantics of it scare me very much.
   choosing not to support them for anyone's sanity. *)
type block = {
  expr : evalable;
  content : token list;
  else_content : token list;
}

and partial_info = {
  name : string;
  context : evalable option;
  hash_args : (string * evalable) list;
}

and token =
  [ `Comment of string
  | `Escaped of evalable
  | `Unescaped of evalable
  | `Block of block
  | `Partial of partial_info
  | `WhitespaceControl
  | `Whitespace of string
  | `Raw of string ]
[@@deriving show, eq]

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
(* | Ok ([tokens])
     | Error { msg; pos; buf } *)

let uchar_array_of_string str =
  Array.init (String.length str) (fun i -> Uchar.of_char (String.get str i))

let string_of_uchar_array c_arr =
  Array.to_seq c_arr |> Seq.map Uchar.to_char |> String.of_seq
