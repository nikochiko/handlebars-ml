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

type block_kind = [ `If | `Unless | `Each | `With | `Mustache of ident_path ]
[@@deriving show, eq]

type block = {
  kind : block_kind;
  expr : evalable;
  content : token list;
  else_content : token list;
}

and token =
  [ `Comment of (Uchar.t array[@printer Print_utils.ustring_printer fprintf])
  | `Substitution of evalable * blockattr list
  | `Block of block
  | `WhitespaceControl
  | `Raw of (Uchar.t array[@printer Print_utils.ustring_printer fprintf]) ]
[@@deriving show]

type lex_error = { msg : string; pos : Lexing.position; buf : Sedlexing.lexbuf }

val pp_lex_error : Format.formatter -> lex_error -> unit
val show_lex_error : lex_error -> string

type lex_result = (token list, lex_error) result [@@deriving show]

val uchar_array_of_string : string -> Uchar.t array
val string_of_uchar_array : Uchar.t array -> string
