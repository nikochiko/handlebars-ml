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
  | `Literal of literal
  | `App of string * evalable list
  | `WhateverMakesSense of evalable list ]
[@@deriving show, eq]

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
  [ `Comment
  | `Escaped of evalable
  | `Unescaped of evalable
  | `Block of block
  | `Partial of partial_info
  | `WhitespaceControl
  | `Whitespace of string
  | `Raw of string ]
[@@deriving show, eq]