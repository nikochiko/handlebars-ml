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
  [ `Comment of (Uchar.t array[@printer Print_utils.ustring_printer fprintf])
  | `Escaped of evalable
  | `Unescaped of evalable
  | `Block of block
  | `Partial of partial_info
  | `WhitespaceControl
  | `Raw of (Uchar.t array[@printer Print_utils.ustring_printer fprintf]) ]
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
