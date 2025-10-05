{
open Lexing

type templ_kind =
  | Escaped
  | Unescaped
  | Section
  | InvertedSection
  | CloseSection
  | Partial
[@@deriving show, eq]

type ident_path_segment =
  | INDEX of [ `String of string | `Int of int ]
  | IDENT of string
  | ONEDOT
  | TWODOT
[@@deriving show, eq]

type token = 
  | RAW of string
  | WHITESPACE of string
  | TEMPL_OPEN of { ws_control: bool; kind: templ_kind }
  | TEMPL_CLOSE of { ws_control: bool; is_unescaped: bool; raw: string }
  | IDENT_PATH of ident_path_segment list
  | COMMENT
  | LITERAL of Types.literal
  | START_HASH_ARG of string
  | LPAREN
  | RPAREN
  | EOF
[@@deriving show, eq]

let templ_open_char = '{'
let templ_close_char = '}'
let templ_open = String.make 2 templ_open_char
let templ_close = String.make 2 templ_close_char
}

let templ_open_char = '{'
let templ_close_char = '}'
let templ_open = "{{"
let templ_close = "}}"

let newline = '\n' | '\r' | "\r\n"
let whitespace = [' ' '\t']

let string_literal = '"' (([^ '"'] | "\\\"")* as s) '"' | '\'' (([^ '\''] | "\\\'")* as s) '\''
let int_literal = ['0'-'9']+
let float_literal = '-'? ['0'-'9']+ '.' ['0'-'9']* (['e' 'E'] ['+' '-']? ['0'-'9']+)?

let ident = ['a'-'z' 'A'-'Z' '_' '@'] (['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*)

rule lex = parse
  | '\\' ('{' as c) { RAW (String.make 1 c) }
  | templ_open ('~'? as ws_control) { lex_in_templ_open (ws_control = "~") lexbuf }
  | '{' { RAW (lexeme lexbuf) }
  | whitespace+ as s { WHITESPACE s }
  | newline whitespace* { new_line lexbuf; WHITESPACE (lexeme lexbuf) }
  | [^ '{' '\\' ' ' '\t' '\n' '\r']+ as s { RAW s }
  | eof { EOF }
and lex_in_templ_open ws_control = parse
  | templ_open_char { TEMPL_OPEN { ws_control; kind = Unescaped } }
  | '#' { TEMPL_OPEN { ws_control; kind = Section } }
  | '^' { TEMPL_OPEN { ws_control; kind = InvertedSection } }
  | '/' { TEMPL_OPEN { ws_control; kind = CloseSection } }
  | '>' { TEMPL_OPEN { ws_control; kind = Partial } }
  | '!' { lex_comment lexbuf }
  | "!--" { lex_mustache_comment lexbuf }
  | "" { TEMPL_OPEN { ws_control; kind = Escaped } }
and lex_in_templ = parse
  | (templ_close_char? as unescaped) ('~'? as ws_control) templ_close {
      TEMPL_CLOSE { ws_control = (ws_control = "~"); is_unescaped = (unescaped = String.make 1 templ_close_char); raw = lexeme lexbuf }
    }
  | newline { new_line lexbuf; lex_in_templ lexbuf }
  | whitespace { lex_in_templ lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | string_literal { LITERAL (`String s) }
  | int_literal as s {
      let v = match int_of_string_opt s with
      | Some i -> `Int i
      | None -> `Intlit s
      in LITERAL v
    }
  | float_literal as s { LITERAL (`Float (float_of_string s)) }
  | (ident as s) whitespace* '=' { START_HASH_ARG s }
  | '[' { lex_as_hash_arg_or_nested_index_path (lex_index lexbuf) lexbuf }
  | ident as s { lex_nested_path [IDENT s] lexbuf }
  | '.' { IDENT_PATH [ONEDOT] }
  | ".." { IDENT_PATH [TWODOT] }
  | _ { failwith ("unexpected token: " ^ lexeme lexbuf) }
  | eof { failwith "unexpected EOF" }
and lex_as_hash_arg_or_nested_index_path first_segment = parse
  | whitespace* '=' {
      START_HASH_ARG (match first_segment with
        | `String s -> s
        | `Int i -> string_of_int i)
    }
  | "" { lex_nested_path [INDEX first_segment] lexbuf }
and lex_index = parse
  | (int_literal as s) ']' { `Int (int_of_string s) }
  | "" { `String (lex_index_wildly (Buffer.create 64) lexbuf) }
and lex_index_wildly buf = parse
  | '\\' (']' as c) { Buffer.add_char buf c; lex_index_wildly buf lexbuf }
  | ']' { Buffer.contents buf }
  | '\n' { new_line lexbuf; Buffer.add_char buf '\n'; lex_index_wildly buf lexbuf }
  | _ as c { Buffer.add_char buf c; lex_index_wildly buf lexbuf }
  | eof { failwith "unexpected EOF" }
and eat_templ_close_char = parse
  | templ_close_char { () }
  | "" { failwith (Printf.sprintf "expected token: %c" templ_close_char) }
and lex_nested_path acc = parse
  | '.' (ident as s) { lex_nested_path (IDENT s :: acc) lexbuf }
  | '.' '[' { lex_nested_path (INDEX (lex_index lexbuf) :: acc) lexbuf }
  | "" { IDENT_PATH (List.rev acc) }
  | eof { failwith "unexpected EOF" }
and lex_mustache_comment = parse
  | "--}}" { COMMENT }
  | eof { failwith "unexpected EOF" }
  | '\n' { new_line lexbuf; lex_mustache_comment lexbuf }
  | _ { lex_mustache_comment lexbuf }
and lex_comment = parse
  | templ_close { COMMENT }
  | eof { failwith "unexpected EOF" }
  | '\n' { new_line lexbuf; lex_comment lexbuf }
  | _ { lex_comment lexbuf }
