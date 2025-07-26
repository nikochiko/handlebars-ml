open Types

type compile_error = Not_found | Missing_helper of string
type compile_result = (string, compile_error) result
type hb_error = LexError of lex_error | CompileError of compile_error
type hb_result = (string, hb_error) result
type helper = literal list -> literal option
type get_helper = string -> helper option

val compile : ?get_helper:get_helper -> string -> literal -> hb_result
val compile_tokens : get_helper -> token list -> literal -> compile_result
