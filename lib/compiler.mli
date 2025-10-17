open Types

(** Compilation errors that can occur during template processing *)
type compile_error =
  | Missing_helper of string  (** Helper function not found *)
  | Missing_partial of string  (** Partial template not found *)
  | Partial_parse_error of string * Parser.parse_error
      (** Parsing error in a partial template *)
  | Partial_compile_error of string * compile_error
      (** Compilation error in a partial template *)
[@@deriving show]

type compile_result = (string, compile_error) result
(** Result type for compilation operations *)

(** Combined error type for both lexing and compilation errors *)
type hb_error =
  | ParseError of Parser.parse_error  (** Error during parsing *)
  | CompileError of compile_error  (** Error during compilation *)
[@@deriving show]

type hb_result = (string, hb_error) result [@@deriving show]
(** Result type for complete handlebars processing *)

type context_values = {
  v : literal_or_collection;
  extras : literal_or_collection;
}
(** Context values for template variables + injected extras *)

(** Template execution context with parent chain *)
type context =
  | Root of context_values  (** Root context *)
  | Child of { values : context_values; parent : context; root : context }
      (** Child context with parent and root reference *)

type custom_helper = literal_or_collection list -> literal_or_collection option
(** Custom helper function type *)

type custom_helper_lookup_fn = string -> custom_helper option
(** Function to look up custom helpers by name *)

type partial_lookup_fn = string -> string option
(** Function to look up partial templates by name *)

val make_ctx :
  ?parent_ctx:context ->
  ?extras:(string * literal_or_collection) List.t ->
  literal_or_collection ->
  context
(** Create a context from a value with optional parent *)

val lookup : context -> ident_path_segment list -> literal_or_collection
(** Look up a value in context using an identifier path, returns `Null if not
    found *)

val eval :
  context ->
  custom_helper_lookup_fn ->
  evalable ->
  (literal_or_collection, compile_error) result
(** Evaluate an expression in a given context *)

val compile_tokens :
  custom_helper_lookup_fn ->
  partial_lookup_fn ->
  token list ->
  literal_or_collection ->
  compile_result
(** Compile a list of tokens with given context and helpers *)

val string_of_literal : literal_or_collection -> string
(** Convert a literal_or_collection to string representation *)

val escape_html : string -> string
(** Escape HTML entities in a string *)

val default_get_helper : custom_helper_lookup_fn
(** Default helper lookup function with built-in helpers *)

val default_get_partial : partial_lookup_fn
(** Default partial lookup function (returns None for all partials) *)

val compile :
  ?get_helper:custom_helper_lookup_fn ->
  ?get_partial:partial_lookup_fn ->
  string ->
  literal_or_collection ->
  hb_result
(** Main compilation function *)
