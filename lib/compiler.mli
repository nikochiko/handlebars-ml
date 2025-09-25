open Types

(** Compilation errors that can occur during template processing *)
type compile_error =
  | Missing_helper of string (** Helper function not found *)
  | Type_error of string (** Type mismatch or invalid operation *)
[@@deriving show]

(** Result type for compilation operations *)
type compile_result = (string, compile_error) result

(** Combined error type for both lexing and compilation errors *)
type hb_error =
  | LexError of lex_error (** Error during lexical analysis *)
  | CompileError of compile_error (** Error during compilation *)
[@@deriving show]

(** Result type for complete handlebars processing *)
type hb_result = (string, hb_error) result [@@deriving show]

(** Context value wrapper for template variables *)
type context_value =
  | Simple of literal_or_collection (** Simple value without extras *)
  | WithExtras of {
      v : literal_or_collection;
      extras : literal_or_collection
    } (** Value with additional context data (@index, @key, etc.) *)

(** Template execution context with parent chain *)
type context =
  | Root of { v : context_value } (** Root context *)
  | Child of { v : context_value; parent : context } (** Child context with parent reference *)

(** Custom helper function type *)
type custom_helper = literal_or_collection list -> literal_or_collection option

(** Function to look up custom helpers by name *)
type custom_helper_lookup_fn = string -> custom_helper option

(** Result type for builtin helper functions *)
type builtin_helper_result =
  | Value of literal_or_collection (** Simple value result *)
  | ContextList of context list (** List of contexts for iteration helpers *)

(** Builtin helper function type with context access *)
type builtin_helper = literal_or_collection list -> context -> builtin_helper_result

(** Unified helper type *)
type helper =
  | Custom of custom_helper (** User-defined helper *)
  | Builtin of builtin_helper (** Built-in helper with context access *)

(** Create a context from a value with optional parent *)
val make_ctx : ?parent_ctx:context -> context_value -> context

(** Convert a literal to literal_or_collection *)
val literal_or_collection_of_literal : literal -> literal_or_collection

(** Check if a value is considered truthy in handlebars semantics *)
val is_truthy : literal_or_collection -> bool

(** Look up a value in context using an identifier path, returns `Null if not found *)
val lookup : context -> ident_path_segment list -> literal_or_collection

(** Evaluate an expression in a given context *)
val eval :
  context ->
  custom_helper_lookup_fn ->
  evalable ->
  (literal_or_collection, compile_error) result

(** Convert a literal_or_collection to string representation *)
val string_of_literal : literal_or_collection -> string

(** Escape HTML entities in a string *)
val escape_html : string -> string

(** Compile a list of tokens with given context and helpers *)
val compile_tokens :
  custom_helper_lookup_fn ->
  token list ->
  literal_or_collection ->
  compile_result

(** Default helper lookup function with built-in helpers *)
val default_get_helper : custom_helper_lookup_fn

(** Built-in 'each' helper for iteration *)
val builtin_helper_each : builtin_helper

(** Main compilation function *)
val compile :
  ?get_helper:custom_helper_lookup_fn ->
  string ->
  literal_or_collection ->
  hb_result

(** Utility functions for whitespace control *)
val trim_left : Uchar.t array -> Uchar.t array
val trim_right : Uchar.t array -> Uchar.t array

(** Helper to check if a character is whitespace *)
val is_space : Uchar.t -> bool