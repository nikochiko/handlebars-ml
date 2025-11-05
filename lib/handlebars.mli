module Parser = Parser
module Compiler = Compiler
module Types = Types

type custom_helper = Compiler.custom_helper
(** Type alias for custom helper functions *)

type hb_error = Compiler.hb_error [@@deriving show]
(** Type alias for handlebars error type *)

type hb_result = Compiler.hb_result
(** Type alias for handlebars result type *)

val default_get_helper : Compiler.custom_helper_lookup_fn
(** Default helper lookup function with built-in helpers *)

val default_get_partial : Compiler.partial_lookup_fn
(** Default partial lookup function (returns None for all partials) *)

val compile :
  ?get_helper:Compiler.custom_helper_lookup_fn ->
  ?get_partial:Compiler.partial_lookup_fn ->
  string ->
  Types.literal_or_collection ->
  Compiler.hb_result
(** Main compilation function *)

val string_of_literal : Types.literal_or_collection -> string
(** Convert a literal_or_collection to string representation *)

val escape_html : string -> string
(** Escape HTML entities in a string *)
