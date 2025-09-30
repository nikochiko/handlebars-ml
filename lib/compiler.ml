open Types

type compile_error =
  | Missing_helper of string
      [@printer fun fmt -> fprintf fmt "Missing helper: %s"]
  | Missing_partial of string
      [@printer fun fmt -> fprintf fmt "Missing partial: %s"]
  | Partial_parse_error of string * Parser.parse_error
      [@printer
        fun fmt (name, e) ->
          Format.fprintf fmt "Parsing error in partial \"%s\": %s" name
            (Parser.show_parse_error e)]
  | Partial_compile_error of string * compile_error
      [@printer
        fun fmt (name, e) ->
          Format.fprintf fmt "In partial \"%s\": %s" name (show_compile_error e)]
[@@deriving show]

type compile_result = (string, compile_error) result

type hb_error =
  | ParseError of Parser.parse_error
  | CompileError of compile_error
[@@deriving show]

type hb_result = (string, hb_error) result [@@deriving show]

type context_values = {
  v : literal_or_collection;
  extras : literal_or_collection;
}

type context =
  | Root of context_values
  | Child of { values : context_values; parent : context; root : context }

type custom_helper = literal_or_collection list -> literal_or_collection option
type custom_helper_lookup_fn = string -> custom_helper option
type partial_lookup_fn = string -> string option

let ( >>= ) = Result.bind
let ( let* ) = ( >>= )

let make_ctx ?parent_ctx ?extras v =
  let extras_lst = match extras with None -> [] | Some lst -> lst in
  let extras_lst =
    match v with
    | `List l -> ("length", `Int (List.length l)) :: extras_lst
    | _ -> extras_lst
  in
  let values = { v; extras = `Assoc extras_lst } in
  match parent_ctx with
  | None -> Root values
  | Some (Root _ as root_ctx) ->
      Child { values; parent = root_ctx; root = root_ctx }
  | Some (Child { root; _ } as parent) -> Child { values; parent; root }

let literal_or_collection_of_literal (lit : literal) : literal_or_collection =
  match lit with
  | `Bool b -> `Bool b
  | `String s -> `String s
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `Null -> `Null

let is_truthy = function
  | `Null
  | `String ""
  | `Int 0
  | `Float 0.0
  | `Bool false
  | `List []
  | `Assoc [] ->
      false
  | _ -> true

let string_of_literal (lit : literal_or_collection) : string =
  match lit with
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `Null -> ""
  | `List _ | `Assoc _ -> ""

let escape_html s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&apos;"
      | '>' -> Buffer.add_string b "&gt;"
      | '<' -> Buffer.add_string b "&lt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let lookup ctx segments =
  let lookup_multi_assoc name v_lst =
    let lookup_assoc name (v : [> literal_or_collection ]) =
      match v with `Assoc lst -> List.assoc_opt name lst | _ -> None
    in
    let assoc_seq = List.to_seq v_lst in
    Seq.find_map (lookup_assoc name) assoc_seq
  in
  let rec aux ctx segments =
    let values =
      match ctx with Root values -> values | Child { values; _ } -> values
    in
    match segments with
    | [] ->
        let { v; _ } = values in
        v
    | `Ident name :: rest when name = "@root" ->
        let root_ctx =
          match ctx with (Root _ as root) | Child { root; _ } -> root
        in
        aux root_ctx rest
    | `Ident name :: rest when name = "." || name = "this" -> aux ctx rest
    | `Ident name :: rest -> (
        let { v; extras } = values in
        match lookup_multi_assoc name [ extras; v ] with
        | Some next_v ->
            let new_ctx = make_ctx ~parent_ctx:ctx next_v in
            aux new_ctx rest
        | None -> `Null)
    | `Index (`String name) :: rest -> (
        let { v; _ } = values in
        match v with
        | `Assoc lst -> (
            match List.assoc_opt name lst with
            | Some next_v ->
                let new_ctx = make_ctx ~parent_ctx:ctx next_v in
                aux new_ctx rest
            | None -> `Null)
        | _ -> `Null)
    | `Index (`Int idx) :: rest when idx >= 0 -> (
        let { v; _ } = values in
        match v with
        | `List lst -> (
            match List.nth_opt lst idx with
            | Some next_v ->
                let new_ctx = make_ctx ~parent_ctx:ctx next_v in
                aux new_ctx rest
            | None -> `Null)
        | _ -> `Null)
    | `DotPath `OneDot :: rest -> aux ctx rest
    | `DotPath `TwoDot :: rest ->
        let parent_ctx =
          match ctx with Root _ -> ctx | Child { parent; _ } -> parent
        in
        aux parent_ctx rest
    | _ -> `Null
  in
  aux ctx segments

let rec eval ctx get_helper (expr : evalable) =
  match expr with
  | `Literal lit -> Ok (literal_or_collection_of_literal lit)
  | `App (name, args) -> (
      match get_helper name with
      | None -> Error (Missing_helper name)
      | Some helper -> (
          let* arg_values =
            List.fold_left
              (fun acc arg ->
                let* lst = acc in
                let* v = eval ctx get_helper arg in
                Ok (v :: lst))
              (Ok []) args
          in
          let arg_values = List.rev arg_values in
          match helper arg_values with Some v -> Ok v | None -> Ok `Null))
  | `IdentPath segments -> Ok (lookup ctx segments)
  | `WhateverMakesSense exprs ->
      let rec try_eval = function
        | [] -> Ok `Null
        | expr :: rest -> (
            match eval ctx get_helper expr with
            | Error (Missing_helper _) -> try_eval rest
            | result -> result)
      in
      try_eval exprs

(* TODO: support hash-arguments for helpers *)
let default_get_helper name =
  let upper args =
    match args with
    | [ `String s ] -> Some (`String (String.uppercase_ascii s))
    | _ -> None
  in
  let lower args =
    match args with
    | [ `String s ] -> Some (`String (String.lowercase_ascii s))
    | _ -> None
  in
  let length args =
    match args with
    | [ `String s ] -> Some (`Int (String.length s))
    | [ `List lst ] -> Some (`Int (List.length lst))
    | [ `Assoc lst ] -> Some (`Int (List.length lst))
    | _ -> None
  in
  let concat args =
    let printables =
      List.filter
        (function `Assoc _ | `List _ | `Null -> false | _ -> true)
        args
    in
    let s = printables |> List.map string_of_literal |> String.concat "" in
    Some (`String s)
  in
  let eq = function [ a; b ] -> Some (`Bool (a = b)) | _ -> None in
  let not_ = function
    | [ any ] -> Some (`Bool (not (is_truthy any)))
    | _ -> None
  in
  let remove_protocol = function
    | [ `String url ] ->
        let re = Str.regexp "^https?://" in
        let stripped = Str.replace_first re "" url in
        Some (`String stripped)
    | _ -> None
  in
  let format_date args =
    let aux fmt s =
      let open ISO8601.Permissive in
      try
        let dt, maybe_tz = datetime_tz ~reqtime:false s in
        let tz = match maybe_tz with None -> 0. | Some tz -> tz in
        pp_format Format.str_formatter fmt dt tz;
        Some (`String (Format.flush_str_formatter ()))
      with Failure _ ->
        None (* TODO: better error handling? let the user know *)
    in
    match args with
    | [ `String s ] -> aux "%Y-%M-%D" s
    | [ `String fmt; `String s ] -> aux fmt s
    | _ -> None
  in
  let add = function
    | [ `Int a; `Int b ] -> Some (`Int (a + b))
    | [ `Float a; `Float b ] -> Some (`Float (a +. b))
    | [ `Int a; `Float b ] -> Some (`Float (float_of_int a +. b))
    | [ `Float a; `Int b ] -> Some (`Float (a +. float_of_int b))
    | _ -> None
  in
  let increment = function
    | [ `Int a ] -> Some (`Int (a + 1))
    | [ `Float a ] -> Some (`Float (a +. 1.0))
    | _ -> None
  in
  let decrement = function
    | [ `Int a ] -> Some (`Int (a - 1))
    | [ `Float a ] -> Some (`Float (a -. 1.0))
    | _ -> None
  in
  match name with
  | "upper" -> Some upper
  | "lower" -> Some lower
  | "length" -> Some length
  | "concat" -> Some concat
  | "eq" -> Some eq
  | "not" -> Some not_
  | "remove_protocol" -> Some remove_protocol
  | "format_date" -> Some format_date
  | "add" -> Some add
  | "increment" -> Some increment
  | "decrement" -> Some decrement
  | _ -> None

let default_get_partial _name = None

let apply_indentation s indentation =
  if indentation = "" then s
  else
    let lines = String.split_on_char '\n' s in
    let len = List.length lines in
    lines
    |> List.mapi (fun i line ->
           if (i = len - 1 && line = "") || i = 0 then line
           else indentation ^ line)
    |> String.concat "\n"

(* this hack is not pretty. but works for now to identify standalone templates *)
let newline_sentinel_char = '\x1B'
let newline_sentinel = "\x1B"

let compile_tokens get_helper get_partial tokens values =
  let starts_with_newline s = String.length s > 0 && (s.[0] = '\n' || s.[0] = newline_sentinel_char) in
  let swap_newline_sentinel s =
    if s.[0] = '\n' then newline_sentinel ^ String.sub s 1 (String.length s - 1)
    else s
  in
  let drop_left n s =
    if String.length s <= n then "" else String.sub s n (String.length s - n)
  in
  let handle_standalone_block ?(bv="") pre content1 content2 rest =
    let auxr lhs rhs =
      match (List.rev lhs, rhs) with
      | (`Whitespace s1) :: lhs_rev_rest, `Whitespace s2 :: rhs_rest
        when starts_with_newline s1 && starts_with_newline s2 ->
          let left_ws = if bv <> "" then `Whitespace s1 else `Whitespace (String.make 1 s1.[0]) in
          (List.rev (left_ws :: lhs_rev_rest), `Whitespace (swap_newline_sentinel s2) :: rhs_rest)
      |   `Whitespace s1 :: lhs_rev_rest,
          `Whitespace _ :: `Whitespace s2 :: rhs_rest
        when starts_with_newline s1 && starts_with_newline s2 ->
          let left_ws = if bv <> "" then `Whitespace s1 else `Whitespace (String.make 1 s1.[0]) in
          (List.rev (left_ws :: lhs_rev_rest), `Whitespace (swap_newline_sentinel s2) :: rhs_rest)
      | _ -> (lhs, rhs)
    in
    let pre = if pre = "" then newline_sentinel else pre in
    let pre', content1 = auxr [ `Whitespace pre ] content1 in
    let pre =
      match pre' with
      | [] -> ""
      | `Whitespace s :: _ when String.length s > 0 && s.[0] = newline_sentinel_char -> drop_left 1 s
      | `Whitespace s :: _ -> s
      | _ -> pre
    in
    match content2 with
    | [] ->
        let content1, rest = auxr content1 rest in
        (pre, content1, content2, rest)
    | _ ->
        let content1, content2 = auxr content1 content2 in
        let content2, rest = auxr content2 rest in
        (pre, content1, content2, rest)
  in
  let rec compile_maybe_standalone_ws pre curr acc ctx rest =
    let* (acc, ctx, rest) = match curr with
    | `Partial { name; context; hash_args } -> (
        let indentation =
          match pre with
          | s when starts_with_newline s -> String.sub s 1 (String.length s - 1)
          | _ -> pre
        in
        let* v = compile_partial ~indentation name context hash_args ctx in
        let pre, rest, _, _ = handle_standalone_block ~bv:v pre rest [] [] in
        let acc = v :: pre :: acc in
        Ok (acc, ctx, rest))
    | `Block { expr; content; else_content; kind } when kind = Section -> (
        let pre, content, else_content, rest =
          handle_standalone_block pre content else_content rest
        in
        let* compiled_block = compile_block expr content else_content ctx in
        let acc = compiled_block :: pre :: acc in
        Ok (acc, ctx, rest))
    | `Block { expr; content; else_content; kind } when kind = InvertedSection
      -> (
        let pre, else_content, content, rest =
          handle_standalone_block pre else_content content rest
        in
        let* compiled_block = compile_block expr content else_content ctx in
        let acc = compiled_block :: pre :: acc in
        Ok (acc, ctx, rest))
    | _ -> failwith "expected partial or block only"
    in match rest with
    | (`Partial _ | `Block _ as curr) :: rest' ->
        compile_maybe_standalone_ws "" curr acc ctx rest'
    | _ -> compile_token_list acc ctx rest
  and compile_token_list acc ctx tokens =
    match tokens with
    | [] -> Ok (List.rev acc |> String.concat "")
    | `Comment :: rest
    | `WhitespaceControl :: `Whitespace _ :: rest
    | `Whitespace _ :: `WhitespaceControl :: rest
    | `WhitespaceControl :: rest ->
        compile_token_list acc ctx rest
    | `Raw s :: rest -> compile_token_list (s :: acc) ctx rest
    | `Whitespace pre :: ((`Block _ | `Partial _) as curr) :: rest
      when starts_with_newline pre ->
        compile_maybe_standalone_ws pre curr acc ctx rest
    | `Whitespace s :: rest when String.length s > 0 && s.[0] = newline_sentinel_char ->
        compile_token_list (String.sub s 1 (String.length s - 1) :: acc) ctx rest
    | `Whitespace s :: rest -> compile_token_list (s :: acc) ctx rest
    | `Escaped expr :: rest ->
        let* value = eval ctx get_helper expr in
        let escaped_str = string_of_literal value |> escape_html in
        compile_token_list (escaped_str :: acc) ctx rest
    | `Unescaped expr :: rest ->
        let* value = eval ctx get_helper expr in
        let unescaped_str = string_of_literal value in
        compile_token_list (unescaped_str :: acc) ctx rest
    | `Partial { name; context; hash_args } :: rest ->
        let* compiled_partial = compile_partial name context hash_args ctx in
        compile_token_list (compiled_partial :: acc) ctx rest
    | `Block { expr; content; else_content; _ } :: rest ->
        let* compiled_block = compile_block expr content else_content ctx in
        compile_token_list (compiled_block :: acc) ctx rest
  and compile_block expr content else_content ctx =
    match expr with
    | `App ("if", [ condition ]) ->
        let* value = eval ctx get_helper condition in
        let content_to_use =
          if is_truthy value then content else else_content
        in
        compile_token_list [] ctx content_to_use
    | `App ("with", [ context_expr ]) ->
        let* v = eval ctx get_helper context_expr in
        let new_ctx = make_ctx ~parent_ctx:ctx v in
        let content_to_use = if is_truthy v then content else else_content in
        compile_token_list [] new_ctx content_to_use
    | `App ("each", [ iterable_expr ]) -> (
        let* value = eval ctx get_helper iterable_expr in
        if not (is_truthy value) then compile_token_list [] ctx else_content
        else
          match value with
          | `List lst ->
              let compile_for_each i v =
                let extras =
                  [
                    ("@index", `Int i);
                    ("@first", `Bool (i = 0));
                    ("@last", `Bool (i = List.length lst - 1));
                  ]
                in
                let item_ctx = make_ctx ~parent_ctx:ctx ~extras v in
                compile_token_list [] item_ctx content
              in
              let* all_compiled =
                List.mapi compile_for_each lst
                |> List.fold_left
                     (fun acc_result compiled_result ->
                       let* acc = acc_result in
                       let* compiled = compiled_result in
                       Ok (compiled :: acc))
                     (Ok [])
              in
              Ok (List.rev all_compiled |> String.concat "")
          | `Assoc lst ->
              let compile_for_each (k, v) =
                let extras = [ ("@key", `String k) ] in
                let item_ctx = make_ctx ~parent_ctx:ctx ~extras v in
                compile_token_list [] item_ctx content
              in
              let* all_compiled =
                List.map compile_for_each lst
                |> List.fold_left
                     (fun acc_result compiled_result ->
                       let* acc = acc_result in
                       let* compiled = compiled_result in
                       Ok (compiled :: acc))
                     (Ok [])
              in
              Ok (List.rev all_compiled |> String.concat "")
          | _ -> compile_token_list [] ctx else_content)
    | other_expr -> (
        let* v = eval ctx get_helper other_expr in
        match is_truthy v with
        | true -> compile_token_list [] (make_ctx ~parent_ctx:ctx v) content
        | false -> compile_token_list [] ctx else_content)
  and compile_partial ?(indentation = "") name context_opt hash_args ctx =
    match get_partial name with
    | None -> Error (Missing_partial name)
    | Some partial_template -> (
        (* Determine context for partial *)
        let* partial_ctx =
          match context_opt with
          | None -> Ok ctx (* inherit current context *)
          | Some context_expr ->
              let* v = eval ctx get_helper context_expr in
              Ok (make_ctx ~parent_ctx:ctx v)
        in
        (* Evaluate hash arguments and add them to context *)
        let* partial_ctx_with_hash =
          if hash_args = [] then Ok partial_ctx
          else
            (* Evaluate hash arguments *)
            let* hash_values =
              List.fold_left
                (fun acc_result (key, expr) ->
                  let* acc = acc_result in
                  let* value = eval ctx get_helper expr in
                  Ok ((key, value) :: acc))
                (Ok []) hash_args
            in
            let hash_values = List.rev hash_values in
            (* Merge hash arguments with context *)
            let values =
              match partial_ctx with
              | Root values -> values
              | Child { values; _ } -> values
            in
            let { v; extras } = values in
            let new_extras =
              match extras with
              | `Assoc lst -> hash_values @ lst
              (* TODO: not happy with runtime error for this *)
              | _ -> failwith "extras should always be assoc"
            in
            let new_ctx =
              make_ctx ~parent_ctx:partial_ctx ~extras:new_extras v
            in
            Ok new_ctx
        in
        (* Parse and compile the partial template *)
        let lexbuf = Lexing.from_string partial_template in
        match Parser.parse lexbuf with
        | Error e -> Error (Partial_parse_error (name, e))
        | Ok partial_tokens -> (
            let partial_tokens = `Whitespace newline_sentinel :: partial_tokens in
            match
              compile_token_list [] partial_ctx_with_hash partial_tokens
            with
            | Error e -> Error (Partial_compile_error (name, e))
            | Ok compiled -> Ok (apply_indentation compiled indentation)))
  in
  let ctx = make_ctx values in
  let tokens = `Whitespace newline_sentinel :: tokens in
  compile_token_list [] ctx tokens

let compile ?(get_helper = default_get_helper)
    ?(get_partial = default_get_partial) template values =
  let lexbuf = Lexing.from_string template in
  match Parser.parse lexbuf with
  | Error e -> Error (ParseError e)
  | Ok tokens -> (
      match compile_tokens get_helper get_partial tokens values with
      | Error e -> Error (CompileError e)
      | Ok result -> Ok result)

