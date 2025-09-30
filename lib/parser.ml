open Handlebars_lexer

let ( let* ) = Result.bind
let ( >>= ) = Result.bind

let equal_pos a b =
  a.Lexing.pos_cnum = b.Lexing.pos_cnum
  && a.Lexing.pos_bol = b.Lexing.pos_bol
  && a.Lexing.pos_lnum = b.Lexing.pos_lnum

let equal_buf _ _ = true

type parse_error = {
  msg : string;
  pos : Lexing.position; [@equal equal_pos]
  buf : Lexing.lexbuf; [@equal equal_buf]
}
[@@deriving eq]

let string_of_path_segment segment =
  match segment with
  | `Ident name -> name
  | `DotPath `OneDot -> "."
  | `DotPath `TwoDot -> ".."
  | `Index (`String s) -> Printf.sprintf "['%s']" s
  | `Index (`Int i) -> Printf.sprintf "[%d]" i
  | _ -> failwith "not implemented"

let string_of_ident_path (path : Types.ident_path) =
  let aux path =
    let (`IdentPath segments) = path in
    match segments with
    | [] -> failwith "ident path shouldn't be empty"
    | _ ->
        segments
        |> List.fold_left
             (fun (prev, acc) this ->
               let s_this = string_of_path_segment this in
               match (prev, this) with
               | Some (`DotPath _), _ -> (Some this, acc ^ "/" ^ s_this)
               | Some _, `DotPath _ -> (Some this, acc ^ "/" ^ s_this)
               | Some _, `Ident _ -> (Some this, acc ^ "." ^ s_this)
               | _, _ -> (Some this, acc ^ s_this))
             (None, "")
  in
  let _, result = aux path in
  result

let string_of_literal lit : string =
  match lit with
  | `String s -> s
  | `Int i -> string_of_int i
  | _ -> failwith "not implemented"

let rec string_of_evalable (evalable : Types.evalable) =
  match evalable with
  | `IdentPath path -> string_of_ident_path (`IdentPath path)
  | `App (name, args) -> (
      match args with
      | [] -> name
      | _ ->
          let args_str =
            args |> List.map string_of_evalable |> String.concat ", "
          in
          Printf.sprintf "%s(%s)" name args_str)
  | `WhateverMakesSense exprs -> string_of_evalable (List.hd exprs)
  | `Literal l -> string_of_literal l

let mk_err msg buf : parse_error = { msg; pos = buf.Lexing.lex_curr_p; buf }

let pp_position fmt pos =
  Format.fprintf fmt "line %d, column %d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let pp_parse_error fmt { msg; pos; _ } =
  pp_position fmt pos;
  Format.fprintf fmt ": %s" msg

let show_parse_error e =
  pp_parse_error Format.str_formatter e;
  Format.flush_str_formatter ()

type parse_result = (Types.token list, parse_error) result [@@deriving show, eq]

let mlex f buf = try Ok (f buf) with Failure msg -> Error (mk_err msg buf)

type container =
  | Root of Types.token list
  | Unclosed of { parent : container; block : Types.block }
  | UnclosedInverted of { parent : container; block : Types.block }

let mk_block ~kind evalable =
  { Types.expr = evalable; kind; content = []; else_content = [] }

let add_token container token =
  match container with
  | Root acc -> Root (token :: acc)
  | Unclosed { parent; block } ->
      let block = { block with content = token :: block.content } in
      Unclosed { parent; block }
  | UnclosedInverted { parent; block } ->
      let block = { block with else_content = token :: block.else_content } in
      UnclosedInverted { parent; block }

let ( ++ ) = add_token

let mature_unclosed container =
  match container with
  | Unclosed { parent; block } | UnclosedInverted { parent; block } ->
      let block =
        {
          block with
          content = List.rev block.content;
          else_content = List.rev block.else_content;
        }
      in
      parent ++ `Block block
  | _ -> failwith "Cannot mature a non-unclosed block"

let invert_unclosed container =
  match container with
  | Unclosed { parent; block } -> UnclosedInverted { parent; block }
  | UnclosedInverted _ ->
      failwith "This looks like an extra 'else' block in the template"
  | _ -> failwith "Cannot invert a non-unclosed block"

let parse_literal (lit : Yojson.Basic.t) : Types.literal =
  match lit with
  | `String s -> `String s
  | `Int i -> `Int i
  | `Float f -> `Float f
  | _ -> failwith "Cannot lift non-literal to literal"

let parse_path_segment token : Types.ident_path_segment =
  match token with
  | IDENT id -> `Ident id
  | INDEX (`String s) -> `Index (`String s)
  | INDEX (`Int i) -> `Index (`Int i)
  | ONEDOT -> `DotPath `OneDot
  | TWODOT -> `DotPath `TwoDot

let mk_closing_path expr =
  let rec aux = function
    | `IdentPath path -> Some (`IdentPath path)
    | `App (name, _) -> Some (`IdentPath [ `Ident name ])
    | `WhateverMakesSense exprs ->
        let expr_seq = List.to_seq exprs in
        Seq.find_map aux expr_seq
    | _ -> None
  in
  match aux expr with
  | Some path -> path
  | None -> failwith "mk_closing_path: no path found in expr"

let rec parse_root container buf =
  let* lexres = mlex lex buf in
  match lexres with
  | EOF -> (
      match container with
      | Root acc -> Ok (List.rev acc)
      | Unclosed _ | UnclosedInverted _ ->
          Error (mk_err "unmatched block (did you forget an {{/}}?)" buf))
  | WHITESPACE s -> parse_root (container ++ `Whitespace s) buf
  | RAW s -> parse_root (container ++ `Raw s) buf
  | TEMPL_OPEN { ws_control; kind } -> (
      let container =
        if ws_control then container ++ `WhitespaceControl else container
      in
      match kind with
      | Escaped -> parse_escaped container buf
      | Unescaped -> parse_unescaped container buf
      | Partial ->
          let* partial_info, ws_control = parse_partial buf in
          let container = container ++ `Partial partial_info in
          let container =
            if ws_control then container ++ `WhitespaceControl else container
          in
          parse_root container buf
      | Section -> parse_section container buf
      | InvertedSection -> parse_inverted_section container buf
      | CloseSection -> parse_close_section container buf)
  | COMMENT -> parse_root (container ++ `Comment) buf
  | _ -> Error (mk_err "unexpected token in root" buf)

and parse_escaped container buf =
  let* inner, ws_control = parse_templ buf in
  let* container =
    match inner with
    | `App ("else", []) -> (
        try Ok (invert_unclosed container)
        with Failure e -> Error (mk_err e buf))
    | _ -> Ok (container ++ `Escaped inner)
  in
  let container =
    if ws_control then container ++ `WhitespaceControl else container
  in
  parse_root container buf

and parse_unescaped container buf =
  let* inner, ws_control = parse_templ ~is_unescaped:true buf in
  let container = container ++ `Unescaped inner in
  let container =
    if ws_control then container ++ `WhitespaceControl else container
  in
  parse_root container buf

and parse_templ ?(is_unescaped = false) buf =
  let expect_templ_close result = function
    | TEMPL_CLOSE { ws_control; is_unescaped = t_is_unescaped; raw } ->
        if is_unescaped <> t_is_unescaped then
          let expected =
            if is_unescaped then templ_open ^ String.make 1 templ_open_char
            else templ_open
          in
          let msg =
            Printf.sprintf "expected closing tag \"%s\" but found \"%s\""
              expected raw
          in
          Error (mk_err msg buf)
        else Ok (result, ws_control)
    | _ -> Error (mk_err "expected closing tag" buf)
  in
  mlex lex_in_templ buf >>= function
  | IDENT_PATH [ IDENT name ] when name = "else" ->
      let expr = `App ("else", []) in
      mlex lex_in_templ buf >>= expect_templ_close expr
  | IDENT_PATH [ IDENT name ]
  | IDENT_PATH [ INDEX (`String name) ]
  | LITERAL (`String name) ->
      let until = function TEMPL_CLOSE _ -> true | _ -> false in
      (* TODO: add support for hash args for helpers *)
      let* pos_args, _hash_args, close_token = parse_arguments ~until buf in
      let expr =
        match pos_args with
        | [] ->
            `WhateverMakesSense [ `App (name, []); `IdentPath [ `Ident name ] ]
        | _ -> `App (name, pos_args)
      in
      close_token |> expect_templ_close expr
  | LPAREN ->
      let* name, pos_args, _hash_args, _last =
        parse_application ~until:(equal_token RPAREN) buf
      in
      mlex lex_in_templ buf >>= expect_templ_close (`App (name, pos_args))
  | IDENT_PATH path ->
      let expr = `IdentPath (List.map parse_path_segment path) in
      mlex lex_in_templ buf >>= expect_templ_close expr
  | LITERAL (`Int i) ->
      let expr = `IdentPath [ `Index (`Int i) ] in
      mlex lex_in_templ buf >>= expect_templ_close expr
  | token ->
      let msg = Printf.sprintf "unexpected token: %s" (show_token token) in
      Error (mk_err msg buf)

and parse_application ~until buf =
  mlex lex_in_templ buf >>= function
  | IDENT_PATH [ IDENT name ]
  | IDENT_PATH [ INDEX (`String name) ]
  | LITERAL (`String name) ->
      let* pos_args, hash_args, last_token = parse_arguments ~until buf in
      Ok (name, pos_args, hash_args, last_token)
  | _ -> Error (mk_err "expected function name in application" buf)

and parse_arguments ~until buf =
  let parse_arg = function
    | IDENT_PATH path -> Ok (`IdentPath (List.map parse_path_segment path))
    | LITERAL lit -> Ok (`Literal (parse_literal lit))
    | LPAREN ->
        let* name, pos_args, _hash_args, _t =
          parse_application ~until:(equal_token RPAREN) buf
        in
        Ok (`App (name, pos_args))
    | token ->
        let msg =
          Printf.sprintf "unexpected token: \"%s\"" (show_token token)
        in
        Error (mk_err msg buf)
  in
  let rec aux pos_args hash_args buf =
    mlex lex_in_templ buf >>= function
    | t when until t -> Ok (List.rev pos_args, List.rev hash_args, t)
    | START_HASH_ARG name ->
        let* v = mlex lex_in_templ buf >>= parse_arg in
        aux pos_args ((name, v) :: hash_args) buf
    | t ->
        let* arg = parse_arg t in
        aux (arg :: pos_args) hash_args buf
  in
  aux [] [] buf

and parse_section container buf =
  let* evalable, ws_control = parse_templ buf in
  let block = mk_block ~kind:Section evalable in
  let child = Unclosed { parent = container; block } in
  let child = if ws_control then child ++ `WhitespaceControl else child in
  parse_root child buf

and parse_inverted_section container buf =
  let* evalable, ws_control = parse_templ buf in
  let block = mk_block ~kind:InvertedSection evalable in
  let child = UnclosedInverted { parent = container; block } in
  let child = if ws_control then child ++ `WhitespaceControl else child in
  parse_root child buf

and parse_close_section container buf =
  let* inner, ws_control = parse_templ buf in
  match container with
  | Root _ -> Error (mk_err "unexpected close block without matching open" buf)
  | Unclosed { block; _ } | UnclosedInverted { block; _ } -> (
      let expected_closing = mk_closing_path block.expr in
      let rec make_sense = function
        | `IdentPath path -> Some (`IdentPath path)
        | `WhateverMakesSense exprs ->
            let expr_seq = List.to_seq exprs in
            Seq.find_map make_sense expr_seq
        | _ -> None
      in
      match make_sense inner with
      | Some path when Types.equal_evalable path expected_closing ->
          let container = mature_unclosed container in
          let container =
            if ws_control then container ++ `WhitespaceControl else container
          in
          parse_root container buf
      | _ ->
          let msg =
            Printf.sprintf
              "unexpected close block: \"%s\"; does not match \"%s\""
              (string_of_evalable inner)
              (string_of_evalable expected_closing)
          in
          Error (mk_err msg buf))

and parse_partial buf =
  let until = function TEMPL_CLOSE _ -> true | _ -> false in
  let* name, pos_args, hash_args, last_token = parse_application ~until buf in
  let* ws_control =
    match last_token with
    | TEMPL_CLOSE { ws_control; _ } -> Ok ws_control
    | _ -> Error (mk_err "expected closing tag" buf)
  in
  let partial_info =
    match pos_args with
    | [] -> { Types.name; context = None; hash_args }
    | [ context ] -> { Types.name; context = Some context; hash_args }
    | _ -> failwith "A partial takes exactly 1 positional argument for context"
  in
  Ok (partial_info, ws_control)

let parse lexbuf : parse_result = parse_root (Root []) lexbuf
