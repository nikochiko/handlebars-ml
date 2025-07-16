open Types

type handlebars_error = LexError of lex_error | CompileError of string
[@@deriving show]

type hb_result = (string, handlebars_error) result [@@deriving show]

let ( >>= ) = Option.bind
let ( let* ) = ( >>= )

let is_space c =
  match Uchar.to_char c with
  | ' ' | '\012' | '\t' | '\r' | '\n' -> true
  | _ -> false

let trim_left c_arr =
  let i = ref 0 in
  while !i < Array.length c_arr && is_space c_arr.(!i) do
    incr i
  done;
  Array.sub c_arr !i (Array.length c_arr - !i)

let trim_right c_arr =
  let i = ref (Array.length c_arr - 1) in
  while !i >= 0 && is_space c_arr.(!i) do
    decr i
  done;
  Array.sub c_arr 0 (!i + 1)

let string_of_literal (lit : literal) : string =
  match lit with
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | _ -> ""

let escape_html s =
  (* borrowed from ocaml-mustache
   * https://github.com/rgrinberg/ocaml-mustache/blob/55abaef61e285be1b92b3b05f5560279f908dc28/mustache/lib/mustache.ml#L47-L58 
   *)
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

let rec is_truthy kind (lit : literal option) : bool =
  match kind with
  | `Each -> (
      match lit with
      | Some (`List lst) -> not (List.is_empty lst)
      | Some (`Assoc lst) -> not (List.is_empty lst)
      | _ -> false)
  | `Unless -> not (is_truthy `If lit)
  | `If | `With | `Mustache _ -> (
      match lit with
      | None
      | Some `Null
      | Some (`String "")
      | Some (`Int 0)
      | Some (`Float 0.0)
      | Some (`Bool false)
      | Some (`List [])
      | Some (`Assoc []) ->
          false
      | _ -> true)

type context =
  | Root of { v : Yojson.Basic.t }
  | Child of { v : Yojson.Basic.t; parent : context }

let lookup ctx segments : Yojson.Basic.t option =
  let rec aux ctx segments =
    let v = match ctx with Root { v } -> v | Child { v; _ } -> v in
    match segments with
    | [] -> Some v
    | `Ident name :: rest | `Index (`String name) :: rest ->
        let* next_v =
          match v with `Assoc lst -> List.assoc_opt name lst | _ -> None
        in
        let new_ctx = Child { v = next_v; parent = ctx } in
        aux new_ctx rest
    | `Index (`Int idx) :: rest when idx >= 0 ->
        let* next_v =
          match v with `List lst -> List.nth_opt lst idx | _ -> None
        in
        aux (Child { v = next_v; parent = ctx }) rest
    | `DotPath `OneDot :: rest -> aux ctx rest
    | `DotPath `TwoDot :: rest ->
        let parent_ctx =
          match ctx with Root _ -> ctx | Child { parent; _ } -> parent
        in
        aux parent_ctx rest
    | _ -> None
  in
  aux ctx segments

let rec eval ctx (expr : evalable) : Yojson.Basic.t option =
  match expr with
  | `Literal lit -> Some lit
  | `App _ -> None (* not implemented *)
  | `IdentPath segments -> lookup ctx segments
  | `WhateverMakesSense exprs -> List.to_seq exprs |> Seq.find_map (eval ctx)

let make_ctx ?parent_ctx v =
  match parent_ctx with
  | None -> Root { v }
  | Some ctx -> Child { v; parent = ctx }

let compile_tokens (tokens : token list) values =
  let ( let* ) = Result.bind in
  let rec aux acc ctx (tokens : token list) =
    match tokens with
    | [] -> Ok (List.rev acc |> String.concat "")
    | `Comment _ :: rest -> aux acc ctx rest
    | `WhitespaceControl :: `Raw s :: rest ->
        let trimmed = trim_left s in
        aux acc ctx (`Raw trimmed :: rest)
    | `Raw s :: `WhitespaceControl :: rest ->
        let trimmed = trim_right s |> string_of_uchar_array in
        aux (trimmed :: acc) ctx rest
    | `WhitespaceControl :: rest -> aux acc ctx rest
    | `Raw s :: rest ->
        let raw_str = string_of_uchar_array s in
        aux (raw_str :: acc) ctx rest
    | `Escaped expr :: rest -> (
        match eval ctx expr with
        | None -> aux acc ctx rest
        | Some value ->
            let escaped_str = string_of_literal value |> escape_html in
            aux (escaped_str :: acc) ctx rest)
    | `Unescaped expr :: rest -> (
        match eval ctx expr with
        | None -> aux acc ctx rest
        | Some value ->
            let unescaped_str = string_of_literal value in
            aux (unescaped_str :: acc) ctx rest)
    | `Block { kind; expr; content; else_content } :: rest ->
        let evaluated = eval ctx expr in
        let is_truthy = is_truthy kind evaluated in
        let ctx =
          match (evaluated, kind) with
          | (Some v, `With | Some v, `Mustache _) when is_truthy ->
              make_ctx ~parent_ctx:ctx v
          | _ -> ctx
        in
        let sub_compiler =
          match kind with
          | (`With | `Mustache _) when is_truthy ->
              let v = Option.get evaluated in
              let ctx = make_ctx ~parent_ctx:ctx v in
              aux [] ctx
          | `Each when is_truthy ->
              let v = Option.get evaluated in
              let iterable_values =
                match v with
                | `List lst -> lst
                | `Assoc lst -> List.map snd lst
                | _ -> failwith "Expected a list or assoc for `Each block"
              in
              let compile_for_each v content =
                let ctx = make_ctx ~parent_ctx:ctx v in
                aux [] ctx content
              in
              fun content ->
                let* all_compiled =
                  List.fold_left
                    (fun acc v ->
                      match acc with
                      | Error _ -> acc
                      | Ok lst ->
                          let* compiled = compile_for_each v content in
                          Ok (compiled :: lst))
                    (Ok []) iterable_values
                in
                Ok (List.rev all_compiled |> String.concat "")
          | _ -> aux [] ctx
        in
        let* compiled_block =
          if is_truthy then sub_compiler content else sub_compiler else_content
        in
        aux (compiled_block :: acc) ctx rest
  in
  aux [] (make_ctx values) tokens

let compile template values =
  let lexbuf = uchar_array_of_string template |> Sedlexing.from_uchar_array in
  let result = Lexer.lex lexbuf in
  match result with
  | Error e -> Error (LexError e)
  | Ok tokens -> compile_tokens tokens values

let make_test template values expected =
  let result = compile template values in
  match result = expected with
  | true -> true
  | false ->
      Printf.printf "------ Test failed: -----\n";
      Printf.printf "Input:    \t%s\n" template;
      Printf.printf "Values:   \t%s\n" (Yojson.Basic.pretty_to_string values);
      Printf.printf "Output:   \t%s\n" (show_hb_result result);
      Printf.printf "Expected: \t%s\n" (show_hb_result expected);
      false

let%test "compile simple template" =
  let template = "{{firstname}} {{lastname}}" in
  let values =
    `Assoc [ ("firstname", `String "Yahuda"); ("lastname", `String "Katz") ]
  in
  make_test template values (Ok "Yahuda Katz")

let%test "compile simple template with whitespace control" =
  let template = "{{firstname~}} {{lastname}}" in
  let values =
    `Assoc [ ("firstname", `String "Yahuda"); ("lastname", `String "Katz") ]
  in
  make_test template values (Ok "YahudaKatz")

let%test "compile template with nested context" =
  let template =
    {|
    {{~#user}}
      {{~#profile~}}
        Name: {{name}}, Age: {{age}}
      {{~/profile}}
    {{~/user~}}
    |}
  in
  let values =
    `Assoc
      [
        ( "user",
          `Assoc
            [
              ("profile", `Assoc [ ("name", `String "Alice"); ("age", `Int 30) ]);
            ] );
      ]
  in
  make_test template values (Ok "Name: Alice, Age: 30")
