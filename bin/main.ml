open Handlebars_ml

type input_source = Stdin | File of string

type config = {
  template_source : input_source;
  data_source : input_source;
  inline_json : string option;
  partials_dir : string option;
}

let version = "0.2.1"

let usage_msg =
  "handlebars-ml [OPTIONS] [TEMPLATE_FILE]\n\n"
  ^ "Compile handlebars templates with JSON data.\n\n" ^ "Examples:\n"
  ^ "  echo '{\"name\":\"World\"}' | handlebars-ml template.hbs\n"
  ^ "  handlebars-ml -d data.json template.hbs\n"
  ^ "  handlebars-ml -j '{\"name\":\"Alice\"}' <<< 'Hello {{name}}!'\n"
  ^ "  handlebars-ml --partials-dir ./partials template.hbs < data.json\n"

let help_msg =
  usage_msg ^ "\nOptions:\n"
  ^ "  -d, --data FILE     JSON data file (default: stdin)\n"
  ^ "  -j, --json STRING   Inline JSON data\n"
  ^ "  -p, --partials-dir  DIR  Load partials from directory (*.hbs files)\n"
  ^ "  -h, --help          Show this help\n"
  ^ "  -v, --version       Show version\n"

let read_file filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Ok content
  with Sys_error msg -> Error msg

let read_stdin () =
  try
    let buffer = Buffer.create 4096 in
    let rec read_loop () =
      try
        let line = input_line stdin in
        Buffer.add_string buffer line;
        Buffer.add_char buffer '\n';
        read_loop ()
      with End_of_file -> ()
    in
    read_loop ();
    Ok (Buffer.contents buffer)
  with Sys_error msg -> Error msg

let read_input source =
  match source with
  | Stdin -> read_stdin ()
  | File filename -> read_file filename

let parse_json json_str =
  try Ok (Yojson.Safe.from_string json_str)
  with Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)

let yojson_to_literal_or_collection (json : Yojson.Safe.t) :
    Types.literal_or_collection =
  let rec convert = function
    | `Null -> `Null
    | `Bool b -> `Bool b
    | `Int i -> `Int i
    | `Intlit s -> `Intlit s
    | `Float f -> `Float f
    | `String s -> `String s
    | `List lst -> `List (List.map convert lst)
    | `Assoc lst -> `Assoc (List.map (fun (k, v) -> (k, convert v)) lst)
    | `Tuple _ | `Variant _ ->
        failwith "Unsupported Yojson variant: Tuple or Variant"
  in
  convert json

let load_partials_from_dir dir =
  try
    let entries = Sys.readdir dir in
    let partials = ref [] in
    Array.iter
      (fun entry ->
        if String.ends_with ~suffix:".hbs" entry then
          let filepath = Filename.concat dir entry in
          let name = Filename.remove_extension entry in
          match read_file filepath with
          | Ok content -> partials := (name, content) :: !partials
          | Error _ -> () (* Skip files that can't be read *))
      entries;
    Ok !partials
  with Sys_error msg -> Error ("Failed to read partials directory: " ^ msg)

let error_and_exit msg code =
  Printf.eprintf "Error: %s\n" msg;
  exit code

let parse_args () =
  let config =
    ref
      {
        template_source = Stdin;
        data_source = Stdin;
        inline_json = None;
        partials_dir = None;
      }
  in
  let template_file = ref None in
  let set_template_file file = template_file := Some file in

  let rec parse_args_rec = function
    | [] -> ()
    | "-h" :: _ | "--help" :: _ ->
        print_string help_msg;
        exit 0
    | "-v" :: _ | "--version" :: _ ->
        Printf.printf "handlebars-ml %s\n" version;
        exit 0
    | "-d" :: file :: rest | "--data" :: file :: rest ->
        config := { !config with data_source = File file };
        parse_args_rec rest
    | "-j" :: json :: rest | "--json" :: json :: rest ->
        config := { !config with inline_json = Some json };
        parse_args_rec rest
    | "-p" :: dir :: rest | "--partials-dir" :: dir :: rest ->
        config := { !config with partials_dir = Some dir };
        parse_args_rec rest
    | file :: rest when not (String.starts_with ~prefix:"-" file) ->
        set_template_file file;
        parse_args_rec rest
    | unknown :: _ -> error_and_exit ("Unknown option: " ^ unknown) 1
  in

  parse_args_rec (List.tl (Array.to_list Sys.argv));

  (* Set template source based on whether file was provided *)
  let final_config =
    match !template_file with
    | Some file -> { !config with template_source = File file }
    | None -> !config
  in
  final_config

let main () =
  let config = parse_args () in

  (* Read template *)
  let template =
    match read_input config.template_source with
    | Ok content -> content
    | Error msg -> error_and_exit ("Failed to read template: " ^ msg) 2
  in

  (* Get JSON data *)
  let json_data =
    match config.inline_json with
    | Some json -> json
    | None -> (
        match read_input config.data_source with
        | Ok content -> content
        | Error msg -> error_and_exit ("Failed to read data: " ^ msg) 2)
  in

  (* Parse JSON *)
  let data =
    match parse_json json_data with
    | Ok json -> yojson_to_literal_or_collection json
    | Error msg -> error_and_exit msg 3
  in

  (* Load partials from directory if specified *)
  let all_partials =
    match config.partials_dir with
    | None -> []
    | Some dir -> (
        match load_partials_from_dir dir with
        | Ok dir_partials -> dir_partials
        | Error msg -> error_and_exit msg 2)
  in

  (* Create partial lookup function *)
  let get_partial name = List.assoc_opt name all_partials in

  (* Compile template *)
  match compile ~get_partial template data with
  | Ok result ->
      print_string result;
      exit 0
  | Error (ParseError err) ->
      error_and_exit ("Parsing error: " ^ Parser.show_parse_error err) 4
  | Error (CompileError err) ->
      error_and_exit ("Compile error: " ^ Compiler.show_compile_error err) 4

let () = main ()
