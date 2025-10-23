open Handlebars_ml.Compiler
open Handlebars_ml.Types
open Handlebars_ml.Parser

let hb_result_testable =
  let pp fmt = function
    | Ok s -> Format.fprintf fmt "Ok %S" s
    | Error e -> Format.fprintf fmt "Error (%s)" (Handlebars_ml.Compiler.show_hb_error e)
  in
  let equal a b = match (a, b) with
    | (Ok a, Ok b) -> String.equal a b
    | (Error a, Error b) -> Handlebars_ml.Compiler.equal_hb_error a b
    | _ -> false
  in
  Alcotest.testable pp equal

let parse_result_testable =
  let pp fmt = function
    | Ok tokens -> Format.fprintf fmt "Ok [%s]" (String.concat "; " (List.map Handlebars_ml.Types.show_token tokens))
    | Error e -> Format.fprintf fmt "Error (%s)" (Handlebars_ml.Parser.show_parse_error e)
  in
  let equal a b = match (a, b) with
    | (Ok a, Ok b) -> List.for_all2 Handlebars_ml.Types.equal_token a b
    | (Error a, Error b) -> Handlebars_ml.Parser.equal_parse_error a b
    | _ -> false
  in
  Alcotest.testable pp equal

let make_compiler_test ?(get_helper = default_get_helper)
    ?(get_partial = default_get_partial) name template values expected =
  Alcotest.test_case name `Quick (fun () ->
    let result = compile ~get_helper ~get_partial template values in
    Alcotest.(check hb_result_testable) "compiler result" expected result
  )

let make_parser_test name input expected =
  Alcotest.test_case name `Quick (fun () ->
    let buf = Lexing.from_string input in
    let result = parse buf in
    Alcotest.(check parse_result_testable) "parser result" expected result
  )

let mk_test_err msg lnum cnum =
  Error
    {
      msg;
      pos =
        { Lexing.pos_fname = ""; pos_lnum = lnum; pos_bol = 0; pos_cnum = cnum };
      buf = Lexing.from_string "";
    }

let compiler_tests = [
  make_compiler_test "simple variable substitution"
    "Hello {{name}}!"
    (`Assoc [ ("name", `String "World") ])
    (Ok "Hello World!");

  make_compiler_test "multiple variables"
    "{{firstname}} {{lastname}}"
    (`Assoc [ ("firstname", `String "John"); ("lastname", `String "Doe") ])
    (Ok "John Doe");

  make_compiler_test "missing variable returns empty string"
    "Hello {{missing}}!"
    (`Assoc [])
    (Ok "Hello !");

  make_compiler_test "nested object access"
    "{{user.name}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ])
    (Ok "Alice");

  make_compiler_test "escaped output by default"
    "{{content}}"
    (`Assoc [ ("content", `String "<script>alert('xss')</script>") ])
    (Ok "&lt;script&gt;alert(&apos;xss&apos;)&lt;/script&gt;");

  make_compiler_test "unescaped output with triple braces"
    "{{{content}}}"
    (`Assoc [ ("content", `String "<b>bold</b>") ])
    (Ok "<b>bold</b>");

  make_compiler_test "upper helper"
    "{{upper name}}"
    (`Assoc [ ("name", `String "hello") ])
    (Ok "HELLO");

  make_compiler_test "lower helper"
    "{{lower name}}"
    (`Assoc [ ("name", `String "WORLD") ])
    (Ok "world");

  make_compiler_test "length helper with string"
    "{{length text}}"
    (`Assoc [ ("text", `String "hello") ])
    (Ok "5");

  make_compiler_test "missing helper error"
    "{{missing_helper name}}"
    (`Assoc [ ("name", `String "test") ])
    (Error (CompileError (Missing_helper "missing_helper")));

  make_compiler_test "if block with truthy value"
    "{{#if user}}Hello {{user.name}}{{/if}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ])
    (Ok "Hello Bob");

  make_compiler_test "if block with falsy value"
    "{{#if missing}}Never shown{{/if}}"
    (`Assoc [])
    (Ok "");

  make_compiler_test "empty string is falsy"
    "{{#if empty}}truthy{{else}}falsy{{/if}}"
    (`Assoc [ ("empty", `String "") ])
    (Ok "falsy");

  make_compiler_test "zero is falsy"
    "{{#if zero}}truthy{{else}}falsy{{/if}}"
    (`Assoc [ ("zero", `Int 0) ])
    (Ok "falsy");

  make_compiler_test "non-empty string is truthy"
    "{{#if text}}truthy{{else}}falsy{{/if}}"
    (`Assoc [ ("text", `String "hello") ])
    (Ok "truthy");

  make_compiler_test "integer to string"
    "Count: {{count}}"
    (`Assoc [ ("count", `Int 42) ])
    (Ok "Count: 42");

  make_compiler_test "custom helper"
    ~get_helper:(fun name ->
      match name with
      | "greet" ->
          Some
            (function
            | [ `String name ] -> Some (`String ("Hello, " ^ name ^ "!"))
            | _ -> None)
      | _ -> default_get_helper name)
    "{{greet name}}"
    (`Assoc [ ("name", `String "Alice") ])
    (Ok "Hello, Alice!");

  make_compiler_test "each block with array"
    "{{#each items}}{{.}} {{/each}}"
    (`Assoc [ ("items", `List [ `String "a"; `String "b"; `String "c" ]) ])
    (Ok "a b c ");

  make_compiler_test "each block with object"
    "{{#each user}}{{.}} {{/each}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "John"); ("age", `Int 30) ]) ])
    (Ok "John 30 ");

  make_compiler_test "with block context"
    "{{#with user}}Hello {{name}}{{/with}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ])
    (Ok "Hello Bob");

  make_compiler_test "mustache-style block with dot-nesting"
    "{{#resume.basics}}{{name}}{{/resume.basics}}"
    (`Assoc
      [ ("resume", `Assoc [ ("basics", `Assoc [ ("name", `String "Eve") ]) ]) ])
    (Ok "Eve");

  make_compiler_test "fallback with WhateverMakesSense"
    ~get_helper:(fun name ->
      match name with
      | "name" -> Some (fun _ -> Some (`String "from helper"))
      | _ -> default_get_helper name)
    "{{name}}"
    (`Assoc [ ("name", `String "from variable") ])
    (Ok "from helper");

  make_compiler_test "basic partial inclusion should work"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "{{name}}" | _ -> None)
    "Hello {{> greeting}}!"
    (`Assoc [ ("name", `String "World") ])
    (Ok "Hello World!");

  make_compiler_test "partial with context inheritance should work"
    ~get_partial:(fun name ->
      match name with
      | "userCard" -> Some "Name: {{name}}, Age: {{age}}"
      | _ -> None)
    "{{#with user}}{{> userCard}}{{/with}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Alice"); ("age", `Int 25) ]) ])
    (Ok "Name: Alice, Age: 25");

  make_compiler_test "partial with custom context should work - Phase 2"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "Hello {{user.name}}!" | _ -> None)
    "{{> greeting}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ])
    (Ok "Hello Bob!");

  make_compiler_test "nested partials should work"
    ~get_partial:(fun name ->
      match name with
      | "outer" -> Some "Outer: {{> inner}}"
      | "inner" -> Some "Inner: {{value}}"
      | _ -> None)
    "{{> outer}}"
    (`Assoc [ ("value", `String "test") ])
    (Ok "Outer: Inner: test");

  make_compiler_test "partial recursion should be prevented"
    ~get_partial:(fun name ->
      match name with "recursive" -> Some "{{> recursive}}" | _ -> None)
    "{{> missing}}"
    (`Assoc [])
    (Error (CompileError (Missing_partial "missing")));

  make_compiler_test "partial with explicit context should work"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "Hello {{name}}!" | _ -> None)
    "{{> greeting user}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ])
    (Ok "Hello Bob!");

  make_compiler_test "partial with nested context expression should work"
    ~get_partial:(fun name ->
      match name with
      | "userCard" -> Some "Name: {{name}}, Age: {{age}}"
      | _ -> None)
    "{{> userCard user.profile}}"
    (`Assoc
      [
        ( "user",
          `Assoc
            [
              ("profile", `Assoc [ ("name", `String "Alice"); ("age", `Int 30) ]);
            ] );
      ])
    (Ok "Name: Alice, Age: 30");

  make_compiler_test "partial with literal context should work"
    ~get_partial:(fun name -> match name with "echo" -> Some "{{.}}" | _ -> None)
    "{{> echo \"Hello World\"}}"
    (`Assoc [])
    (Ok "Hello World");

  make_compiler_test "partial context should not affect parent context"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "Hi {{name}}!" | _ -> None)
    "{{name}} {{> greeting user}} {{name}}"
    (`Assoc
      [
        ("name", `String "Main");
        ("user", `Assoc [ ("name", `String "Partial") ]);
      ])
    (Ok "Main Hi Partial! Main");

  make_compiler_test "@root should reference initial context"
    "{{name}} {{#with user}}{{name}} {{@root.name}}{{/with}}"
    (`Assoc
      [
        ("name", `String "Root"); ("user", `Assoc [ ("name", `String "User") ]);
      ])
    (Ok "Root User Root");

  make_compiler_test "@root should work in each blocks"
    "{{#each items}}{{.}} - {{@root.title}} {{/each}}"
    (`Assoc
      [
        ("title", `String "List");
        ("items", `List [ `String "A"; `String "B"; `String "C" ]);
      ])
    (Ok "A - List B - List C - List ");

  make_compiler_test "@root should work in partials"
    ~get_partial:(fun name ->
      match name with
      | "item_card" -> Some "{{name}} (from {{@root.source}})"
      | _ -> None)
    "{{> item_card item}}"
    (`Assoc
      [
        ("source", `String "API");
        ("item", `Assoc [ ("name", `String "Widget") ]);
      ])
    (Ok "Widget (from API)");

  make_compiler_test "@root should work with nested contexts"
    "{{#with company}}{{#each departments}}{{name}}: {{@root.company_name}} {{/each}}{{/with}}"
    (`Assoc
      [
        ("company_name", `String "Acme Corp");
        ( "company",
          `Assoc
            [
              ( "departments",
                `List
                  [
                    `Assoc [ ("name", `String "Engineering") ];
                    `Assoc [ ("name", `String "Sales") ];
                  ] );
            ] );
      ])
    (Ok "Engineering: Acme Corp Sales: Acme Corp ");

  make_compiler_test "partial with single hash argument should work"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "Hello {{name}}!" | _ -> None)
    "{{> greeting name=\"World\"}}"
    (`Assoc [])
    (Ok "Hello World!");

  make_compiler_test "partial with multiple hash arguments should work"
    ~get_partial:(fun name ->
      match name with
      | "user-card" -> Some "Name: {{name}}, Age: {{age}}"
      | _ -> None)
    "{{> user-card name=\"Alice\" age=25}}"
    (`Assoc [])
    (Ok "Name: Alice, Age: 25");

  make_compiler_test "partial with context and hash arguments should work"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "{{msg}} {{name}}!" | _ -> None)
    "{{> greeting user msg=\"Hi\"}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ])
    (Ok "Hi Alice!");

  make_compiler_test "partial hash arguments should override context values"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "Hello {{name}}!" | _ -> None)
    "{{> greeting user name=\"Override\"}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Original") ]) ])
    (Ok "Hello Override!");

  make_compiler_test "partial with variable as hash argument value should work"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "Message: {{msg}}" | _ -> None)
    "{{> greeting msg=message}}"
    (`Assoc [ ("message", `String "Hello World") ])
    (Ok "Message: Hello World");

  make_compiler_test "partial hash arguments work with context object properties"
    ~get_partial:(fun name ->
      match name with
      | "wrapper" -> Some "{{title}}: {{name}} ({{age}})"
      | _ -> None)
    "{{> wrapper user title=\"User Profile\"}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Bob"); ("age", `Int 30) ]) ])
    (Ok "User Profile: Bob (30)");

  make_compiler_test "partial without context inherits current execution context"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "Hello {{name}}!" | _ -> None)
    "{{#with user}}{{> greeting}}{{/with}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ])
    (Ok "Hello Alice!");

  make_compiler_test "partial with hash args but no context inherits current execution context"
    ~get_partial:(fun name ->
      match name with "greeting" -> Some "{{msg}} {{name}}!" | _ -> None)
    "{{#with user}}{{> greeting msg=\"Hi\"}}{{/with}}"
    (`Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ])
    (Ok "Hi Bob!");

  make_compiler_test "partial with hash args inherits loop context variables"
    ~get_partial:(fun name ->
      match name with
      | "item-display" -> Some "{{@index}}: {{prefix}}"
      | _ -> None)
    {| {{~#each items}}{{> item-display prefix="Item"}}{{/each~}} |}
    (`Assoc [ ("items", `List [ `String "A"; `String "B" ]) ])
    (Ok "0: Item1: Item");

  make_compiler_test "partial with 'this'"
    ~get_partial:(fun name ->
      match name with "item-card" -> Some "Item: {{this}}" | _ -> None)
    "{{#each items}}{{> item-card this}} {{/each}}"
    (`Assoc [ ("items", `List [ `String "A"; `String "B" ]) ])
    (Ok "Item: A Item: B ");

  make_compiler_test "helper function: upper and lower: OK case"
    "{{upper name}} and {{lower name}}"
    (`Assoc [ ("name", `String "Test") ])
    (Ok "TEST and test");

  make_compiler_test "helper function: length: OK case"
    "Length: {{length text}}, Items: {{length items}}"
    (`Assoc
      [
        ("text", `String "hello");
        ("items", `List [ `String "a"; `String "b"; `String "c" ]);
      ])
    (Ok "Length: 5, Items: 3");

  make_compiler_test "helper function: concat: OK case"
    "{{concat part1 part2 part3}}"
    (`Assoc
      [
        ("part1", `String "Hello, ");
        ("part2", `String "World");
        ("part3", `String "!");
      ])
    (Ok "Hello, World!");

  make_compiler_test "helper function: eq: OK case"
    "{{#if (eq val1 val2)}}Equal{{else}}Not Equal{{/if}}"
    (`Assoc [ ("val1", `Int 10); ("val2", `Int 10) ])
    (Ok "Equal");

  make_compiler_test "helper function: not: OK case"
    "{{#if (not condition)}}False{{else}}True{{/if}}"
    (`Assoc [ ("condition", `Bool false) ])
    (Ok "False");

  make_compiler_test "preserves indentation in partial"
    ~get_partial:(fun name ->
      match name with
      | "item-list" -> Some "{{#each this}}- {{this}}\n{{/each}}"
      | _ -> None)
    "Items:\n  {{> item-list items}}"
    (`Assoc [ ("items", `List [ `String "A"; `String "B" ]) ])
    (Ok "Items:\n  - A\n  - B\n");

  make_compiler_test "preserves indentation when partials are chained"
    ~get_partial:(fun name ->
      match name with
      | "item-list" -> Some "{{#each this}}{{> item-view}}{{/each}}"
      | "item-view" -> Some "- Item {{@index}}: {{this}}\n"
      | _ -> None)
    "List:\n  {{> item-list items}}"
    (`Assoc [ ("items", `List [ `String "A"; `String "B" ]) ])
    (Ok "List:\n  - Item 0: A\n  - Item 1: B\n");

  make_compiler_test "removes whitespace on standalone template line"
    {|
    {{#each foo}}
      - {{ this }}
    {{/each}}
|}
    (`Assoc [ ("foo", `List [ `String "a"; `String "b" ]) ])
    (Ok "\n      - a\n      - b\n");

  make_compiler_test "standalone partials do not leave empty line behind"
    ~get_partial:(fun name ->
      match name with
      | "item-list" -> Some "{{#each this}}- {{this}}\n{{/each}}"
      | _ -> None)
    {|
    Items:
    {{> item-list items}}
    {{> item-list items}}
    {{> item-list items}}
    End of list.
|}
    (`Assoc [])
    (Ok "\n    Items:\n    End of list.\n");

  make_compiler_test "standalone block helpers do not leave empty line behind"
    {|
    {{#if condition}}
      Condition is true.
    {{/if}}
    After condition.
|}
    (`Assoc [ ("condition", `Bool true) ])
    (Ok "\n      Condition is true.\n    After condition.\n");

  make_compiler_test "nested helper blocks don't consume lines"
    ~get_partial:(fun name ->
      match name with "item-view" -> Some "- {{this}}\n" | _ -> None)
    {|
    {{#if condition}}
      {{#each items}}
      {{> item-view this }}
      {{/each}}
    {{/if}}
|}
    (`Assoc
      [
        ("condition", `Bool true); ("items", `List [ `String "a"; `String "b" ]);
      ])
    (Ok "\n      - a\n      - b\n");

  make_compiler_test "standalone partials are handled correctly"
    ~get_partial:(fun name ->
      match name with "pat" -> Some "PARTIAL" | _ -> None)
    {|
1
{{> pat }}
{{> pat }}
2
|}
    (`Assoc [])
    (Ok "\n1\nPARTIALPARTIAL2\n");

  make_compiler_test "standalone block at the beginning does not leave empty line"
    {|{{#if condition}}
Condition is true.
{{/if}}|}
    (`Assoc [ ("condition", `Bool true) ])
    (Ok "Condition is true.\n");

  make_compiler_test "standalone partial with section leaves no whitespace"
    ~get_partial:(fun name ->
      match name with
      | "header" -> Some {|{{#basics}}
<h1>{{name}}</h1>
{{/basics}}
|}
      | _ -> None)
    {|
<body>
  <main>
    {{> header}}
  </main>
</body>
|}
    (`Assoc [ ("basics", `Assoc [ ("name", `String "Title") ]) ])
    (Ok {|
<body>
  <main>
    <h1>Title</h1>
  </main>
</body>
|});
]

let parser_tests = [
  make_parser_test "parses simple template substitution correctly"
    "Hello {{name}}!"
    (Ok
      [
        `Raw "Hello";
        `Whitespace " ";
        `Escaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `Raw "!";
      ]);

  make_parser_test "parses simple comments correctly"
    "Hello, {{! this is a comment }} world"
    (Ok
      [
        `Raw "Hello,"; `Whitespace " "; `Comment; `Whitespace " "; `Raw "world";
      ]);

  make_parser_test "errors when parsing an unclosed template"
    "Hello, {{name world"
    (mk_test_err "unexpected EOF" 1 19);

  make_parser_test "errors when unexpected token in template"
    "Hello, {{name !"
    (mk_test_err "unexpected token: !" 1 15);

  make_parser_test "parses nested paths correctly"
    "{{a.b.c.[0].[\"hello\"].d}}"
    (Ok
      [
        `Escaped
          (`IdentPath
             [
               `Ident "a";
               `Ident "b";
               `Ident "c";
               `Index (`Int 0);
               `Index (`String "\"hello\"");
               `Ident "d";
             ]);
      ]);

  make_parser_test "parses function application correctly"
    "{{func arg1 arg2.[0] nested.func2.[\"hello\"]}}"
    (Ok
      [
        `Escaped
          (`App
             ( "func",
               [
                 `IdentPath [ `Ident "arg1" ];
                 `IdentPath [ `Ident "arg2"; `Index (`Int 0) ];
                 `IdentPath
                   [
                     `Ident "nested";
                     `Ident "func2";
                     `Index (`String "\"hello\"");
                   ];
               ] ));
      ]);

  make_parser_test "parses whitespace control correctly"
    "Hello, {{~name~}}!"
    (Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `WhitespaceControl;
        `Escaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `WhitespaceControl;
        `Raw "!";
      ]);

  make_parser_test "parses unescaped templates correctly"
    "Hello, {{{name}}}!"
    (Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `Unescaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `Raw "!";
      ]);

  make_parser_test "parsed unescaped templates with whitespace control correctly"
    "Hello, {{~{name}}}!"
    (Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `WhitespaceControl;
        `Unescaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `Raw "!";
      ]);

  make_parser_test "parses partials correctly"
    "Hello, {{> partialName context}}"
    (Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `Partial
          {
            name = "partialName";
            context = Some (`IdentPath [ `Ident "context" ]);
            hash_args = [];
          };
      ]);

  make_parser_test "parses partials with whitespace control correctly"
    "Hello, {{~> partialName ~}}"
    (Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `WhitespaceControl;
        `Partial { name = "partialName"; context = None; hash_args = [] };
        `WhitespaceControl;
      ]);

  make_parser_test "parses template with escaped chars"
    "hello \\{{world}}"
    (Ok [ `Raw "hello"; `Whitespace " "; `Raw "{"; `Raw "{"; `Raw "world}}" ]);

  make_parser_test "parses template with substitution block"
    "hello, {{world}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ]);

  make_parser_test "parses template with substitution block and strip before"
    "hello, {{~world}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ]);

  make_parser_test "parses template with substitution block and strip before + after"
    "hello, {{~ world ~}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
         `WhitespaceControl;
       ]);

  make_parser_test "parses template substitution block with nested ident"
    "hello, {{~a.b.c}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ]);

  make_parser_test "parses parenthesis expressions"
    "hello, {{~(fncall a.b.c)}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App
              ("fncall", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ]);

  make_parser_test "parses substitution with indexing"
    "hello, {{~a.['world'] }}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Index (`String "'world'") ]);
       ]);

  make_parser_test "parses nested fn calls and primitive literals"
    "hello, {{~fncall a.b.c (fn2 1 2.3) }}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App
              ( "fncall",
                [
                  `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ];
                  `App ("fn2", [ `Literal (`Int 1); `Literal (`Float 2.3) ]);
                ] ));
       ]);

  make_parser_test "parses comments"
    "hello, {{! this is a comment }} world"
    (Ok
       [
         `Raw "hello,"; `Whitespace " "; `Comment; `Whitespace " "; `Raw "world";
       ]);

  make_parser_test "parses comments containing mustache syntax"
    {|
    hello, {{!--
      {{# this is a
        multiline comment }}
    --}} world
  |}
    (Ok
       [
         `Whitespace "\n    ";
         `Raw "hello,";
         `Whitespace " ";
         `Comment;
         `Whitespace " ";
         `Raw "world";
         `Whitespace "\n  ";
       ]);

  make_parser_test "parses unescaped substitution"
    "hello, {{~{ a.b.c }}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ]);

  Alcotest.test_case "lexing unclosed '{{{' block throws error" `Quick (fun () ->
    let buf = Lexing.from_string "hello, {{{ ~a.b.c }}" in
    let result = parse buf in
    match result with Ok _ -> Alcotest.fail "Expected error" | Error _ -> ());

  make_parser_test "parses fn application without parenthesis"
    "hello, {{~fn a.b.c}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App ("fn", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ]);

  make_parser_test "parses literal-looking values correctly"
    "hello, {{~true}} and {{~false}} and substitute {{true_looking}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("true", []); `IdentPath [ `Ident "true" ] ]);
         `Whitespace " ";
         `Raw "and";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("false", []); `IdentPath [ `Ident "false" ] ]);
         `Whitespace " ";
         `Raw "and";
         `Whitespace " ";
         `Raw "substitute";
         `Whitespace " ";
         `Escaped
           (`WhateverMakesSense
              [
                `App ("true_looking", []); `IdentPath [ `Ident "true_looking" ];
              ]);
       ]);

  make_parser_test "parses StripAfter in unescaped substitution"
    "hello, {{~{ a.b.c }~}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
         `WhitespaceControl;
       ]);

  make_parser_test "parses else block"
    "hello, {{#if a}}yes{{else}}no{{/if}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `Block
           {
             expr = `App ("if", [ `IdentPath [ `Ident "a" ] ]);
             kind = Section;
             content = [ `Raw "yes" ];
             else_content = [ `Raw "no" ];
           };
       ]);

  make_parser_test "parses else looking things as something else"
    "hello, {{~else1}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("else1", []); `IdentPath [ `Ident "else1" ] ]);
       ]);

  Alcotest.test_case "parses else block without open block as Error" `Quick (fun () ->
    let buf = Lexing.from_string "{{#if a}}ok{{/if}} {{else}} no business here" in
    let result = parse buf in
    match result with Ok _ -> Alcotest.fail "Expected error" | Error _ -> ());

  Alcotest.test_case "parses mismatching close block as Error" `Quick (fun () ->
    let buf = Lexing.from_string "{{#if a}}ok{{/each}}" in
    let result = parse buf in
    match result with Ok _ -> Alcotest.fail "Expected error" | Error _ -> ());

  make_parser_test "parses mustache-style open & close blocks"
    "{{#a}}{{ . }}{{/a}}"
    (Ok
       [
         `Block
           {
             expr =
               `WhateverMakesSense [ `App ("a", []); `IdentPath [ `Ident "a" ] ];
             kind = Section;
             content = [ `Escaped (`IdentPath [ `DotPath `OneDot ]) ];
             else_content = [];
           };
       ]);

  make_parser_test "parses mustache style open/close with dot-index path"
    "{{#resume.basics}}{{name}}{{/resume.basics}}"
    (Ok
       [
         `Block
           {
             expr = `IdentPath [ `Ident "resume"; `Ident "basics" ];
             kind = Section;
             content =
               [
                 `Escaped
                   (`WhateverMakesSense
                      [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
               ];
             else_content = [];
           };
       ]);

  make_parser_test "parses inverted blocks"
    "{{^a}}{{ . }}{{/a}}"
    (Ok
       [
         `Block
           {
             expr =
               `WhateverMakesSense [ `App ("a", []); `IdentPath [ `Ident "a" ] ];
             kind = InvertedSection;
             content = [];
             else_content = [ `Escaped (`IdentPath [ `DotPath `OneDot ]) ];
           };
       ]);

  make_parser_test "parses example 1 from handlebarsjs docs"
    {|
{{#with person}}
{{firstname}} {{lastname}}
{{/with}}
|}
    (Ok
       [
         `Whitespace "\n";
         `Block
           {
             expr = `App ("with", [ `IdentPath [ `Ident "person" ] ]);
             kind = Section;
             content =
               [
                 `Whitespace "\n";
                 `Escaped
                   (`WhateverMakesSense
                      [
                        `App ("firstname", []);
                        `IdentPath [ `Ident "firstname" ];
                      ]);
                 `Whitespace " ";
                 `Escaped
                   (`WhateverMakesSense
                      [
                        `App ("lastname", []); `IdentPath [ `Ident "lastname" ];
                      ]);
                 `Whitespace "\n";
               ];
             else_content = [];
           };
         `Whitespace "\n";
       ]);

  make_parser_test "parses literal string as key for substitution"
    {| {{#with obj}}{{ "key" }}{{/with}} |}
    (Ok
       [
         `Whitespace " ";
         `Block
           {
             kind = Section;
             expr = `App ("with", [ `IdentPath [ `Ident "obj" ] ]);
             content =
               [
                 `Escaped
                   (`WhateverMakesSense
                      [ `App ("key", []); `IdentPath [ `Ident "key" ] ]);
               ];
             else_content = [];
           };
         `Whitespace " ";
       ]);

  make_parser_test "parses literal int as index for substitution"
    {| {{#with arr}}{{ 0 }}{{/with}} |}
    (Ok
       [
         `Whitespace " ";
         `Block
           {
             kind = Section;
             expr = `App ("with", [ `IdentPath [ `Ident "arr" ] ]);
             content = [ `Escaped (`IdentPath [ `Index (`Int 0) ]) ];
             else_content = [];
           };
         `Whitespace " ";
       ]);

  make_parser_test "parses multiple index arguments"
    {| {{#with arr}}{{concat [0] [1] "two"}}{{/with}} |}
    (Ok
       [
         `Whitespace " ";
         `Block
           {
             kind = Section;
             expr = `App ("with", [ `IdentPath [ `Ident "arr" ] ]);
             content =
               [
                 `Escaped
                   (`App
                      ( "concat",
                        [
                          `IdentPath [ `Index (`Int 0) ];
                          `IdentPath [ `Index (`Int 1) ];
                          `Literal (`String "two");
                        ] ));
               ];
             else_content = [];
           };
         `Whitespace " ";
       ]);

  make_parser_test "parses basic partial syntax"
    "Hello {{> greeting}}!"
    (Ok
       [
         `Raw "Hello";
         `Whitespace " ";
         `Partial { name = "greeting"; context = None; hash_args = [] };
         `Raw "!";
       ]);

  make_parser_test "parses partial with whitespace control"
    "{{~> greeting ~}}"
    (Ok
       [
         `WhitespaceControl;
         `Partial { name = "greeting"; context = None; hash_args = [] };
         `WhitespaceControl;
       ]);

  make_parser_test "parses partial with context"
    "{{> greeting user}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = Some (`IdentPath [ `Ident "user" ]);
             hash_args = [];
           };
       ]);

  make_parser_test "parses parital name with hyphen"
    "{{> my-partial}}"
    (Ok [ `Partial { name = "my-partial"; context = None; hash_args = [] } ]);

  make_parser_test "parses partial with single hash argument"
    "{{> greeting name=\"World\"}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = None;
             hash_args = [ ("name", `Literal (`String "World")) ];
           };
       ]);

  make_parser_test "parses partial with multiple hash arguments"
    "{{> user-card name=\"Alice\" age=25}}"
    (Ok
       [
         `Partial
           {
             name = "user-card";
             context = None;
             hash_args =
               [
                 ("name", `Literal (`String "Alice"));
                 ("age", `Literal (`Int 25));
               ];
           };
       ]);

  make_parser_test "parses partial with context and hash arguments"
    "{{> greeting user name=\"Hello\"}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = Some (`IdentPath [ `Ident "user" ]);
             hash_args = [ ("name", `Literal (`String "Hello")) ];
           };
       ]);

  make_parser_test "parses partial with variable as hash argument value"
    "{{> greeting msg=message}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = None;
             hash_args = [ ("msg", `IdentPath [ `Ident "message" ]) ];
           };
       ]);

  make_parser_test "parses if section"
    "{{#if condition}}Yes{{else}}No{{/if}}"
    (Ok
       [
         `Block
           {
             kind = Section;
             expr = `App ("if", [ `IdentPath [ `Ident "condition" ] ]);
             content = [ `Raw "Yes" ];
             else_content = [ `Raw "No" ];
           };
       ]);
]

let () =
  let open Alcotest in
  run "Handlebars ML Tests" [
    ("Compiler", compiler_tests);
    ("Parser", parser_tests);
  ]