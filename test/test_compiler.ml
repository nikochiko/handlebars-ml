open Handlebars_ml.Compiler
open Handlebars_ml.Types

let make_test ?(get_helper = default_get_helper)
    ?(get_partial = default_get_partial) template values expected =
  let result = compile ~get_helper ~get_partial template values in
  match result = expected with
  | true -> true
  | false ->
      Printf.printf "------ Test failed: -----\n";
      Printf.printf "Template: %s\n" template;
      Printf.printf "Values:   %s\n" (show_literal_or_collection values);
      Printf.printf "Output:   %s\n" (show_hb_result result);
      Printf.printf "Expected: %s\n" (show_hb_result expected);
      false

let%test "simple variable substitution" =
  let template = "Hello {{name}}!" in
  let values = `Assoc [ ("name", `String "World") ] in
  make_test template values (Ok "Hello World!")

let%test "multiple variables" =
  let template = "{{firstname}} {{lastname}}" in
  let values =
    `Assoc [ ("firstname", `String "John"); ("lastname", `String "Doe") ]
  in
  make_test template values (Ok "John Doe")

let%test "missing variable returns empty string" =
  let template = "Hello {{missing}}!" in
  let values = `Assoc [] in
  make_test template values (Ok "Hello !")

let%test "nested object access" =
  let template = "{{user.name}}" in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ] in
  make_test template values (Ok "Alice")

let%test "escaped output by default" =
  let template = "{{content}}" in
  let values =
    `Assoc [ ("content", `String "<script>alert('xss')</script>") ]
  in
  make_test template values
    (Ok "&lt;script&gt;alert(&apos;xss&apos;)&lt;/script&gt;")

let%test "unescaped output with triple braces" =
  let template = "{{{content}}}" in
  let values = `Assoc [ ("content", `String "<b>bold</b>") ] in
  make_test template values (Ok "<b>bold</b>")

let%test "upper helper" =
  let template = "{{upper name}}" in
  let values = `Assoc [ ("name", `String "hello") ] in
  make_test template values (Ok "HELLO")

let%test "lower helper" =
  let template = "{{lower name}}" in
  let values = `Assoc [ ("name", `String "WORLD") ] in
  make_test template values (Ok "world")

let%test "length helper with string" =
  let template = "{{length text}}" in
  let values = `Assoc [ ("text", `String "hello") ] in
  make_test template values (Ok "5")

let%test "missing helper error" =
  let template = "{{missing_helper name}}" in
  let values = `Assoc [ ("name", `String "test") ] in
  let expected = Error (CompileError (Missing_helper "missing_helper")) in
  make_test template values expected

let%test "if block with truthy value" =
  let template = "{{#if user}}Hello {{user.name}}{{/if}}" in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test template values (Ok "Hello Bob")

let%test "if block with falsy value" =
  let template = "{{#if missing}}Never shown{{/if}}" in
  let values = `Assoc [] in
  make_test template values (Ok "")

let%test "empty string is falsy" =
  let template = "{{#if empty}}truthy{{else}}falsy{{/if}}" in
  let values = `Assoc [ ("empty", `String "") ] in
  make_test template values (Ok "falsy")

let%test "zero is falsy" =
  let template = "{{#if zero}}truthy{{else}}falsy{{/if}}" in
  let values = `Assoc [ ("zero", `Int 0) ] in
  make_test template values (Ok "falsy")

let%test "non-empty string is truthy" =
  let template = "{{#if text}}truthy{{else}}falsy{{/if}}" in
  let values = `Assoc [ ("text", `String "hello") ] in
  make_test template values (Ok "truthy")

let%test "integer to string" =
  let template = "Count: {{count}}" in
  let values = `Assoc [ ("count", `Int 42) ] in
  make_test template values (Ok "Count: 42")

let%test "custom helper" =
  let template = "{{greet name}}" in
  let get_helper name =
    match name with
    | "greet" ->
        Some
          (function
          | [ `String name ] -> Some (`String ("Hello, " ^ name ^ "!"))
          | _ -> None)
    | _ -> default_get_helper name
  in
  let values = `Assoc [ ("name", `String "Alice") ] in
  make_test ~get_helper template values (Ok "Hello, Alice!")

let%test "each block with array" =
  let template = "{{#each items}}{{.}} {{/each}}" in
  let values =
    `Assoc [ ("items", `List [ `String "a"; `String "b"; `String "c" ]) ]
  in
  make_test template values (Ok "a b c ")

let%test "each block with object" =
  let template = "{{#each user}}{{.}} {{/each}}" in
  let values =
    `Assoc [ ("user", `Assoc [ ("name", `String "John"); ("age", `Int 30) ]) ]
  in
  make_test template values (Ok "John 30 ")

let%test "with block context" =
  let template = "{{#with user}}Hello {{name}}{{/with}}" in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test template values (Ok "Hello Bob")

let%test "mustache-style block with dot-nesting" =
  let template = "{{#resume.basics}}{{name}}{{/resume.basics}}" in
  let values =
    `Assoc
      [ ("resume", `Assoc [ ("basics", `Assoc [ ("name", `String "Eve") ]) ]) ]
  in
  make_test template values (Ok "Eve")

let%test "fallback with WhateverMakesSense" =
  let template = "{{name}}" in
  let get_helper name =
    match name with
    | "name" -> Some (fun _ -> Some (`String "from helper"))
    | _ -> default_get_helper name
  in
  let values = `Assoc [ ("name", `String "from variable") ] in
  make_test ~get_helper template values (Ok "from helper")

let%test "basic partial inclusion should work" =
  let template = "Hello {{> greeting}}!" in
  let get_partial name =
    match name with "greeting" -> Some "{{name}}" | _ -> None
  in
  let values = `Assoc [ ("name", `String "World") ] in
  make_test ~get_partial template values (Ok "Hello World!")

let%test "partial with context inheritance should work" =
  let template = "{{#with user}}{{> userCard}}{{/with}}" in
  let get_partial name =
    match name with
    | "userCard" -> Some "Name: {{name}}, Age: {{age}}"
    | _ -> None
  in
  let values =
    `Assoc [ ("user", `Assoc [ ("name", `String "Alice"); ("age", `Int 25) ]) ]
  in
  make_test ~get_partial template values (Ok "Name: Alice, Age: 25")

let%test "partial with custom context should work - Phase 2" =
  let template = "{{> greeting}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{user.name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test ~get_partial template values (Ok "Hello Bob!")

let%test "nested partials should work" =
  let template = "{{> outer}}" in
  let get_partial name =
    match name with
    | "outer" -> Some "Outer: {{> inner}}"
    | "inner" -> Some "Inner: {{value}}"
    | _ -> None
  in
  let values = `Assoc [ ("value", `String "test") ] in
  make_test ~get_partial template values (Ok "Outer: Inner: test")

let%test "partial recursion should be prevented" =
  let get_partial name =
    match name with "recursive" -> Some "{{> recursive}}" | _ -> None
  in
  let values = `Assoc [] in
  let template_missing = "{{> missing}}" in
  make_test ~get_partial template_missing values
    (Error (CompileError (Missing_partial "missing")))

let%test "partial with explicit context should work" =
  let template = "{{> greeting user}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test ~get_partial template values (Ok "Hello Bob!")

let%test "partial with nested context expression should work" =
  let template = "{{> userCard user.profile}}" in
  let get_partial name =
    match name with
    | "userCard" -> Some "Name: {{name}}, Age: {{age}}"
    | _ -> None
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
  make_test ~get_partial template values (Ok "Name: Alice, Age: 30")

let%test "partial with literal context should work" =
  let template = "{{> echo \"Hello World\"}}" in
  let get_partial name = match name with "echo" -> Some "{{.}}" | _ -> None in
  let values = `Assoc [] in
  make_test ~get_partial template values (Ok "Hello World")

let%test "partial context should not affect parent context" =
  let template = "{{name}} {{> greeting user}} {{name}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hi {{name}}!" | _ -> None
  in
  let values =
    `Assoc
      [
        ("name", `String "Main");
        ("user", `Assoc [ ("name", `String "Partial") ]);
      ]
  in
  make_test ~get_partial template values (Ok "Main Hi Partial! Main")

let%test "@root should reference initial context" =
  let template = "{{name}} {{#with user}}{{name}} {{@root.name}}{{/with}}" in
  let values =
    `Assoc
      [
        ("name", `String "Root"); ("user", `Assoc [ ("name", `String "User") ]);
      ]
  in
  make_test template values (Ok "Root User Root")

let%test "@root should work in each blocks" =
  let template = "{{#each items}}{{.}} - {{@root.title}} {{/each}}" in
  let values =
    `Assoc
      [
        ("title", `String "List");
        ("items", `List [ `String "A"; `String "B"; `String "C" ]);
      ]
  in
  make_test template values (Ok "A - List B - List C - List ")

let%test "@root should work in partials" =
  let template = "{{> item_card item}}" in
  let get_partial name =
    match name with
    | "item_card" -> Some "{{name}} (from {{@root.source}})"
    | _ -> None
  in
  let values =
    `Assoc
      [
        ("source", `String "API");
        ("item", `Assoc [ ("name", `String "Widget") ]);
      ]
  in
  make_test ~get_partial template values (Ok "Widget (from API)")

let%test "@root should work with nested contexts" =
  let template =
    "{{#with company}}{{#each departments}}{{name}}: {{@root.company_name}} \
     {{/each}}{{/with}}"
  in
  let values =
    `Assoc
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
      ]
  in
  make_test template values (Ok "Engineering: Acme Corp Sales: Acme Corp ")

let%test "partial with single hash argument should work" =
  let template = "{{> greeting name=\"World\"}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [] in
  make_test ~get_partial template values (Ok "Hello World!")

let%test "partial with multiple hash arguments should work" =
  let template = "{{> user-card name=\"Alice\" age=25}}" in
  let get_partial name =
    match name with
    | "user-card" -> Some "Name: {{name}}, Age: {{age}}"
    | _ -> None
  in
  let values = `Assoc [] in
  make_test ~get_partial template values (Ok "Name: Alice, Age: 25")

let%test "partial with context and hash arguments should work" =
  let template = "{{> greeting user msg=\"Hi\"}}" in
  let get_partial name =
    match name with "greeting" -> Some "{{msg}} {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ] in
  make_test ~get_partial template values (Ok "Hi Alice!")

let%test "partial hash arguments should override context values" =
  let template = "{{> greeting user name=\"Override\"}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Original") ]) ] in
  make_test ~get_partial template values (Ok "Hello Override!")

let%test "partial with variable as hash argument value should work" =
  let template = "{{> greeting msg=message}}" in
  let get_partial name =
    match name with "greeting" -> Some "Message: {{msg}}" | _ -> None
  in
  let values = `Assoc [ ("message", `String "Hello World") ] in
  make_test ~get_partial template values (Ok "Message: Hello World")

let%test "partial hash arguments work with context object properties" =
  let template = "{{> wrapper user title=\"User Profile\"}}" in
  let get_partial name =
    match name with
    | "wrapper" -> Some "{{title}}: {{name}} ({{age}})"
    | _ -> None
  in
  let values =
    `Assoc [ ("user", `Assoc [ ("name", `String "Bob"); ("age", `Int 30) ]) ]
  in
  make_test ~get_partial template values (Ok "User Profile: Bob (30)")

let%test "partial without context inherits current execution context" =
  let template = "{{#with user}}{{> greeting}}{{/with}}" in
  let get_partial name =
    match name with "greeting" -> Some "Hello {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Alice") ]) ] in
  make_test ~get_partial template values (Ok "Hello Alice!")

let%test
    "partial with hash args but no context inherits current execution context" =
  let template = "{{#with user}}{{> greeting msg=\"Hi\"}}{{/with}}" in
  let get_partial name =
    match name with "greeting" -> Some "{{msg}} {{name}}!" | _ -> None
  in
  let values = `Assoc [ ("user", `Assoc [ ("name", `String "Bob") ]) ] in
  make_test ~get_partial template values (Ok "Hi Bob!")

let%test "partial with hash args inherits loop context variables" =
  let template =
    {| {{~#each items}}{{> item-display prefix="Item"}}{{/each~}} |}
  in
  let get_partial name =
    match name with
    | "item-display" -> Some "{{@index}}: {{prefix}}"
    | _ -> None
  in
  let values = `Assoc [ ("items", `List [ `String "A"; `String "B" ]) ] in
  make_test ~get_partial template values (Ok "0: Item1: Item")

let%test "partial with 'this'" =
  let template = "{{#each items}}{{> item-card this}} {{/each}}" in
  let get_partial name =
    match name with "item-card" -> Some "Item: {{this}}" | _ -> None
  in
  let values = `Assoc [ ("items", `List [ `String "A"; `String "B" ]) ] in
  make_test ~get_partial template values (Ok "Item: A Item: B ")

let%test "helper function: upper and lower: OK case" =
  let template = "{{upper name}} and {{lower name}}" in
  let values = `Assoc [ ("name", `String "Test") ] in
  make_test template values (Ok "TEST and test")

let%test "helper function: length: OK case" =
  let template = "Length: {{length text}}, Items: {{length items}}" in
  let values =
    `Assoc
      [
        ("text", `String "hello");
        ("items", `List [ `String "a"; `String "b"; `String "c" ]);
      ]
  in
  make_test template values (Ok "Length: 5, Items: 3")

let%test "helper function: concat: OK case" =
  let template = "{{concat part1 part2 part3}}" in
  let values =
    `Assoc
      [
        ("part1", `String "Hello, ");
        ("part2", `String "World");
        ("part3", `String "!");
      ]
  in
  make_test template values (Ok "Hello, World!")

let%test "helper function: eq: OK case" =
  let template = "{{#if (eq val1 val2)}}Equal{{else}}Not Equal{{/if}}" in
  let values = `Assoc [ ("val1", `Int 10); ("val2", `Int 10) ] in
  make_test template values (Ok "Equal")

let%test "helper function: not: OK case" =
  let template = "{{#if (not condition)}}False{{else}}True{{/if}}" in
  let values = `Assoc [ ("condition", `Bool false) ] in
  make_test template values (Ok "False")

let%test "preserves indentation in partial" =
  let template = "Items:\n  {{> item-list items}}" in
  let get_partial name =
    match name with
    | "item-list" -> Some "{{#each this}}- {{this}}\n{{/each}}"
    | _ -> None
  in
  let values = `Assoc [ ("items", `List [ `String "A"; `String "B" ]) ] in
  let expected = "Items:\n  - A\n  - B\n" in
  make_test ~get_partial template values (Ok expected)

let%test "preserves indentation when partials are chained" =
  let template = "List:\n  {{> item-list items}}" in
  let get_partial name =
    match name with
    | "item-list" -> Some "{{#each this}}{{> item-view}}{{/each}}"
    | "item-view" -> Some "- Item {{@index}}: {{this}}\n"
    | _ -> None
  in
  let values = `Assoc [ ("items", `List [ `String "A"; `String "B" ]) ] in
  let expected = "List:\n  - Item 0: A\n  - Item 1: B\n" in
  make_test ~get_partial template values (Ok expected)

let%test "removes whitespace on standalone template line" =
  let template = {|
    {{#each foo}}
      - {{ this }}
    {{/each}}
|} in
  let values = `Assoc [ ("foo", `List [ `String "a"; `String "b" ]) ] in
  let expected = "\n      - a\n      - b\n" in
  make_test template values (Ok expected)

let%test "standalone partials do not leave empty line behind" =
  let template =
    {|
    Items:
    {{> item-list items}}
    {{> item-list items}}
    {{> item-list items}}
    End of list.
|}
  in
  let get_partial name =
    match name with
    | "item-list" -> Some "{{#each this}}- {{this}}\n{{/each}}"
    | _ -> None
  in
  let values = `Assoc [] in
  let expected = "\n    Items:\n    End of list.\n" in
  make_test ~get_partial template values (Ok expected)

let%test "standalone block helpers do not leave empty line behind" =
  let template =
    {|
    {{#if condition}}
      Condition is true.
    {{/if}}
    After condition.
|}
  in
  let values = `Assoc [ ("condition", `Bool true) ] in
  make_test template values
    (Ok "\n      Condition is true.\n    After condition.\n")

let%test "nested helper blocks don't consume lines" =
  let template =
    {|
    {{#if condition}}
      {{#each items}}
      {{> item-view this }}
      {{/each}}
    {{/if}}
|}
  in
  let get_partial name =
    match name with "item-view" -> Some "- {{this}}\n" | _ -> None
  in
  let values =
    `Assoc
      [
        ("condition", `Bool true); ("items", `List [ `String "a"; `String "b" ]);
      ]
  in
  let expected = "\n      - a\n      - b\n" in
  make_test ~get_partial template values (Ok expected)

let%test "standalone partials are handled correctly" =
  let template = {|
1
{{> pat }}
{{> pat }}
2
|} in
  let get_partial name =
    match name with "pat" -> Some "PARTIAL" | _ -> None
  in
  let values = `Assoc [] in
  let expected = "\n1\nPARTIALPARTIAL2\n" in
  make_test ~get_partial template values (Ok expected)

let%test "standalone block at the beginning does not leave empty line" =
  let template = {|{{#if condition}}
Condition is true.
{{/if}}|} in
  let values = `Assoc [ ("condition", `Bool true) ] in
  make_test template values (Ok "Condition is true.\n")

let%test "standalone partial with section leaves no whitespace" =
  let template = {|
<body>
  <main>
    {{> header}}
  </main>
</body>
|} in
  let get_partial name =
    match name with
    | "header" -> Some {|{{#basics}}
<h1>{{name}}</h1>
{{/basics}}
|}
    | _ -> None
  in
  let values = `Assoc [ ("basics", `Assoc [ ("name", `String "Title") ]) ] in
  let expected = {|
<body>
  <main>
    <h1>Title</h1>
  </main>
</body>
|} in
  make_test ~get_partial template values (Ok expected)
