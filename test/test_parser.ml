open Handlebars_ml.Parser
open Handlebars_ml.Types

let make_test input expected =
  let buf = Lexing.from_string input in
  let result = parse buf in
  match equal_parse_result result expected with
  | true -> true
  | false ->
      Printf.printf "------ Test failed: -----\n";
      Printf.printf "Input:    \t%s\n" input;
      Printf.printf "Tokens:   \t%s\n" (show_parse_result result);
      Printf.printf "Expected: \t%s\n" (show_parse_result expected);
      false

let mk_test_err msg lnum cnum =
  Error
    {
      msg;
      pos =
        { Lexing.pos_fname = ""; pos_lnum = lnum; pos_bol = 0; pos_cnum = cnum };
      buf = Lexing.from_string "";
    }

let%test "parses simple template substitution correctly" =
  let input = "Hello {{name}}!" in
  let expected =
    Ok
      [
        `Raw "Hello";
        `Whitespace " ";
        `Escaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `Raw "!";
      ]
  in
  make_test input expected

let%test "parses simple comments correctly" =
  let input = "Hello, {{! this is a comment }} world" in
  let expected =
    Ok
      [
        `Raw "Hello,"; `Whitespace " "; `Comment; `Whitespace " "; `Raw "world";
      ]
  in
  make_test input expected

let%test "errors when parsing an unclosed template" =
  let input = "Hello, {{name world" in
  let expected = mk_test_err "unexpected EOF" 1 19 in
  make_test input expected

let%test "errors when unexpected token in template" =
  let input = "Hello, {{name !" in
  let expected = mk_test_err "unexpected token: !" 1 15 in
  make_test input expected

let%test "parses nested paths correctly" =
  let input = "{{a.b.c.[0].[\"hello\"].d}}" in
  let expected =
    Ok
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
      ]
  in
  make_test input expected

let%test "parses function application correctly" =
  let input = "{{func arg1 arg2.[0] nested.func2.[\"hello\"]}}" in
  let expected =
    Ok
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
      ]
  in
  make_test input expected

let%test "parses whitespace control correctly" =
  let input = "Hello, {{~name~}}!" in
  let expected =
    Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `WhitespaceControl;
        `Escaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `WhitespaceControl;
        `Raw "!";
      ]
  in
  make_test input expected

let%test "parses unescaped templates correctly" =
  let input = "Hello, {{{name}}}!" in
  let expected =
    Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `Unescaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `Raw "!";
      ]
  in
  make_test input expected

let%test "parsed unescaped templates with whitespace control correctly" =
  let input = "Hello, {{~{name}}}!" in
  let expected =
    Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `WhitespaceControl;
        `Unescaped
          (`WhateverMakesSense
             [ `App ("name", []); `IdentPath [ `Ident "name" ] ]);
        `Raw "!";
      ]
  in
  make_test input expected

let%test "parses partials correctly" =
  let input = "Hello, {{> partialName context}}" in
  let expected =
    Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `Partial
          {
            name = "partialName";
            context = Some (`IdentPath [ `Ident "context" ]);
            hash_args = [];
          };
      ]
  in
  make_test input expected

let%test "parses partials with whitespace control correctly" =
  let input = "Hello, {{~> partialName ~}}" in
  let expected =
    Ok
      [
        `Raw "Hello,";
        `Whitespace " ";
        `WhitespaceControl;
        `Partial { name = "partialName"; context = None; hash_args = [] };
        `WhitespaceControl;
      ]
  in
  make_test input expected

let%test "parses template with escaped chars" =
  make_test "hello \\{{world}}"
    (Ok [ `Raw "hello"; `Whitespace " "; `Raw "{"; `Raw "{"; `Raw "world}}" ])

let%test "parses template with substitution block" =
  make_test "hello, {{world}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ])

let%test "parses template with substitution block and strip before" =
  make_test "hello, {{~world}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
       ])

let%test "parses template with substitution block and strip before + after" =
  make_test "hello, {{~ world ~}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("world", []); `IdentPath [ `Ident "world" ] ]);
         `WhitespaceControl;
       ])

let%test "parses template substitution block with nested ident" =
  make_test "hello, {{~a.b.c}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ])

let%test "parses parenthesis expressions" =
  make_test "hello, {{~(fncall a.b.c)}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App
              ("fncall", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ])

let%test "parses substitution with indexing" =
  make_test "hello, {{~a.['world'] }}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped (`IdentPath [ `Ident "a"; `Index (`String "'world'") ]);
       ])

let%test "parses nested fn calls and primitive literals" =
  make_test "hello, {{~fncall a.b.c (fn2 1 2.3) }}"
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
       ])

let%test "parses comments" =
  make_test "hello, {{! this is a comment }} world"
    (Ok
       [
         `Raw "hello,"; `Whitespace " "; `Comment; `Whitespace " "; `Raw "world";
       ])

let%test "parses comments containing mustache syntax" =
  make_test
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
       ])

let%test "parses unescaped substitution" =
  make_test "hello, {{~{ a.b.c }}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
       ])

let%test "lexing unclosed '{{{' block throws error" =
  let buf = Lexing.from_string "hello, {{{ ~a.b.c }}" in
  let result = parse buf in
  match result with Ok _ -> false | Error _ -> true

let%test "parses fn application without parenthesis" =
  make_test "hello, {{~fn a.b.c}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`App ("fn", [ `IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ] ]));
       ])

let%test "parses literal-looking values correctly" =
  make_test "hello, {{~true}} and {{~false}} and substitute {{true_looking}}"
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
       ])

let%test "parses StripAfter in unescaped substitution" =
  make_test "hello, {{~{ a.b.c }~}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Unescaped (`IdentPath [ `Ident "a"; `Ident "b"; `Ident "c" ]);
         `WhitespaceControl;
       ])

let%test "parses else block" =
  make_test "hello, {{#if a}}yes{{else}}no{{/if}}"
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
       ])

let%test "parses else looking things as something else" =
  make_test "hello, {{~else1}}"
    (Ok
       [
         `Raw "hello,";
         `Whitespace " ";
         `WhitespaceControl;
         `Escaped
           (`WhateverMakesSense
              [ `App ("else1", []); `IdentPath [ `Ident "else1" ] ]);
       ])

let%test "parses else block without open block as Error" =
  let buf = Lexing.from_string "{{#if a}}ok{{/if}} {{else}} no business here" in
  let result = parse buf in
  match result with Ok _ -> false | Error _ -> true

let%test "parses mismatching close block as Error" =
  let buf = Lexing.from_string "{{#if a}}ok{{/each}}" in
  let result = parse buf in
  match result with Ok _ -> false | Error _ -> true

let%test "parses mustache-style open & close blocks" =
  make_test "{{#a}}{{ . }}{{/a}}"
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
       ])

let%test "parses mustache style open/close with dot-index path" =
  make_test "{{#resume.basics}}{{name}}{{/resume.basics}}"
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
       ])

let%test "parses inverted blocks" =
  make_test "{{^a}}{{ . }}{{/a}}"
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
       ])

let%test "parses example 1 from handlebarsjs docs" =
  let input = {|
{{#with person}}
{{firstname}} {{lastname}}
{{/with}}
|} in
  make_test input
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
       ])

let%test "parses literal string as key for substitution" =
  make_test {| {{#with obj}}{{ "key" }}{{/with}} |}
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
       ])

let%test "parses literal int as index for substitution" =
  make_test {| {{#with arr}}{{ 0 }}{{/with}} |}
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
       ])

let%test "parses multiple index arguments" =
  make_test {| {{#with arr}}{{concat [0] [1] "two"}}{{/with}} |}
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
       ])

let%test "parses basic partial syntax" =
  make_test "Hello {{> greeting}}!"
    (Ok
       [
         `Raw "Hello";
         `Whitespace " ";
         `Partial { name = "greeting"; context = None; hash_args = [] };
         `Raw "!";
       ])

let%test "parses partial with whitespace control" =
  make_test "{{~> greeting ~}}"
    (Ok
       [
         `WhitespaceControl;
         `Partial { name = "greeting"; context = None; hash_args = [] };
         `WhitespaceControl;
       ])

let%test "parses partial with context" =
  make_test "{{> greeting user}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = Some (`IdentPath [ `Ident "user" ]);
             hash_args = [];
           };
       ])

let%test "parses parital name with hyphen" =
  make_test "{{> my-partial}}"
    (Ok [ `Partial { name = "my-partial"; context = None; hash_args = [] } ])

let%test "parses partial with single hash argument" =
  make_test "{{> greeting name=\"World\"}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = None;
             hash_args = [ ("name", `Literal (`String "World")) ];
           };
       ])

let%test "parses partial with multiple hash arguments" =
  make_test "{{> user-card name=\"Alice\" age=25}}"
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
       ])

let%test "parses partial with context and hash arguments" =
  make_test "{{> greeting user name=\"Hello\"}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = Some (`IdentPath [ `Ident "user" ]);
             hash_args = [ ("name", `Literal (`String "Hello")) ];
           };
       ])

let%test "parses partial with variable as hash argument value" =
  make_test "{{> greeting msg=message}}"
    (Ok
       [
         `Partial
           {
             name = "greeting";
             context = None;
             hash_args = [ ("msg", `IdentPath [ `Ident "message" ]) ];
           };
       ])

let%test "parses if section" =
  make_test "{{#if condition}}Yes{{else}}No{{/if}}"
    (Ok
       [
         `Block
           {
             kind = Section;
             expr = `App ("if", [ `IdentPath [ `Ident "condition" ] ]);
             content = [ `Raw "Yes" ];
             else_content = [ `Raw "No" ];
           };
       ])
