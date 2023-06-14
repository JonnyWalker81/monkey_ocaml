open Base
open Core

type t = { lexer : Lexer.lexer; cur_token : Token.t; peek_token : Token.t }
[@@deriving show]

type precedence =
  | Lowest
  | Equals
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call
[@@deriving enum, show, ord]

let token_prec = function
  | Token.Equal | Token.NotEqual -> Equals
  | Token.LessThan | Token.GreaterThan -> LessGreater
  | Token.Plus | Token.Minus -> Sum
  | Token.Slash | Token.Asterisk -> Product
  | Token.LeftParen -> Call
  | _ -> Lowest

let next_token parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; cur_token = parser.peek_token; peek_token = peek }

let init lexer =
  let parser =
    { lexer; cur_token = Token.Illegal; peek_token = Token.Illegal }
  in
  let parser = next_token parser in
  let parser = next_token parser in
  parser

let chomp_semicolon parser =
  match parser.peek_token with
  | Token.Semicolon -> next_token parser
  | _ -> parser

let expect_peek parser condition =
  match condition parser.peek_token with
  | true -> (next_token parser, true)
  | _ -> (parser, false)

let expect_peek_assign parser =
  expect_peek parser (function Token.Assign -> true | _ -> false)

let expect_peek_right_paren parser =
  expect_peek parser (function Token.RightParen -> true | _ -> false)

let expect_peek_left_paren parser =
  expect_peek parser (function Token.LeftParen -> true | _ -> false)

let expect_peek_left_brace parser =
  expect_peek parser (function Token.LeftBrace -> true | _ -> false)

let expect_peek_right_brace parser =
  expect_peek parser (function Token.RightBrace -> true | _ -> false)

let peek_is parser token =
  if Token.equal parser.peek_token token then true else false

let expect_current_is parser token =
  if Token.equal parser.cur_token token then true else false

let rec skip_while parser condition =
  if condition parser then skip_while (next_token parser) condition else parser

let parse_identifier parser =
  match parser.peek_token with
  | Token.Ident identifier -> (next_token parser, { Ast.identifier })
  | _ -> failwith "expected identifier"

let is_not_semicolon parser =
  match parser.cur_token with Token.Semicolon -> false | _ -> true

let prec_gte x y = compare_precedence x y >= 0

let rec parse parser =
  let rec parse' parser statements =
    match parser.cur_token with
    | Token.Eof -> (parser, List.rev statements)
    | _ -> (
        match parse_statement parser with
        | Ok (parser, s) -> parse' (next_token parser) (s :: statements)
        | Error _ -> failwith "parse' error")
  in
  let _, statements = parse' parser [] in
  Ok (Ast.Program { statements })

and parse_expression parser precedence =
  let parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    let peek_prec = token_prec parser.peek_token in
    if peek_is parser Token.Semicolon || prec_gte precedence peek_prec then
      (parser, left)
    else
      match get_infix_fn parser with
      | Some infix_fn ->
          let parser = next_token parser in
          let parser, left = infix_fn parser left in
          parse_expression' parser left
      | None -> (parser, left)
  in
  parse_expression' parser left

and parse_expression_statement parser =
  let parser, expr = parse_expression parser Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and parse_let parser =
  let parser, name = parse_identifier parser in
  let parser, failed = expect_peek_assign parser in
  if not failed then
    failwith
      (Printf.sprintf "expected token not found: %s, got: %s"
         (Token.show Token.Assign)
         (Token.show parser.cur_token))
  else
    let parser = next_token parser in
    let parser, expr = parse_expression parser Lowest in
    let parser = chomp_semicolon parser in
    Ok (parser, Ast.Let { name; value = expr })

and parse_return parser =
  let parser = next_token parser in
  let parser, expr = parse_expression parser Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Return expr)

and parse_statement parser =
  match parser.cur_token with
  | Token.Let -> parse_let parser
  | Token.Return -> parse_return parser
  | _ -> parse_expression_statement parser

and get_infix_fn parser =
  match parser.peek_token with
  | Token.Plus | Token.Minus | Token.Slash | Token.Asterisk | Token.Equal
  | Token.NotEqual | Token.LessThan | Token.GreaterThan ->
      Some parse_infix_expression
  | Token.LeftParen -> Some parse_call_expression
  | _ -> None

and parse_infix_expression parser left =
  let op = parser.cur_token in
  let parser = next_token parser in
  let prec = token_prec op in
  let parser, right = parse_expression parser prec in
  (parser, Ast.Infix { left; operator = op; right })

and parse_number_expression parser =
  match parser.cur_token with
  | Token.Integer s -> (parser, Ast.Integer (int_of_string s))
  | _ -> failwith "expected a number"

and parse_identifier_expression parser =
  match parser.cur_token with
  | Token.Ident s -> (parser, Ast.Identifier { identifier = s })
  | _ -> failwith "expected a number"

and parse_prefix_operator_expression parser operator =
  let parser = next_token parser in
  let parser, right = parse_expression parser Prefix in
  (parser, Ast.Prefix { operator; right })

and parse_prefix_expression parser =
  match parser.cur_token with
  | Token.Ident _ -> parse_identifier_expression parser
  | Token.Integer _ -> parse_number_expression parser
  | Token.Bang -> parse_prefix_operator_expression parser parser.cur_token
  | Token.Minus -> parse_prefix_operator_expression parser parser.cur_token
  | Token.True | Token.False -> parse_bool_expression parser
  | Token.LeftParen -> parse_grouped_expression parser
  | Token.If -> parse_if_expression parser
  | Token.Function -> parse_function_expression parser
  | tok -> failwith (Printf.sprintf "unhandled token: %s" (Token.show tok))

and parse_function_expression parser =
  let parser, ok = expect_peek_left_paren parser in
  if not ok then failwith "expected left paren"
  else
    let parser, parameters = parse_function_parameters parser in
    let parser, ok = expect_peek_left_brace parser in
    if not ok then failwith "expected left brace"
    else
      let parser, body = parse_block_statement parser in
      (parser, Ast.FunctionLiteral { parameters; body })

and parse_function_parameters parser =
  let rec parse_function_parameters' parser identifiers =
    if peek_is parser Token.RightParen then
      (next_token parser, List.rev identifiers)
    else
      let parser, identifier = parse_identifier parser in
      let identifiers = identifier :: identifiers in
      match parser.peek_token with
      | Token.Comma ->
          let parser = next_token parser in
          let parser, identifer = parse_identifier parser in
          parse_function_parameters' parser (identifer :: identifiers)
      | _ -> (parser, List.rev identifiers)
  in
  parse_function_parameters' parser []

and parse_call_expression parser fn =
  let parser, arguments = parse_function_arguments parser in
  if not (peek_is parser Token.RightParen) then
    failwith "expected a right paren after args"
  else
    let parser = next_token parser in
    let parser = chomp_semicolon parser in
    (parser, Ast.Call { func = fn; arguments })

and parse_function_arguments parser =
  if peek_is parser Token.RightParen then (next_token parser, [])
  else
    let parser = next_token parser in
    let parser, arg = parse_expression parser Lowest in
    let rec parse_function_arguments' parser arguments =
      match parser.peek_token with
      | Token.Comma ->
          let parser = next_token parser in
          let parser = next_token parser in
          let parser, arg = parse_expression parser Lowest in
          parse_function_arguments' parser (arg :: arguments)
      | _ -> (parser, List.rev arguments)
    in
    parse_function_arguments' parser [ arg ]

and parse_if_expression parser =
  let parser, ok = expect_peek_left_paren parser in
  if not ok then failwith "expected a left paren"
  else
    let parser, condition = parse_expression parser Lowest in
    let ok = expect_current_is parser Token.RightParen in
    if not ok then
      failwith
        (Printf.sprintf "expected a right paren: %s"
           (Token.show parser.peek_token))
    else
      let parser, ok = expect_peek_left_brace parser in
      if not ok then failwith "expected a left brace"
      else
        let parser, consequence = parse_block_statement parser in
        if peek_is parser Token.Else then
          let parser = next_token parser in
          let parser, ok = expect_peek_left_brace parser in
          if ok then
            let parser, alternative = parse_block_statement parser in
            ( parser,
              Ast.If { condition; consequence; alternative = Some alternative }
            )
          else failwith "expected left brace"
        else (parser, Ast.If { condition; consequence; alternative = None })

and parse_block_statement parser =
  let parser = next_token parser in
  let rec parse_block_statement' parser statements =
    match parser.cur_token with
    | Token.RightBrace | Token.Eof -> (parser, List.rev statements)
    | _ -> (
        let statement = parse_statement parser in
        match statement with
        | Ok (parser, stmt) ->
            parse_block_statement' (next_token parser) (stmt :: statements)
        | _ -> parse_block_statement' (next_token parser) statements)
  in
  let parser, statements = parse_block_statement' parser [] in
  (parser, Ast.{ statements })

and parse_grouped_expression parser =
  let parser = next_token parser in
  let parser, expr = parse_expression parser Lowest in
  let parser, ok = expect_peek_right_paren parser in
  if not ok then
    failwith
      (Fmt.failwith "expected right paren: %s" (Token.show parser.cur_token))
  else (parser, expr)

and parse_bool_expression parser =
  match parser.cur_token with
  | Token.True -> (parser, Ast.Boolean true)
  | Token.False -> (parser, Ast.Boolean false)
  | _ -> failwith "unexpected bool token"

let show_expression = Ast.show_expression

let string_of_statement = function
  | Ast.Let stmt ->
      Fmt.str "LET: let %s = %s"
        (Ast.show_identifier stmt.name)
        (show_expression stmt.value)
  | Ast.Return expr -> Fmt.str "RETURN %s" (show_expression expr)
  | Ast.ExpressionStatement expr -> Fmt.str "EXPR %s;" (show_expression expr)
  | BlockStatement _ -> assert false

let string_of_ident ident = Ast.(ident.identifier)

let print_node = function
  | Ast.Program program ->
      Fmt.pr "Program: [@.";
      List.iter program.statements ~f:(fun s ->
          Fmt.pr "  %s@." (string_of_statement s));
      Fmt.pr "]@."
  | _ -> failwith "yaya"

module Test = struct
  let expect_program input =
    let lexer = Lexer.init input in
    let parser = init lexer in
    let program = parse parser in
    match program with
    | Ok program -> print_node program
    | Error msg -> Fmt.failwith "error...%s" msg
  (* | Error msg -> Fmt.failwith "%a@." pp_parse_error msg *)

  let%expect_test "TestLetStatement" =
    let input =
      {|
     let x = 5;
     let y = 10;
     let foobar = 838383;|}
    in
    expect_program input;
    [%expect
      {|
Program: [
  LET: let { identifier = "x" } = (Integer 5)
  LET: let { identifier = "y" } = (Integer 10)
  LET: let { identifier = "foobar" } = (Integer 838383)
]|}]

  let%expect_test "single let statement" =
    let input = {|
    let x = 5;
  |} in
    expect_program input;
    [%expect
      {|
    Program: [
      LET: let { identifier = "x" } = (Integer 5)
    ] |}]

  let%expect_test "TestReturnStatement" =
    let input = {|
     return 5;
     return 10;
     return 993322;
   |} in
    expect_program input;
    [%expect
      {|
Program: [
  RETURN (Integer 5)
  RETURN (Integer 10)
  RETURN (Integer 993322)
]
    |}]

  let%expect_test "TestPrefixOperator" =
    let input = {|
     !5;
     !15;
     -20;
     -3;
   |} in
    expect_program input;
    [%expect
      {|
Program: [
  EXPR Prefix {operator = Token.Bang; right = (Integer 5)};
  EXPR Prefix {operator = Token.Bang; right = (Integer 15)};
  EXPR Prefix {operator = Token.Minus; right = (Integer 20)};
  EXPR Prefix {operator = Token.Minus; right = (Integer 3)};
]
    |}]

  let%expect_test "TestInfixOperator" =
    let input =
      {|
     5 + 5;
     5 - 5;
     5 * 5;
     5 / 5;
     5 > 5;
     5 < 5;
     5 == 5;
     5 != 5;
   |}
    in
    expect_program input;
    [%expect
      {|
Program: [
  EXPR Infix {left = (Integer 5); operator = Token.Plus; right = (Integer 5)};
  EXPR Infix {left = (Integer 5); operator = Token.Minus; right = (Integer 5)};
  EXPR Infix {left = (Integer 5); operator = Token.Asterisk; right = (Integer 5)};
  EXPR Infix {left = (Integer 5); operator = Token.Slash; right = (Integer 5)};
  EXPR Infix {left = (Integer 5); operator = Token.GreaterThan; right = (Integer 5)};
  EXPR Infix {left = (Integer 5); operator = Token.LessThan; right = (Integer 5)};
  EXPR Infix {left = (Integer 5); operator = Token.Equal; right = (Integer 5)};
  EXPR Infix {left = (Integer 5); operator = Token.NotEqual; right = (Integer 5)};
]
    |}]

  let%expect_test "TestBoolExpression" =
    let input =
      {|
     true;
     false;
     let foobar = true;
     let barfoo = false;
   |}
    in
    expect_program input;
    [%expect
      {|
Program: [
  EXPR (Boolean true);
  EXPR (Boolean false);
  LET: let { identifier = "foobar" } = (Boolean true)
  LET: let { identifier = "barfoo" } = (Boolean false)
]
    |}]

  let%expect_test "TestIfExpression" =
    let input = {|
      if (x < y) { x } else { y };
   |} in
    expect_program input;
    [%expect
      {|
Program: [
  EXPR If {
  condition =
  Infix {left = (Identifier { identifier = "x" }); operator = Token.LessThan;
    right = (Identifier { identifier = "y" })};
  consequence =
  { statements = [(ExpressionStatement (Identifier { identifier = "x" }))] };
  alternative =
  (Some { statements =
          [(ExpressionStatement (Identifier { identifier = "y" }))] })};
]
    |}]

  let%expect_test "TestFunctionLiteral" =
    let input = {|
    fn(x, y) { return x + y; }
   |} in
    expect_program input;
    [%expect
      {|
Program: [
  EXPR FunctionLiteral {parameters = [{ identifier = "x" }; { identifier = "y" }];
  body =
  { statements =
    [(Return
        Infix {left = (Identifier { identifier = "x" });
          operator = Token.Plus; right = (Identifier { identifier = "y" })})
      ]
    }};
]
    |}]

  let%expect_test "TestFunctionCall" =
    let input = {|
    add(1, 2 * 3, 4 + 5);
   |} in
    expect_program input;
    [%expect
      {|
Program: [
  EXPR Call {func = (Identifier { identifier = "add" });
  arguments =
  [(Integer 1);
    Infix {left = (Integer 2); operator = Token.Asterisk; right = (Integer 3)};
    Infix {left = (Integer 4); operator = Token.Plus; right = (Integer 5)}]};
]
    |}]

  let%expect_test "TestFunctionCallLiteral" =
    let input = {|
    fn(x, y) { x + y; }(2, 3);
  |} in
    expect_program input;
    [%expect
      {|
Program: [
  EXPR Call {
  func =
  FunctionLiteral {parameters = [{ identifier = "x" }; { identifier = "y" }];
    body =
    { statements =
      [(ExpressionStatement
          Infix {left = (Identifier { identifier = "x" });
            operator = Token.Plus; right = (Identifier { identifier = "y" })})
        ]
      }};
  arguments = [(Integer 2); (Integer 3)]};
]
     |}]
end
