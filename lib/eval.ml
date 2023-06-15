type t = { parser : Parser.t }

let init parser = { parser }

let rec eval node =
  match node with
  | Ast.Program p -> eval_statements p.statements
  | Ast.Expression expr -> eval_expr expr
  | Ast.Statement statement -> eval_statement statement
  | _ -> failwith "unexpected node"

and eval_expr expr =
  match expr with
  | Integer i -> Object.Integer i
  | Boolean b -> eval_boolean b
  | Prefix { operator; right } ->
      let result = eval_expr right in
      eval_prefix_expr operator result
  | Infix { left; operator; right } ->
      let left = eval_expr left in
      let right = eval_expr right in
      eval_infix_expr operator left right
  | If { condition; consequence; alternative } ->
      eval_if_statement condition consequence alternative
  | _ -> failwith "unexpected expr"

and eval_if_statement condition consequence alternative =
  let condition = eval_expr condition in
  if is_truthy condition then eval_block consequence
  else if Option.is_some alternative then eval_block (Option.get alternative)
  else Object.Null

and eval_block block = eval_statements block.statements

and is_truthy = function
  | Object.Null -> false
  | Object.Boolean b -> b
  | _ -> true

and eval_boolean b = Object.Boolean b

and eval_infix_expr operator left right =
  match (left, right) with
  | Object.Integer _, Object.Integer _ ->
      eval_integer_infix_expr operator left right
  | Object.Boolean _, Object.Boolean _ ->
      eval_integer_infix_expr operator left right
  | _ -> Object.Null

and eval_integer_infix_expr operator left right =
  match (operator, left, right) with
  | Token.Plus, Object.Integer leftVal, Object.Integer rightVal ->
      Object.Integer (leftVal + rightVal)
  | Token.Minus, Object.Integer leftVal, Object.Integer rightVal ->
      Object.Integer (leftVal - rightVal)
  | Token.Asterisk, Object.Integer leftVal, Object.Integer rightVal ->
      Object.Integer (leftVal * rightVal)
  | Token.Slash, Object.Integer leftVal, Object.Integer rightVal ->
      Object.Integer (leftVal / rightVal)
  | Token.LessThan, Object.Integer leftVal, Object.Integer rightVal ->
      eval_boolean (leftVal < rightVal)
  | Token.LessThan, Object.Boolean leftVal, Object.Boolean rightVal ->
      eval_boolean (leftVal < rightVal)
  | Token.GreaterThan, Object.Integer leftVal, Object.Integer rightVal ->
      eval_boolean (leftVal > rightVal)
  | Token.GreaterThan, Object.Boolean leftVal, Object.Boolean rightVal ->
      eval_boolean (leftVal > rightVal)
  | Token.Equal, Object.Integer leftVal, Object.Integer rightVal ->
      eval_boolean (leftVal = rightVal)
  | Token.Equal, Object.Boolean leftVal, Object.Boolean rightVal ->
      eval_boolean (leftVal = rightVal)
  | Token.NotEqual, Object.Integer leftVal, Object.Integer rightVal ->
      eval_boolean (leftVal <> rightVal)
  | Token.NotEqual, Object.Boolean leftVal, Object.Boolean rightVal ->
      eval_boolean (leftVal <> rightVal)
  | _ -> Object.Null

and eval_prefix_expr op right =
  match op with
  | Token.Bang -> eval_bang_expr right
  | Token.Minus -> eval_minus_expr right
  | _ -> failwith "unexpected prefix expr"

and eval_minus_expr right =
  match right with Object.Integer i -> Object.Integer (-i) | _ -> Object.Null

and eval_bang_expr right =
  match right with
  | Object.Boolean b -> Object.Boolean (not b)
  | Object.Integer _ -> Object.Boolean false
  | Object.Null -> Object.Boolean true
  | _ -> failwith "expected a boolean: %s"

and eval_statements statements =
  let rec eval_statements' statements result =
    match statements with
    | [] -> result
    | h :: rest ->
        let result = eval_statement h in
        eval_statements' rest result
  in
  eval_statements' statements Object.Null

and eval_statement statement =
  match statement with
  | Ast.ExpressionStatement expr -> eval_expr expr
  | _ -> failwith "unsupported statement"

module Test = struct
  let eval_program input =
    let lexer = Lexer.init input in
    let parser = Parser.init lexer in
    let program = Parser.parse parser in
    match program with Ok p -> eval p | _ -> failwith "error parsing program"

  let expect_primative obj =
    match obj with
    | Object.Integer i -> Fmt.pr "%i\n" i
    | Object.Boolean b -> Fmt.pr "%b\n" b
    | Object.Null -> Fmt.pr "null"
    | _ -> Fmt.failwith "unexpected obj: %s" (Object.show obj)

  let expect_result input =
    let result = eval_program input in
    expect_primative result

  let%expect_test "testIntegerLiteral" =
    let input = {|
        5;
      |} in
    let result = eval_program input in
    expect_primative result;
    let input = {|
        10;
      |} in
    let result = eval_program input in
    expect_primative result;
    let input = {|
        999;
      |} in
    let result = eval_program input in
    expect_primative result;
    [%expect {|
           5
           10
           999
         |}]

  let%expect_test "testBooleanExpression" =
    let input = {|
    true;
  |} in
    expect_result input;
    let input = {|
    false;
  |} in
    [%expect {|
      true
    |}];
    expect_result input;
    [%expect {|
        false
    |}];
    let input = {|
    true == true;
  |} in
    expect_result input;
    [%expect {|
    true
  |}];
    let input = {|
    false == false;
  |} in
    expect_result input;
    [%expect {|
    true
  |}];
    let input = {|
    true == false;
  |} in
    expect_result input;
    [%expect {|
    false
  |}];
    let input = {|
    (1 < 2) == true;
  |} in
    expect_result input;
    [%expect {|
    true
  |}]

  let%expect_test "testBangOperator" =
    let input = {|
    !5;
  |} in
    let result = eval_program input in
    expect_primative result;
    let input = {|
    !false;
  |} in
    let result = eval_program input in
    expect_primative result;
    let input = {|
    !true;
  |} in
    let result = eval_program input in
    expect_primative result;
    let input = {|
    !!5;
  |} in
    let result = eval_program input in
    expect_primative result;
    [%expect {|
    false
    true
    false
    true
  |}]

  let%expect_test "testMinusOperator" =
    let input = {|
    5;
  |} in
    expect_result input;
    [%expect {|
    5
  |}];
    let input = {|
    -5;
  |} in
    expect_result input;
    [%expect {|
    -5
  |}];
    let input = {|
    -10;
  |} in
    expect_result input;
    [%expect {|
    -10
  |}]

  let%expect_test "testIntegerInfixOperators" =
    let input = {|
    5 + 5 + 5 + 5 - 10;
  |} in
    expect_result input;
    [%expect {|
    10
  |}];
    let input = {|
    2 * 2 * 2 * 2 * 2;
  |} in
    expect_result input;
    [%expect {|
    32
  |}];
    let input = {|
      -50 + 100 + -50;
    |} in
    expect_result input;
    [%expect {|
      0
    |}];
    let input = {|
    2 * (5 + 10);
  |} in
    expect_result input;
    [%expect {|
    30
  |}];
    let input = {|
    (5 + 10 * 2 + 15 / 3) * 2 + -10
  |} in
    expect_result input;
    [%expect {|
    50
  |}]

  let%expect_test "testIfElseExpressions" =
    let input = {|
    if (true) { 10 }
  |} in
    expect_result input;
    [%expect {|
    10
  |}];
    let input = {|
    if (false) { 10 }
  |} in
    expect_result input;
    [%expect {|
    null
  |}];
    let input = {|
    if (1 > 2) { 10 } else { 20 };
  |} in
    expect_result input;
    [%expect {|
    20
  |}]
end
