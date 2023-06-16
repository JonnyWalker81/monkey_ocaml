type t = { parser : Parser.t }

let init parser = { parser }

let rec eval node env =
  match node with
  | Ast.Program p -> eval_program p env
  | Ast.Expression expr -> eval_expr expr env
  | Ast.Statement statement -> eval_statement statement env
  | _ -> failwith "unexpected node"

and eval_program program env =
  let rec eval_program' statements env result =
    match statements with
    | [] -> result
    | h :: rest -> (
        let result = eval_statement h env in
        match result with
        | Object.Return o -> o
        | _ -> eval_program' rest env result)
  in
  eval_program' program.statements env Object.Null

and eval_expr expr env =
  match expr with
  | Identifier ident -> eval_identifier ident env
  | Integer i -> Object.Integer i
  | Boolean b -> eval_boolean b
  | String s -> Object.String s
  | Prefix { operator; right } ->
      let result = eval_expr right env in
      eval_prefix_expr operator result
  | Infix { left; operator; right } ->
      let left = eval_expr left env in
      let right = eval_expr right env in
      eval_infix_expr operator left right
  | If { condition; consequence; alternative } ->
      eval_if_statement condition consequence alternative env
  | FunctionLiteral { parameters; body } ->
      Object.Function { parameters; body; env }
  | Call { func; arguments } ->
      let func = eval_expr func env in
      let args = eval_exprs arguments env in
      apply_function func args
  | _ -> failwith "unexpected expr"

and eval_exprs expressions env =
  let rec eval_exprs' expressions env results =
    match expressions with
    | [] -> List.rev results
    | h :: rest ->
        let evaluated = eval_expr h env in
        eval_exprs' rest env (evaluated :: results)
  in
  eval_exprs' expressions env []

and apply_function func args =
  match func with
  | Object.Function func -> (
      let extended_env = extend_function_env func args in
      let evaluated = eval_block func.body extended_env in
      match evaluated with Object.Return v -> v | _ -> evaluated)
  | _ -> failwith "apply_function"

and extend_function_env func args =
  let env = Environment.init_with_env func.env in
  let rec arg_iter params idx =
    match params with
    | [] -> env
    | (h :: rest : Ast.identifier list) ->
        let _ = Environment.set env h.identifier (List.nth args idx) in
        arg_iter rest (idx + 1)
  in
  arg_iter func.parameters 0

and eval_identifier ident env =
  match Environment.get env ident.identifier with
  | Some v -> v
  | None -> Object.Null

and eval_if_statement condition consequence alternative env =
  let condition = eval_expr condition env in
  if is_truthy condition then eval_block consequence env
  else if Option.is_some alternative then
    eval_block (Option.get alternative) env
  else Object.Null

and eval_block block env = eval_statements block.statements env

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
  | Object.String _, Object.String _ ->
      eval_string_infix_expr operator left right
  | _ -> Object.Null

and eval_string_infix_expr operator left right =
  match (operator, left, right) with
  | Token.Plus, Object.String l, Object.String r -> Object.String (l ^ r)
  | _ -> failwith "expected strings"

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

and eval_statements statements env =
  let rec eval_statements' statements env result =
    match statements with
    | [] -> result
    | h :: rest -> (
        let result = eval_statement h env in
        match result with
        | Object.Return _ -> result
        | _ -> eval_statements' rest env result)
  in
  eval_statements' statements env Object.Null

and eval_statement statement env =
  match statement with
  | Ast.ExpressionStatement expr -> eval_expr expr env
  | Ast.Let { name; value } ->
      let result = eval_expr value env in
      Environment.set env name.identifier result
  | Ast.Return expr -> Object.Return (eval_expr expr env)
  | Ast.BlockStatement block -> eval_block_statement block env
  | _ -> failwith "unsupported statement"

and eval_block_statement block env =
  let rec eval_block_statement' statements env result =
    match statements with
    | [] -> result
    | h :: rest -> (
        let result = eval_statement h env in
        match result with
        | Object.Return o -> o
        | _ -> eval_block_statement' rest env result)
  in
  eval_block_statement' block.statements env Object.Null

module Test = struct
  let eval_program input =
    let lexer = Lexer.init input in
    let parser = Parser.init lexer in
    let program = Parser.parse parser in
    let env = Environment.init () in
    match program with
    | Ok p -> eval p env
    | _ -> failwith "error parsing program"

  let expect_primative obj =
    match obj with
    | Object.Integer i -> Fmt.pr "%i\n" i
    | Object.Boolean b -> Fmt.pr "%b\n" b
    | Object.String s -> Fmt.pr "%s\n" s
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

  let%expect_test "testReturnValue" =
    let input = {|
    return 10;
  |} in
    expect_result input;
    [%expect {|
    10
  |}];
    let input = {|
    9; return 5 * 2; 9;
  |} in
    expect_result input;
    [%expect {|
    10
  |}];
    let input =
      {|
    if(10 > 1) {
      if(10 > 1) {
        return 10;
      }

      return 1;
    }
  |}
    in
    expect_result input;
    [%expect {|
    10
  |}]

  let%expect_test "testLetStatements" =
    let input = {|
   let a = 5; a;
 |} in
    expect_result input;
    [%expect {|
   5
 |}]

  let%expect_test "testFunctionLiteral" =
    let input = {|
   let identity = fn(x) { x; }; identity(5);
 |} in
    expect_result input;
    [%expect {|
   5
 |}];
    let input = {|
   let add = fn(x, y) { x + y; }; add(5, 5)
 |} in
    expect_result input;
    [%expect {|
   10
 |}];
    let input =
      {|
let newAdder = fn(x) {
     fn(y) { x + y };
};
   let addTwo = newAdder(2);
   addTwo(2);
 |}
    in
    expect_result input;
    [%expect {|
4
 |}]

  let%expect_test "testStringLiteral" =
    let input = {|
   "Hello World!";
 |} in
    expect_result input;
    [%expect {|
 Hello World!
 |}]

  let%expect_test "testStringConcatenation" =
    let input = {|
   "Hello" + " " + "World!"
 |} in
    expect_result input;
    [%expect {|
Hello World!
 |}]

      let%expect_test "testBuiltinFunction" =
    let input = {|
    let("")
  |} in
    expect_result input;
    [%expect {|
    0
  |}
   ]
end
