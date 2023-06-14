open Base
include Token

type lexer = { input : string; position : int; read_position : int; ch : char }
[@@deriving show]

let null_byte = '\x00'

let read_char lexer =
  let read_to_end = lexer.read_position >= String.length lexer.input in
  let new_ch =
    if read_to_end then null_byte
    else String.get lexer.input lexer.read_position
  in
  {
    lexer with
    position = lexer.read_position;
    read_position = lexer.read_position + 1;
    ch = new_ch;
  }

let init input_string =
  let lexer =
    { input = input_string; position = 0; read_position = 0; ch = null_byte }
  in
  read_char lexer

let is_letter ch =
  match ch with
  | 'A' .. 'Z' -> true
  | 'a' .. 'z' -> true
  | '_' -> true
  | _ -> false

let is_whitespace ch =
  match ch with
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | '\r' -> true
  | _ -> false

let is_digit ch = match ch with '0' .. '9' -> true | _ -> false

let rec read_while condition lexer acc =
  if condition lexer.ch then
    read_while condition (read_char lexer) (acc ^ String.make 1 lexer.ch)
  else (lexer, acc)

let read_identifier lexer = read_while is_letter lexer ""
let read_number lexer = read_while is_digit lexer ""

let rec skip_while condition lexer =
  if condition lexer.ch then skip_while condition (read_char lexer) else lexer

let peek lexer ch current matched =
  if lexer.read_position >= String.length lexer.input then (lexer, Token.Eof)
  else
    let peek_char = String.get lexer.input lexer.read_position in
    if Char.to_int peek_char = Char.to_int ch then
      let lexer = read_char lexer in
      (read_char lexer, matched)
    else (read_char lexer, current)

let next_token lexer =
  let lexer = skip_while is_whitespace lexer in
  let ch = lexer.ch in
  match ch with
  | '=' -> peek lexer '=' Token.Assign Token.Equal
  | ';' -> (read_char lexer, Token.Semicolon)
  | '(' -> (read_char lexer, Token.LeftParen)
  | ')' -> (read_char lexer, Token.RightParen)
  | ',' -> (read_char lexer, Token.Comma)
  | '+' -> (read_char lexer, Token.Plus)
  | '{' -> (read_char lexer, Token.LeftBrace)
  | '}' -> (read_char lexer, Token.RightBrace)
  | '-' -> (read_char lexer, Token.Minus)
  | '!' -> peek lexer '=' Token.Bang Token.NotEqual
  | '*' -> (read_char lexer, Token.Asterisk)
  | '/' -> (read_char lexer, Token.Slash)
  | '<' -> (read_char lexer, Token.LessThan)
  | '>' -> (read_char lexer, Token.GreaterThan)
  | '\x00' -> (read_char lexer, Token.Eof)
  | ch when is_letter ch ->
      let l, ident = read_identifier lexer in
      (l, Token.lookup_ident ident)
  | ch when is_digit ch ->
      let l, number = read_number lexer in
      (l, Token.Integer number)
  | _ -> (read_char lexer, Token.Illegal)

let generate_tokens input_string =
  let lexer = init input_string in
  let rec gen lxr tokens =
    match next_token lxr with
    | _, Token.Eof -> List.rev_append tokens [ Token.Eof ]
    | l, tok -> gen l (tok :: tokens)
  in
  gen lexer []

module Test = struct
  include Token

  let print_tokens tokens =
    List.iter tokens ~f:(fun token -> Fmt.pr "%s\n" @@ Token.show token)

  let%expect_test "next token" =
    let input = "=(){},;;" in
    let tokens = generate_tokens input in
    print_tokens tokens;
    [%expect
      {|
    Token.Assign
    Token.LeftParen
    Token.RightParen
    Token.LeftBrace
    Token.RightBrace
    Token.Comma
    Token.Semicolon
    Token.Semicolon
    Token.Eof|}]

  let%expect_test "monkeyNextToken" =
    let input =
      {|
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
   };

   let result = add(five, ten);
   !-/*5;
   5 < 10 > 5;

   if (5 < 10) {
     return true;
   } else {
     return false;
   }

   10 == 10;
   10 != 9;|}
    in

    let tokens = generate_tokens input in
    print_tokens tokens;
    [%expect
      {|
    Token.Let
    (Token.Ident "five")
    Token.Assign
    (Token.Integer "5")
    Token.Semicolon
    Token.Let
    (Token.Ident "ten")
    Token.Assign
    (Token.Integer "10")
    Token.Semicolon
    Token.Let
    (Token.Ident "add")
    Token.Assign
    Token.Function
    Token.LeftParen
    (Token.Ident "x")
    Token.Comma
    (Token.Ident "y")
    Token.RightParen
    Token.LeftBrace
    (Token.Ident "x")
    Token.Plus
    (Token.Ident "y")
    Token.Semicolon
    Token.RightBrace
    Token.Semicolon
    Token.Let
    (Token.Ident "result")
    Token.Assign
    (Token.Ident "add")
    Token.LeftParen
    (Token.Ident "five")
    Token.Comma
    (Token.Ident "ten")
    Token.RightParen
    Token.Semicolon
    Token.Bang
    Token.Minus
    Token.Slash
    Token.Asterisk
    (Token.Integer "5")
    Token.Semicolon
    (Token.Integer "5")
    Token.LessThan
    (Token.Integer "10")
    Token.GreaterThan
    (Token.Integer "5")
    Token.Semicolon
    Token.If
    Token.LeftParen
    (Token.Integer "5")
    Token.LessThan
    (Token.Integer "10")
    Token.RightParen
    Token.LeftBrace
    Token.Return
    Token.True
    Token.Semicolon
    Token.RightBrace
    Token.Else
    Token.LeftBrace
    Token.Return
    Token.False
    Token.Semicolon
    Token.RightBrace
    (Token.Integer "10")
    Token.Equal
    (Token.Integer "10")
    Token.Semicolon
    (Token.Integer "10")
    Token.NotEqual
    (Token.Integer "9")
    Token.Semicolon
    Token.Eof|}]
end
