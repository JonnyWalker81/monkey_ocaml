type t =
  | Illegal
  | Ident of string
  | Integer of string
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  | If
  | Else
  | Return
  | True
  | False
  | Function
  | Let
  | String of string
  | Eof
[@@deriving show, eq, sexp]

let token_of_string = function
  | Illegal -> "Illegal"
  | Ident a -> "Ident " ^ a
  | Integer a -> "Integer " ^ a
  | Assign -> "Assign"
  | Plus -> "Plus"
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | LessThan -> "<"
  | GreaterThan -> ">"
  | Equal -> "=="
  | NotEqual -> "!="
  | If -> "if"
  | Else -> "else"
  | Return -> "return"
  | True -> "true"
  | False -> "false"
  | Function -> "Function"
  | Let -> "Let"
  | String s -> "String " ^ s
  | Eof -> "Eof"

let lookup_ident ident =
  match ident with
  | "fn" -> Function
  | "let" -> Let
  | "if" -> If
  | "else" -> Else
  | "true" -> True
  | "false" -> False
  | "return" -> Return
  | _ -> Ident ident

let pretty_print ppf tok = Fmt.pf ppf "Token %s" (token_of_string tok)
