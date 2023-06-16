type identifier = { identifier : string }
[@@deriving show { with_path = false }, sexp]

type block = { statements : statement list }
[@@deriving show { with_path = false }, sexp]

and expression =
  | Identifier of identifier
  | Integer of int
  | Boolean of bool
  | String of string
  | Prefix of { operator : Token.t; right : expression }
  | Infix of { left : expression; operator : Token.t; right : expression }
  | If of {
      condition : expression;
      consequence : block;
      alternative : block option;
    }
  | FunctionLiteral of {parameters: identifier list; body: block}
  | Call of {func: expression; arguments: expression list}
[@@deriving show { with_path = false }, sexp]

and statement =
  | Let of { name : identifier; value : expression }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block
[@@deriving show { with_path = false }, sexp]

type program = { statements : statement list }
[@@deriving show { with_path = false }, sexp]

type node =
  | Program of program
  | Statement of statement
  | Expression of expression
