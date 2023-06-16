type 'a env = 'a Environment.t

type t =
  | Integer of int
  | Boolean of bool
  | String of string
  | Return of t
  | Function of func
  | Builtin of builtin
  | Null
[@@deriving show]

and func = {
  parameters : Ast.identifier list
 ; body : Ast.block
 ; env : t env [@opaque]
}
[@@deriving show]


and builtin = t list -> t
