type 'a env = 'a Environment.t

type t =
  | Integer of int
  | Boolean of bool
  | String of string
  | Return of t
  | Function of func
  | Builtin of builtin
  | Array of t list
  | Null
[@@deriving show]

and func = {
  parameters : Ast.identifier list
 ; body : Ast.block
 ; env : t env [@opaque]
}
[@@deriving show]


and builtin = BuiltinFn of (t list -> t)
    [@@deriving show { with_path = false }]

(* | Macro of macro *)
(* and macro = *)
(*   { m_parameters : Ast.identifier list *)
(*   ; m_body : Ast.block *)
(*   ; m_env : t environment [@opaque] *)
(*   } *)

let monkey_true = Boolean true
let monkey_false = Boolean false
let builtin_fn f = Builtin (BuiltinFn f)
