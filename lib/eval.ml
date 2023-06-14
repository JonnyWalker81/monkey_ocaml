
let eval_statements _statements = failwith "eval_statements"

let eval = function
  | Ast.Program p -> eval_statements p.statements
  | _ -> failwith "unexpected node"

