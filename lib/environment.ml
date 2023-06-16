type 'a t = { env : (string, 'a) Hashtbl.t; outer : 'a t option }

let init () = { env = Hashtbl.create 0; outer = None }

let init_with_env e = { env = Hashtbl.create 0; outer = Some e }

let rec get (env:'a t) key  =
  match Hashtbl.find_opt env.env key with
  | Some v -> Some v
  | None -> match env.outer with
    | Some e -> get e key
    | None -> None

let set env key value =
  Hashtbl.add env.env key value;
  value
