open Core

let print_tokens tokens =
  List.iter tokens ~f:(fun token ->
      Printf.printf "%s\n" @@ Oquery.Token.show token)

let prompt = ">> "

let prompt_user () =
  Printf.printf "%s" prompt;
  Out_channel.flush Out_channel.stdout;
  In_channel.input_line In_channel.stdin

let rec repl () =
  Out_channel.flush Out_channel.stdout;
  let input = prompt_user () in
  match input with
  | Some input ->
      let lexer = Oquery.Lexer.init input in
      let parser = Oquery.Parser.init lexer in
      let program = Oquery.Parser.parse parser in
      let () = match program with
      | Ok p ->  (Printf.printf "%s\n" (Oquery.Object.show (Oquery.Eval.eval p)))
      | _ -> failwith "error evalutating" in
    repl ()
  | None -> Printf.printf "Nothing entered..."

let () =
  let user = Unix.getlogin () in
  Printf.printf "Hello %s! This is the Monkey programming language!\n" user;
  Printf.printf "Feel free to type in commands\n";
  Out_channel.flush Out_channel.stdout;
  repl ()
