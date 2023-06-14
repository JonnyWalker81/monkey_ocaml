open Core
open Oquery.Lexer
(* open Async *)
(* open Cohttp *)
(* open Cohttp_async *)
(* open Cohttp_async_unix *)
(* open Yojson.Basic.Util *)

(* let get_age_of_issue issue_key = *)
(*   let base_url = "https://your-jira-instance.atlassian.net/rest/api/2/" in *)
(*   let issue_url = base_url ^ "issue/" ^ issue_key in *)

(*   let headers = *)
(*     Cohttp.Header.init () *)
(*     |> Cohttp.Header.add_authorization_basic "username" "api_token" *)
(*   in *)

(*   Client.get (Uri.of_string issue_url) >>= fun (resp, body) -> *)
(*   Body.to_string body >>= fun body_str -> *)
(*   let json = Yojson.Basic.from_string body_str in *)
(*   let created_date = *)
(*     json |> member "fields" |> member "created" |> to_string |> Yojson.Basic.from_string *)
(*     |> member "created" |> to_string *)
(*   in *)
(*   let created_date = Unix.( *)
(*       strptime created_date "%Y-%m-%dT%H:%M:%S%z" *)
(*       |> gmtime *)
(*       |> mktime *)
(*       |> gmtime *)
(*     ) *)
(*   in *)

(*   let changelog_url = base_url ^ "issue/" ^ issue_key ^ "/changelog" in *)
(*   Client.get (Uri.of_string changelog_url) >>= fun (resp, body) -> *)
(*   Body.to_string body >>= fun body_str -> *)
(*   let json = Yojson.Basic.from_string body_str in *)
(*   let in_progress_date = *)
(*     json |> member "histories" *)
(*     |> to_list *)
(*     |> List.fold_left (fun acc history -> *)
(*            let items = history |> member "items" |> to_list in *)
(*            let in_progress_item = *)
(*              List.find_opt (fun item -> *)
(*                  let field = item |> member "field" |> to_string in *)
(*                  let to_status = item |> member "toString" |> to_string in *)
(*                  field = "status" && to_status = "In Progress" *)
(*                ) items *)
(*            in *)
(*            match in_progress_item with *)
(*            | Some item -> *)
(*                let created_date = *)
(*                  item |> member "created" |> to_string |> Yojson.Basic.from_string |> member "created" |> to_string *)
(*                in *)
(*                let in_progress_date = Unix.( *)
(*                    strptime created_date "%Y-%m-%dT%H:%M:%S%z" *)
(*                    |> gmtime *)
(*                    |> mktime *)
(*                    |> gmtime *)
(*                  ) *)
(*                in *)
(*                Some in_progress_date *)
(*            | None -> acc *)
(*          ) None *)
(*   in *)

(*   let current_date = Unix.(gmtime (time ())) in *)
(*   let age = *)
(*     match in_progress_date with *)
(*     | Some in_progress_date -> int_of_float (Unix.(difftime current_date in_progress_date) /. 86400.) *)
(*     | None -> int_of_float (Unix.(difftime current_date created_date) /. 86400.) *)
(*   in *)

(*   Deferred.return age *)

(* let print_chars input start = *)
(*   match start with *)
(*   | 0 -> () *)
(*   | else -> print_char *)

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
  let t = prompt_user () in
  match t with
  | Some t ->
      let tokens = generate_tokens t in
      print_tokens tokens;
      repl ()
  | None -> Printf.printf "Nothing entered..."

let () =
  let user = Unix.getlogin () in
  Printf.printf "Hello %s! This is the Monkey programming language!\n" user;
  Printf.printf "Feel free to type in commands\n";
  Out_channel.flush Out_channel.stdout;
  repl ()
