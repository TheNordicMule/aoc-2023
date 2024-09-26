open Core

let judge_number_is_not_sufficient couple =
  match String.split ~on:' ' (couple |> String.strip) with
  | [ number; "red" ] -> int_of_string number > 12
  | [ number; "green" ] -> int_of_string number > 13
  | [ number; "blue" ] -> int_of_string number > 14
  | any ->
    Fmt.pr "@.Unmatched parts: %a@." (Fmt.list Fmt.string) any;
    failwith "Pattern matching failed - input not as expected"
;;

let calculate_line_should_score line =
  (*let _ = Fmt.pr "@.line: %s@." line in*)
  let parts = String.split ~on:':' line in
  let game = List.nth_exn parts 1 in
  let turns = String.split ~on:';' game in
  let details = List.map turns ~f:(fun a -> String.split ~on:',' a) |> List.concat in
  (*Fmt.pr "details: @.%a" (Fmt.list Fmt.string) details;*)
  let break = List.find details ~f:judge_number_is_not_sufficient in
  match break with
  | Some _ -> false
  | None -> true
;;

let rec calculate_game_number lst =
  match lst with
  | _ :: 'm' :: 'e' :: ' ' :: a :: ':' :: _ ->
    let number = Fmt.str "%s" (String.of_char a) in
    number
  | _ :: 'm' :: 'e' :: ' ' :: a :: b :: _ ->
    let number = Fmt.str "%s" (String.of_char_list [ a; b ]) in
    number
  | _ :: t -> calculate_game_number t
  | _ -> failwith "error parsing problem"
;;

let calculate_line_score acc line =
  let lst = line |> String.to_list in
  let game_number = int_of_string (calculate_game_number lst) in
  let _ = calculate_line_should_score line |> Fmt.pr "@.line_should_score: %b@." in
  match calculate_line_should_score line with
  | true -> acc + game_number
  | false -> acc
;;

let pt_1 =
  let r file = In_channel.read_lines file in
  let content = r "./input/day2-prod.txt" in
  let answer = List.fold content ~init:0 ~f:calculate_line_score in
  answer
;;

let _ = pt_1 |> Fmt.pr "@.part 1: %i"
