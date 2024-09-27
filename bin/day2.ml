open Core

let judge_number_is_not_sufficient couple =
  match couple |> String.strip |> String.split ~on:' ' with
  | [ number; "red" ] -> int_of_string number > 12
  | [ number; "green" ] -> int_of_string number > 13
  | [ number; "blue" ] -> int_of_string number > 14
  | any ->
    Fmt.pr "@.Unmatched parts: %a@." (Fmt.list Fmt.string) any;
    failwith "Pattern matching failed - input not as expected"
;;

let calculate_line_should_score line =
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

let calculate_game_number line =
  let parts = List.hd_exn (String.split ~on:':' line) in
  List.nth_exn (parts |> String.strip |> String.split ~on:' ') 1 |> int_of_string
;;

let calculate_line_score acc line =
  let game_number = calculate_game_number line in
  (*let _ = game_number |> Fmt.pr "@.line_should_score: %i@." in*)
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

(*separator part 1 and 2*)

type game_color =
  { red : int
  ; green : int
  ; blue : int
  }

let calculate_line_color acc curr =
  match curr |> String.strip |> String.split ~on:' ' with
  | [ number; "red" ] when int_of_string number > acc.red ->
    { red = int_of_string number; green = acc.green; blue = acc.blue }
  | [ number; "green" ] when int_of_string number > acc.green ->
    { red = acc.red; green = int_of_string number; blue = acc.blue }
  | [ number; "blue" ] when int_of_string number > acc.blue ->
    { red = acc.red; green = acc.green; blue = int_of_string number }
  | _ -> acc
;;

let calculate_line_power = function
  | { red; green; blue } -> red * green * blue
;;

let calculate_game_power_score acc line =
  let parts = String.split ~on:':' line in
  let game = List.nth_exn parts 1 in
  let turns = String.split ~on:';' game in
  let details = List.map turns ~f:(fun a -> String.split ~on:',' a) |> List.concat in
  let line_game_color =
    List.fold details ~init:{ red = 0; green = 0; blue = 0 } ~f:calculate_line_color
  in
  let line_power = calculate_line_power line_game_color in
  line_power + acc
;;

let pt_2 =
  let r file = In_channel.read_lines file in
  let content = r "./input/day2-prod.txt" in
  let answer = List.fold content ~init:0 ~f:calculate_game_power_score in
  answer
;;

let _ = pt_1 |> Fmt.pr "@.part 1: %i" in
pt_2 |> Fmt.pr "@.part2: %i"
