open Core

(*type color =*)
(*  | Red of int*)
(*  | Green of int*)
(*  | Blue of int*)

(*let substr line index = String.sub ~pos:index ~len:(String.length line - index) line*)

let calculate_line_should_score _line = false

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
  let _ = calculate_game_number lst |> Fmt.pr "@.parsed: %s@." in
  let game_number = int_of_string (calculate_game_number lst) in
  match calculate_line_should_score line with
  | true -> acc + game_number
  | false -> acc
;;

let pt_1 =
  let r file = In_channel.read_lines file in
  let content = r "./input/day2.txt" in
  let answer = List.fold content ~init:0 ~f:calculate_line_score in
  answer
;;

let _ = pt_1 |> Fmt.pr "@.part 1: %i"
