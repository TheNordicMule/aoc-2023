open Core

type color =
  | Red of int
  | Green of int
  | Blue of int

let substr line index = String.sub ~pos:index ~len:(String.length line - index) line

let get_game_number line =
  let substring = substr line 5 in
  Fmt.pr "@.Game number: %s@." substring
;;

let get_possible (line : string) index =
  let substr = String.sub ~pos:index ~len:(String.length line - index) line in
  let _ = Fmt.pr "@.%s@." substr in
  match index with
  | index when index = String.length line -> true
  | _ -> true
;;

let pt_1 =
  let r file = In_channel.read_lines file in
  let content = r "./input/day2.txt" in
  let _ = Red 12, Green 13, Blue 14 in
  let _ = get_possible (List.hd_exn content) 0 in
  get_game_number (List.nth_exn content 3)
;;

let _ = pt_1
