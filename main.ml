open Core

(*let match_string_with_digit = function*)
(*  | "one" -> Some 1*)
(*  | "two" -> Some 2*)
(*  | "three" -> Some 3*)
(*  | "four" -> Some 4*)
(*  | "five" -> Some 5*)
(*  | "six" -> Some 6*)
(*  | "seven" -> Some 7*)
(*  | "eight" -> Some 8*)
(*  | "nine" -> Some 9*)
(*  | _ -> None*)
(**)
(*let lst_of_digit_string =*)
(*  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]*)

let solution =
  (* Function to read the file lines *)
  let r file = In_channel.read_lines file in

  (* Function to check if a character is a digit *)
  let is_digit = function '0' .. '9' -> true | _ -> false in

  (* Read content of the file *)
  let content = r "./data.txt" in

  (* Get the first digit from a char list *)
  let get_number lst = List.find lst ~f:is_digit in

  (* Get the last digit from a char list *)
  let get_last lst = lst |> List.rev |> get_number in

  (* Map over the content and apply the functions *)
  let first =
    List.map content ~f:(fun line ->
        match get_number (String.to_list line) with
        | Some c -> Char.to_int c - Char.to_int '0'
        | None -> failwith "no first digit")
  in

  let last =
    List.map content ~f:(fun line ->
        match get_last (String.to_list line) with
        | Some c -> Char.to_int c - Char.to_int '0'
        | None -> failwith "no last digit")
  in
  let numbers = List.map2_exn first last ~f:(fun i j -> (i * 10) + j) in
  let answer = List.fold numbers ~init:0 ~f:(fun a b -> a + b) in
  answer

let () =
  let result = solution in
  (* Call the solution to get the result *)
  print_endline (string_of_int result)
