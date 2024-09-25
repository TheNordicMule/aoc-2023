open Core

(* Function to check if a character is a digit *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let rec char_list_with_digit = function
  | c :: _ when is_digit c -> Some c (* If the first character is a digit, return it *)
  | 'o' :: 'n' :: 'e' :: _ -> Some '1'
  | 't' :: 'w' :: 'o' :: _ -> Some '2'
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> Some '3'
  | 'f' :: 'o' :: 'u' :: 'r' :: _ -> Some '4'
  | 'f' :: 'i' :: 'v' :: 'e' :: _ -> Some '5'
  | 's' :: 'i' :: 'x' :: _ -> Some '6'
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> Some '7'
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> Some '8'
  | 'n' :: 'i' :: 'n' :: 'e' :: _ -> Some '9'
  | _ :: t -> char_list_with_digit t
  | [] -> None
;;

let rec reverse_map = function
  | c :: _ when is_digit c -> Some c (* If the first character is a digit, return it *)
  | 'e' :: 'n' :: 'o' :: _ -> Some '1'
  | 'o' :: 'w' :: 't' :: _ -> Some '2'
  | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> Some '3'
  | 'r' :: 'u' :: 'o' :: 'f' :: _ -> Some '4'
  | 'e' :: 'v' :: 'i' :: 'f' :: _ -> Some '5'
  | 'x' :: 'i' :: 's' :: _ -> Some '6'
  | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> Some '7'
  | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> Some '8'
  | 'e' :: 'n' :: 'i' :: 'n' :: _ -> Some '9'
  | _ :: t -> reverse_map t
  | [] -> None
;;

let solution =
  (* Function to read the file lines *)
  let r file = In_channel.read_lines file in
  (* Read content of the file *)
  let content = r "./input/day1.txt" in
  let get_first_digit s = s |> String.to_list |> char_list_with_digit in
  let get_last s = s |> String.to_list |> List.rev |> reverse_map in
  (* Map over the content and apply the functions *)
  let first =
    List.map content ~f:(fun line ->
      match get_first_digit line with
      | Some c -> Char.to_int c - Char.to_int '0'
      | None -> failwith "no first digit")
  in
  let last =
    List.map content ~f:(fun line ->
      match get_last line with
      | Some c -> Char.to_int c - Char.to_int '0'
      | None -> failwith "no last digit")
  in
  let numbers = List.map2_exn first last ~f:(fun i j -> (i * 10) + j) in
  let answer = List.fold numbers ~init:0 ~f:(fun a b -> a + b) in
  answer
;;

let () =
  let result = solution in
  (* Call the solution to get the result *)
  print_endline (string_of_int result)
;;
