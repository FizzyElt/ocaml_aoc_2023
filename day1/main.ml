open Lib.Utils

let digit_pairs =
  [
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9");
  ]

let digits_regexp = Str.regexp "[1-9]"

let first_digit_of_line line =
  match Str.search_forward digits_regexp line 0 with
  | exception Not_found -> (-1, "0")
  | index -> (index, Char.escaped line.[index])

let last_digit_of_line line =
  match Str.search_backward digits_regexp line (String.length line - 1) with
  | exception Not_found -> (-1, "0")
  | index -> (index, Char.escaped line.[index])

let first_letter_of_line line =
  List.fold_left
    (fun acc (letter, digit) ->
      match Str.search_forward (Str.regexp letter) line 0 with
      | exception Not_found -> acc
      | index -> (
          match fst acc with
          | i when i = -1 || i > index -> (index, digit)
          | _ -> acc))
    (-1, "0") digit_pairs

let last_letter_of_line line =
  List.fold_left
    (fun acc (letter, digit) ->
      match
        Str.search_backward (Str.regexp letter) line (String.length line - 1)
      with
      | exception Not_found -> acc
      | index -> (
          match fst acc with
          | i when i = -1 || i < index -> (index, digit)
          | _ -> acc))
    (-1, "0") digit_pairs

let get_first_digit line =
  let digit_idx, digit = first_digit_of_line line in
  let letter_idx, letter_digit = first_letter_of_line line in
  match (digit_idx, letter_idx) with
  | -1, i when i >= 0 -> letter_digit
  | i, -1 when i >= 0 -> digit
  | i, j when i < j -> digit
  | _ -> letter_digit

let get_last_digit line =
  let digit_idx, digit = last_digit_of_line line in
  let letter_idx, letter_digit = last_letter_of_line line in
  match (digit_idx, letter_idx) with
  | -1, i when i >= 0 -> letter_digit
  | i, -1 when i >= 0 -> digit
  | i, j when i > j -> digit
  | _ -> letter_digit

(* part1 solution *)
let _get_int_part1 line =
  let first_digit = snd (first_digit_of_line line) in
  let last_digit = snd (last_digit_of_line line) in
  first_digit ^ last_digit |> int_of_string

(* part2 solution *)
let get_int line =
  let first_digit = get_first_digit line in
  let last_digit = get_last_digit line in
  first_digit ^ last_digit |> int_of_string

let () =
  let sum =
    Sys.argv.(1) |> get_list_of_line get_int |> List.fold_left ( + ) 0
  in
  print_int sum
