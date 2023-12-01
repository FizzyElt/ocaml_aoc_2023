open Lib.Utils

let is_digit = function '0' .. '9' -> true | _ -> false

let get_int line =
  let char_list = String.to_seq line |> List.of_seq |> List.filter is_digit in
  if List.length char_list = 0 then 0
  else
    let first_digit, last_digit =
      (List.hd char_list, List.hd (List.rev char_list))
    in
    Char.escaped first_digit ^ Char.escaped last_digit |> int_of_string

let () =
  let sum =
    Sys.argv.(1) |> get_list_of_line get_int |> List.fold_left ( + ) 0
  in
  print_int sum
