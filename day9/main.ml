open Lib.Utils

let parse_input line =
  line |> String.split_on_char ' ' |> List.map int_of_string

let next_arr arr =
  let rec aux arr res =
    match arr with
    | [] | _ :: [] -> res
    | a :: b :: tail -> aux (b :: tail) ((b - a) :: res)
  in

  List.rev (aux arr [])

let last l = List.hd (List.rev l)

let rec get_last_num arr res =
  match arr with
  | [] -> res
  | [ a ] -> a + res
  | l ->
      let last_num = last l in
      get_last_num (next_arr l) (last_num + res)

let rec get_last_num2 arr =
  match arr with
  | [] -> 0
  | [ a ] -> a
  | l ->
      let first = List.hd l in
      first - get_last_num2 (next_arr l)

let _part1_solution () =
  let list = Sys.argv.(1) |> get_list_of_line parse_input in
  let result = list |> List.map (fun l -> get_last_num l 0) |> sum_of_list in
  print_int result

let part2_solution () =
  let list = Sys.argv.(1) |> get_list_of_line parse_input in
  let result = list |> List.map (fun l -> get_last_num2 l) |> sum_of_list in
  print_int result

let () = part2_solution ()
