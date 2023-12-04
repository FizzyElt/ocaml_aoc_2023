open Lib.Utils
module StringSet = Set.Make (String)

let pow_by_two times =
  let rec aux times res =
    if times = 0 then res else aux (times - 1) (res * 2)
  in
  aux times 1

let parse_nums_list line =
  line |> String.split_on_char ' ' |> List.filter (fun str -> str <> "")

let parse_nums_set line = line |> parse_nums_list |> StringSet.of_list

let split_nums line =
  match String.split_on_char '|' line with
  | left :: right :: _ ->
      (parse_nums_set (String.trim left), parse_nums_list (String.trim right))
  | _ -> (StringSet.empty, [])

let parse_line line =
  match String.split_on_char ':' line with
  | _ :: nums :: _ -> split_nums nums
  | _ -> (StringSet.empty, [])

let get_winner_nums_point (nums, my_nums) =
  let rec aux l count =
    match l with
    | num :: l ->
        if StringSet.exists (fun x -> x = num) nums then aux l (count + 1)
        else aux l count
    | [] -> count
  in

  let count = aux my_nums 0 in

  if count = 0 then 0 else pow_by_two (count - 1)

let () =
  let result =
    Sys.argv.(1)
    |> get_list_of_line (compose get_winner_nums_point parse_line)
    |> sum
  in
  print_int result
