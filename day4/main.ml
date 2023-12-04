open Lib.Utils
module StringSet = Set.Make (String)

let pow_by_two times =
  let rec aux times res =
    if times = 0 then res else aux (times - 1) (res * 2)
  in
  aux times 1

let parse_nums_list line =
  line |> String.split_on_char ' ' |> List.filter (String.equal "")

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

let get_winner_nums_count (nums, my_nums) =
  let rec aux l count =
    match l with
    | num :: l ->
        if StringSet.exists (String.equal num) nums then aux l (count + 1)
        else aux l count
    | [] -> count
  in
  aux my_nums 0

let calc_point count = if count = 0 then 0 else pow_by_two (count - 1)

let _part1_solution () =
  let result =
    Sys.argv.(1)
    |> get_list_of_line (fun line ->
           line |> parse_line |> get_winner_nums_count |> calc_point)
    |> sum_of_list
  in
  print_int result

let copy_card card_count count start_idx arr =
  let end_idx = Int.min (start_idx + count + 1) (Array.length arr) in
  let rec aux idx res =
    if idx = end_idx then res
    else
      let new_count = res.(idx) + card_count in
      res.(idx) <- new_count;
      aux (idx + 1) res
  in
  aux (start_idx + 1) arr

let get_total_cards count_arr =
  let count_length = Array.length count_arr in
  let copy_cards = ref (Array.make count_length 1) in
  for i = 0 to count_length - 1 do
    let count = count_arr.(i) in
    let card_count = !copy_cards.(i) in
    if count > 0 then copy_cards := copy_card card_count count i !copy_cards;
    ()
  done;

  !copy_cards |> sum_of_array

let part2_solution () =
  let result =
    Sys.argv.(1)
    |> get_list_of_line (fun line ->
           line |> parse_line |> get_winner_nums_count)
    |> Array.of_list |> get_total_cards
  in
  print_int result

let () = part2_solution ()
