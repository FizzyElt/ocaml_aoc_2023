open Lib.Utils

type board = char array array

module CharSet = Set.Make (Char)

let is_digit c = c >= '0' && c <= '9'
let chars_to_int chars = int_of_string (List.to_seq chars |> String.of_seq)

let find_symbols (row, col_start, col_end) (symbols : CharSet.t) (arr : board) =
  let count = ref 0 in
  let arr_rows = Array.length arr in
  let arr_cols = Array.length arr.(row) in
  let col_start = if col_start - 1 < 0 then 0 else col_start - 1 in
  let col_end = if col_end >= arr_cols then arr_cols - 1 else col_end in

  if row - 1 > 0 then
    for i = col_start to col_end do
      if CharSet.exists (fun x -> x = arr.(row - 1).(i)) symbols then
        count := !count + 1
    done;
  if row + 1 < arr_rows then
    for i = col_start to col_end do
      if CharSet.exists (fun x -> x = arr.(row + 1).(i)) symbols then
        count := !count + 1
    done;
  for i = col_start to col_end do
    if CharSet.exists (fun x -> x = arr.(row).(i)) symbols then
      count := !count + 1
  done;
  !count

let get_total_row_of_numbers row symbols (arr : board) =
  let total = ref 0 in
  let acc = ref [] in
  let start = ref (-1) in
  let arr_cols = Array.length arr.(row) in

  for col = 0 to arr_cols - 1 do
    let c = arr.(row).(col) in
    match (is_digit c, !acc) with
    | false, [] -> ()
    | true, _ ->
        if !start < 0 then start := col;
        acc := c :: !acc;
        ()
    | false, chars ->
        let num = chars_to_int (List.rev chars) in
        let symbol_count = find_symbols (row, !start, col) symbols arr in
        total := (num * symbol_count) + !total;
        acc := [];
        start := -1;
        ()
  done;
  (if List.length !acc > 0 && !start >= 0 then
     let num = chars_to_int (List.rev !acc) in
     let symbol_count = find_symbols (row, !start, arr_cols) symbols arr in
     total := (num * symbol_count) + !total);
  !total

let _get_total_of_numbers symbols (arr : board) =
  let total = ref 0 in
  for row = 0 to Array.length arr - 1 do
    total := !total + get_total_row_of_numbers row symbols arr
  done;
  !total

let line_to_chars line = line |> String.to_seq |> Array.of_seq

let _get_symbols (arr : board) =
  let symbols = ref CharSet.empty in
  arr
  |> Array.iter (fun row ->
         row
         |> Array.iter (fun c ->
                if (not (is_digit c)) && c <> '.' then
                  symbols := CharSet.add c !symbols;
                ()));
  !symbols

let get_range_of_numbers (arr : char array) (start_idx, end_idx) =
  let rec aux idx acc result =
    if (idx >= end_idx || idx >= Array.length arr) && List.length acc > 0 then
      chars_to_int (List.rev acc) :: result
    else if idx >= end_idx || idx >= Array.length arr then result
    else
      let c = arr.(idx) in
      match (is_digit c, acc) with
      | true, _ -> aux (idx + 1) (c :: acc) result
      | false, [] -> aux (idx + 1) acc result
      | false, chars ->
          aux (idx + 1) [] (chars_to_int (List.rev chars) :: result)
  in
  aux start_idx [] []

let get_range_by_index idx (arr : char array) =
  let rec find_left idx =
    if idx < 0 then 0
    else if not (is_digit arr.(idx)) then idx + 1
    else find_left (idx - 1)
  in
  let rec find_right idx =
    if idx >= Array.length arr then Array.length arr
    else if not (is_digit arr.(idx)) then idx
    else find_right (idx + 1)
  in
  (find_left idx, find_right idx)

let product = List.fold_left ( * ) 1

let get_max_range idx arr =
  let range = ref None in

  (if is_digit arr.(idx) then
     let start_idx', end_idx' = get_range_by_index idx arr in
     match !range with
     | Some (start_idx, end_idx) ->
         range := Some (Int.min start_idx start_idx', Int.max end_idx end_idx')
     | None -> range := Some (start_idx', end_idx'));

  (if idx - 1 >= 0 && is_digit arr.(idx - 1) then
     let start_idx', end_idx' = get_range_by_index (idx - 1) arr in
     match !range with
     | Some (start_idx, end_idx) ->
         range := Some (Int.min start_idx start_idx', Int.max end_idx end_idx')
     | None -> range := Some (start_idx', end_idx'));

  (if idx + 1 < Array.length arr && is_digit arr.(idx + 1) then
     let start_idx', end_idx' = get_range_by_index (idx + 1) arr in
     match !range with
     | Some (start_idx, end_idx) ->
         range := Some (Int.min start_idx start_idx', Int.max end_idx end_idx')
     | None -> range := Some (start_idx', end_idx'));
  !range

let get_gear_product (arr : board) =
  let total = ref 0 in
  let arr_rows = Array.length arr in
  Array.iteri
    (fun row row_arr ->
      Array.iteri
        (fun col c ->
          if c = '*' then (
            let num_list = ref [] in
            if row - 1 >= 0 then
              num_list :=
                List.append !num_list
                  ( get_max_range col arr.(row - 1) |> fun range_option ->
                    match range_option with
                    | Some range -> get_range_of_numbers arr.(row - 1) range
                    | None -> [] );
            if row + 1 < arr_rows then
              num_list :=
                List.append !num_list
                  ( get_max_range col arr.(row + 1) |> fun range_option ->
                    match range_option with
                    | Some range -> get_range_of_numbers arr.(row + 1) range
                    | None -> [] );
            if List.length !num_list > 0 then
              total := !total + product !num_list;
            ()))
        row_arr)
    arr;
  !total

let () =
  let board = Sys.argv.(1) |> get_list_of_line line_to_chars |> Array.of_list in
  let sum = get_gear_product board in
  print_int sum
