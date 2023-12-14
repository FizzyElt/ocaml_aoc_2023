open Lib.Utils

let dfs (board : char array array) start_at =
  let row_len = Array.length board in
  let col_len = Array.length board.(0) in
  let memo = Array.make_matrix row_len col_len false in
  let rec aux row col count =
    let current = board.(row).(col) in
    if current = 'S' && memo.(row).(col) then count
    else if memo.(row).(col) then 0
    else (
      memo.(row).(col) <- true;
      let res =
        match current with
        | 'S' ->
            top row col count
            |> Int.max (bottom row col count)
            |> Int.max (right row col count)
            |> Int.max (left row col count)
        | '-' -> left row col count |> Int.max (right row col count)
        | '|' -> top row col count |> Int.max (bottom row col count)
        | 'L' -> top row col count |> Int.max (right row col count)
        | 'J' -> top row col count |> Int.max (left row col count)
        | 'F' -> bottom row col count |> Int.max (right row col count)
        | '7' -> bottom row col count |> Int.max (left row col count)
        | _ -> 0
      in
      memo.(row).(col) <- false;
      res)
  and top row col count =
    if row - 1 >= 0 then
      let pipe = board.(row - 1).(col) in
      match pipe with
      | 'S' | '|' | 'F' | '7' -> aux (row - 1) col (count + 1)
      | _ -> 0
    else 0
  and bottom row col count =
    if row + 1 < row_len then
      let pipe = board.(row + 1).(col) in
      match pipe with
      | 'S' | '|' | 'J' | 'L' -> aux (row + 1) col (count + 1)
      | _ -> 0
    else 0
  and left row col count =
    if col - 1 >= 0 then
      let pipe = board.(row).(col - 1) in
      match pipe with
      | 'S' | '-' | 'F' | 'L' -> aux row (col - 1) (count + 1)
      | _ -> 0
    else 0
  and right row col count =
    if col + 1 < col_len then
      let pipe = board.(row).(col + 1) in
      match pipe with
      | 'S' | '-' | 'J' | '7' -> aux row (col + 1) (count + 1)
      | _ -> 0
    else 0
  in
  aux (fst start_at) (snd start_at) 0

let find_start (board : char array array) =
  let result = ref (0, 0) in
  Array.iteri
    (fun r row ->
      Array.iteri (fun c char -> if char = 'S' then result := (r, c)) row)
    board;
  !result

let part1_solution () =
  let matrix =
    Sys.argv.(1)
    |> get_list_of_line (compose Array.of_seq String.to_seq)
    |> Array.of_list
  in
  let start = find_start matrix in
  Printf.printf "%d, %d\n" (fst start) (snd start);
  let length = dfs matrix start in
  print_int length

let () = part1_solution ()
