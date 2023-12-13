open Lib.Utils

let find_need_scale_rows (matrix : char array list) =
  matrix
  |> List.map (fun row -> if Array.for_all (Char.equal '.') row then 2 else 0)

let find_need_scale_cols (matrix : char array list) =
  List.hd matrix
  |> Array.mapi (fun col _ ->
         if List.for_all (fun row -> Char.equal '.' row.(col)) matrix then 2
         else 0)

let find_scale_rows (matrix : char array list) =
  matrix
  |> List.mapi (fun i row ->
         if Array.for_all (Char.equal '.') row then i else -1)

let find_scale_cols (matrix : char array list) =
  List.hd matrix
  |> Array.mapi (fun col _ ->
         if List.for_all (fun row -> Char.equal '.' row.(col)) matrix then col
         else -1)

let scale_matrix (matrix : char array list) =
  let scale_cols = find_need_scale_cols matrix in
  let scale_rows = find_need_scale_rows matrix in

  (* scale col *)
  let matrix =
    matrix
    |> List.map (fun row ->
           let new_row = ref [||] in
           Array.iteri
             (fun i c ->
               if c > 0 then
                 new_row := Array.append !new_row (Array.make c row.(i))
               else new_row := Array.append !new_row [| row.(i) |])
             scale_cols;
           !new_row)
  in

  (* scale row *)
  let matrix =
    List.fold_left2
      (fun acc row c ->
        if c > 0 then List.init c (Fun.const row) @ acc else row :: acc)
      [] matrix scale_rows
  in
  matrix |> List.rev

let get_all_galaxy_coord matrix =
  let res = ref [] in
  List.iteri
    (fun r row ->
      List.iteri (fun c char -> if char = '#' then res := (r, c) :: !res) row)
    matrix;
  !res |> List.rev

let calc_galaxy_distance (x1, y1) (x2, y2) =
  let dx = Int.abs (x2 - x1) in
  let dy = Int.abs (y2 - y1) in
  dx + dy

let sum_of_pairs_distance list =
  let rec aux ll res =
    match ll with
    | coord :: tail ->
        let sum_of_pairs =
          List.fold_left
            (fun acc x -> acc + calc_galaxy_distance coord x)
            0 tail
        in
        aux tail (res + sum_of_pairs)
    | [] -> res
  in
  aux list 0

let sum_of_pairs_distance2 list scale_rows scale_cols =
  let find_scale_col_count x1 x2 =
    if x1 > x2 then
      scale_cols
      |> List.filter (fun c -> c >= 0 && c < x1 && c > x2)
      |> List.length
    else
      scale_cols
      |> List.filter (fun c -> c >= 0 && c < x2 && c > x1)
      |> List.length
  in
  let find_scale_row_count y1 y2 =
    if y1 > y2 then
      scale_rows
      |> List.filter (fun c -> c >= 0 && c < y1 && c > y2)
      |> List.length
    else
      scale_rows
      |> List.filter (fun c -> c >= 0 && c < y2 && c > y1)
      |> List.length
  in
  let rec aux ll res =
    match ll with
    | coord :: tail ->
        let sum_of_pairs =
          List.fold_left
            (fun acc x ->
              let x1, y1 = coord in
              let x2, y2 = x in
              let scale_row_count = find_scale_row_count x1 x2 in
              let scale_col_count = find_scale_col_count y1 y2 in
              let dx = Int.abs (x2 - x1) - scale_row_count in
              let dy = Int.abs (y2 - y1) - scale_col_count in

              acc + dx
              + (scale_col_count * 1000000)
              + dy
              + (scale_row_count * 1000000))
            0 tail
        in
        aux tail (res + sum_of_pairs)
    | [] -> res
  in
  aux list 0

let _part1_solution () =
  let matrix =
    Sys.argv.(1)
    |> get_list_of_line (compose Array.of_seq String.to_seq)
    |> scale_matrix
  in
  let galaxies = matrix |> List.map Array.to_list |> get_all_galaxy_coord in
  let total = sum_of_pairs_distance galaxies in
  print_int total

let part2_solution () =
  let matrix =
    Sys.argv.(1) |> get_list_of_line (compose Array.of_seq String.to_seq)
  in
  let scale_cols, scale_rows =
    (find_scale_cols matrix |> Array.to_list, find_scale_rows matrix)
  in
  let galaxies = matrix |> List.map Array.to_list |> get_all_galaxy_coord in
  let total = sum_of_pairs_distance2 galaxies scale_rows scale_cols in
  print_int total

let () = part2_solution ()
