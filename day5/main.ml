open Lib.Utils

let parse_num line =
  match String.split_on_char ' ' line with
  | a :: b :: c :: _ -> (int_of_string a, int_of_string b, int_of_string c)
  | _ -> failwith ("bad input" ^ " " ^ line)

let parse_map lines =
  let rec aux rest_lines map_list result =
    match rest_lines with
    | [] -> map_list :: result
    | line :: ll ->
        if String.ends_with ~suffix:"map:" line then
          aux ll [] (map_list :: result)
        else aux ll (parse_num line :: map_list) result
  in
  aux lines [] [] |> List.rev |> List.tl
  |> List.map (List.sort (fun (a, _, _) (b, _, _) -> Int.compare a b))

let parse_seeds line =
  match String.split_on_char ':' line with
  | [ _; seeds ] ->
      seeds |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  | _ -> failwith "seed parse error"

let parse_data lines =
  match lines with
  | seeds :: rest -> (parse_seeds seeds, parse_map rest)
  | _ -> failwith "parse data error"

let is_in_range num (_, s, l) = num >= s && num < s + l
let get_destin_number (d, s, _) num = num + (d - s)

let find_destination_number num (map : (int * int * int) list) =
  match List.find_opt (is_in_range num) map with
  | Some target -> get_destin_number target num
  | None -> num

let get_final_location num (maps : (int * int * int) list list) =
  List.fold_left find_destination_number num maps

let find_minimal_location seeds maps =
  List.fold_left
    (fun acc seed -> Int.min acc (get_final_location seed maps))
    Int.max_int seeds

let _part1_solution () =
  let lines =
    Sys.argv.(1) |> get_list_of_line Fun.id
    |> List.filter (fun line -> line <> "")
  in
  let seeds, maps = parse_data lines in
  let result = find_minimal_location seeds maps in
  print_int result

let seeds_to_map seeds =
  let rec aux ll result =
    match ll with
    | seed :: len :: rest -> aux rest ((seed, seed, len) :: result)
    | _ :: [] -> result
    | [] -> result
  in
  aux seeds [] |> List.sort (fun (a, _, _) (b, _, _) -> Int.compare a b)

let put_miss_range map =
  let d, _, _ = List.hd map in
  let map = if d <> 0 then (0, 0, d) :: map else map in
  let rec aux ll res =
    match ll with
    | [] -> res
    | (d, s, len) :: [] ->
        aux []
          ((d + len, d + len, Int.max_int - (d + len)) :: (d, s, len) :: res)
    | s :: l -> aux l (s :: res)
  in
  aux map [] |> List.rev

let is_overlap (s1, e1) (s2, e2) = not (e1 <= s2 || s1 >= e2)
let get_min_range (s1, e1) (s2, e2) = (Int.max s1 s2, Int.min e1 e2)

let rec find_init_range (range : int * int) (maps : (int * int * int) list list)
    =
  match maps with
  | [] -> None
  | map :: maps ->
      List.fold_left
        (fun acc (d, s, len) ->
          match acc with
          | None ->
              if is_overlap range (d, d + len) then
                let min_s, min_e = get_min_range range (d, d + len) in
                let diff = s - d in
                let new_range = (min_s + diff, min_e + diff) in
                find_init_range new_range maps
              else acc
          | some -> some)
        None map

let print_maps (maps : (int * int * int) list list) =
  List.iter
    (fun map ->
      List.iter (fun (d, s, l) -> Printf.printf "(%d, %d, %d) " d s l) map;
      print_endline "")
    maps

let part2_solution () =
  let lines =
    Sys.argv.(1) |> get_list_of_line Fun.id
    |> List.filter (fun line -> line <> "")
  in
  let seeds, maps = parse_data lines in
  let new_maps = maps |> List.map put_miss_range in
  let new_maps = seeds_to_map seeds :: new_maps |> List.rev in
  print_maps new_maps;
  let result = find_init_range (0, Int.max_int) new_maps in
  match result with
  | Some (s, e) -> Printf.printf "%d, %d" s e
  | None -> print_endline "not found"

let () = part2_solution ()
