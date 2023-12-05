open Lib.Utils

let parse_num line =
  match String.split_on_char ' ' line with
  | a :: b :: c :: _ -> (int_of_string a, int_of_string b, int_of_string c)
  | _ -> failwith ("bad input" ^ " " ^ line)

let parse_map lines =
  let rec aux rest_lines map_list result =
    match rest_lines with
    | [] -> List.rev map_list :: result
    | line :: ll ->
        if String.ends_with ~suffix:"map:" line then
          aux ll [] (map_list :: result)
        else aux ll (parse_num line :: map_list) result
  in
  aux lines [] [] |> List.rev

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

let part1_solution () =
  let lines =
    Sys.argv.(1) |> get_list_of_line Fun.id
    |> List.filter (fun line -> line <> "")
  in
  let seeds, maps = parse_data lines in
  let result = find_minimal_location seeds maps in
  print_int result

let () = part1_solution ()
