open Lib.Utils

type cube_set = { red : int; green : int; blue : int }
type game = { id : int; set_list : cube_set list }

let red_cubes = 12
let green_cubes = 13
let blue_cubes = 14

let parse_cube line =
  match String.split_on_char ' ' line with
  | [ num; color ] -> Some (int_of_string num, color)
  | _ -> None

let parse_cube_set line =
  let parts = String.split_on_char ',' line |> List.map String.trim in
  let cube_set = { red = 0; green = 0; blue = 0 } in
  List.fold_left
    (fun acc part ->
      match parse_cube part with
      | Some (num, color) -> (
          match color with
          | "red" -> { acc with red = num }
          | "green" -> { acc with green = num }
          | "blue" -> { acc with blue = num }
          | _ -> acc)
      | None -> acc)
    cube_set parts

let parse_game_id line =
  match String.split_on_char ' ' line with
  | _ :: id :: _ -> int_of_string id
  | _ -> 0

let parse_game line =
  match String.split_on_char ':' line with
  | game :: sets :: _ ->
      let id = parse_game_id game in
      let set_list =
        sets |> String.split_on_char ';'
        |> List.map (fun s -> s |> String.trim |> parse_cube_set)
      in
      { id; set_list }
  | _ -> { id = 0; set_list = [] }

let get_possible_game_id game =
  if
    List.for_all
      (fun set ->
        set.red <= red_cubes && set.green <= green_cubes
        && set.blue <= blue_cubes)
      game.set_list
  then game.id
  else 0

let () =
  let sum =
    Sys.argv.(1)
    |> get_list_of_line (fun s -> s |> parse_game |> get_possible_game_id)
    |> List.fold_left ( + ) 0
  in
  print_int sum
