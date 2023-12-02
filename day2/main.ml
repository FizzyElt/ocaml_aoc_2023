open Lib.Utils

type cube_set = { red : int; green : int; blue : int }
type game = { id : int; set_list : cube_set list }

let red_cubes = 12
let green_cubes = 13
let blue_cubes = 14

(* "1 red" -> (1, "red") *)
let parse_cube line =
  match String.split_on_char ' ' line with
  | [ num; color ] -> Some (int_of_string num, color)
  | _ -> None

(* "1 red; 2 green; 3 blue" -> { red = 1; green = 2; blue = 3 } *)
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

let get_fewset_of_cube_set cube_sets =
  List.fold_left
    (fun acc cube_set ->
      let acc =
        if cube_set.red > acc.red then { acc with red = cube_set.red } else acc
      in
      let acc =
        if cube_set.blue > acc.blue then { acc with blue = cube_set.blue }
        else acc
      in
      let acc =
        if cube_set.green > acc.green then { acc with green = cube_set.green }
        else acc
      in
      acc)
    { red = 0; green = 0; blue = 0 }
    cube_sets

let power_of_set cube_set =
  let red_num = if cube_set.red = 0 then 1 else cube_set.red in
  let blue_num = if cube_set.blue = 0 then 1 else cube_set.blue in
  let green_num = if cube_set.green = 0 then 1 else cube_set.green in
  red_num * blue_num * green_num

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

(* part 1 solution *)
let _part1_parse = compose get_possible_game_id parse_game

(* part 2 solution *)
let part2_parse line =
  line |> parse_game |> fun g ->
  get_fewset_of_cube_set g.set_list |> power_of_set

let () =
  let sum =
    Sys.argv.(1) |> get_list_of_line part2_parse |> List.fold_left ( + ) 0
  in
  print_int sum
