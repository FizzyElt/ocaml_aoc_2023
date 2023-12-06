open Lib.Utils

(* print functions *)
let _print_int_list l =
  Printf.printf "%s \n" (l |> List.map string_of_int |> String.concat ", ")

(* parse functions *)
let parse_int_list_of_line line =
  line |> String.trim |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.map int_of_string

let parse_times line =
  match String.split_on_char ':' line with
  | _ :: times :: _ -> parse_int_list_of_line times
  | _ -> failwith "bad times input"

let parse_distances line =
  match String.split_on_char ':' line with
  | _ :: distances :: _ -> parse_int_list_of_line distances
  | _ -> failwith "bad distances input"

let pair_of_list l1 l2 = List.combine l1 l2

let parse_input lines =
  match lines with
  | [ a; b ] -> pair_of_list (parse_times a) (parse_distances b)
  | _ -> failwith "bad input"

let parse_input2 lines =
  match lines with
  | [ a; b ] ->
      let time =
        match String.split_on_char ':' a with
        | _ :: times :: _ ->
            times |> String.split_on_char ' '
            |> List.filter (fun x -> x <> "")
            |> String.concat "" |> int_of_string
        | _ -> failwith "bad times input"
      in
      let distance =
        match String.split_on_char ':' b with
        | _ :: distances :: _ ->
            distances |> String.split_on_char ' '
            |> List.filter (fun x -> x <> "")
            |> String.concat "" |> int_of_string
        | _ -> failwith "bad distances input"
      in
      (time, distance)
  | _ -> failwith "bad input"

(* solution functions *)
let get_distance_of_ms ms =
  List.init (ms + 1) Fun.id |> List.map (fun x -> (ms - x) * x)

let get_possible_ways ms distance =
  let rec aux ms' count =
    match ms' with
    | 0 -> count
    | ms' ->
        if (ms - ms') * ms' > distance then aux (ms' - 1) (count + 1)
        else aux (ms' - 1) count
  in
  aux ms 0

let _part1_solution () =
  let lines = Sys.argv.(1) |> get_list_of_line Fun.id in
  let pair_list = parse_input lines in
  let result =
    pair_list
    |> List.map (fun (time, distance) ->
           get_distance_of_ms time
           |> List.filter (fun x -> x > distance)
           |> List.length)
    |> product_of_list
  in
  print_int result

let part2_solution () =
  let lines = Sys.argv.(1) |> get_list_of_line Fun.id in
  let time, distance = parse_input2 lines in
  let result = get_possible_ways time distance in
  print_int result

let () = part2_solution ()
