open Lib.Utils
module SMap = Map.Make (String)

let parse_node line =
  match String.split_on_char ',' line with
  | left :: right :: _ ->
      let left_node =
        String.to_seq left
        |> Seq.filter (fun c -> c >= 'A' && c <= 'Z')
        |> String.of_seq
      in
      let right_node =
        String.to_seq right
        |> Seq.filter (fun c -> c >= 'A' && c <= 'Z')
        |> String.of_seq
      in
      (left_node, right_node)
  | _ -> failwith "bad node input"

let parse_input line =
  match String.split_on_char '=' line with
  | node :: left_right :: _ -> (String.trim node, parse_node left_right)
  | _ -> failwith "bad node input"

let create_map (nodes : (string * (string * string)) list) = SMap.of_list nodes

let next_node dir node node_map =
  match node_map |> SMap.find_opt node with
  | Some (left, right) -> (
      match dir with
      | 'L' -> Some left
      | 'R' -> Some right
      | _ -> failwith "bad direction input")
  | None -> None

let rec find_target_step target steps count node_map node =
  if target = node then count
  else
    let next_node, count =
      List.fold_left
        (fun (next, count) step ->
          match next with
          | Some n ->
              if target = n then (None, count)
              else (next_node step n node_map, count + 1)
          | None -> (None, count))
        (Some node, count) steps
    in
    match next_node with
    | Some n -> find_target_step target steps count node_map n
    | None -> count

let part1_solution () =
  let lines = Sys.argv.(1) |> get_list_of_line Fun.id in
  let steps, tail =
    match lines with
    | steps :: _ :: tail -> (steps |> String.to_seq |> List.of_seq, tail)
    | _ -> failwith "bad input"
  in
  let node_map = tail |> List.map parse_input |> create_map in
  let total = find_target_step "ZZZ" steps 0 node_map "AAA" in
  print_int total

let () = part1_solution ()
