let get_list_fo_line parse_line filename =
  let input_channel = open_in filename in
  let rec loop acc =
    match input_line input_channel with
    | exception End_of_file ->
        close_in input_channel;
        acc
    | line ->
        let item = line |> String.trim |> parse_line in
        loop (item :: acc)
  in
  List.rev (loop [])

let is_digit = function '0' .. '9' -> true | _ -> false

let get_int line =
  let char_list = String.to_seq line |> List.of_seq |> List.filter is_digit in
  if List.length char_list = 0 then 0
  else
    let first_digit, last_digit =
      (List.hd char_list, List.hd (List.rev char_list))
    in
    Char.escaped first_digit ^ Char.escaped last_digit |> int_of_string

let () =
  let filename = Sys.argv.(1) in
  let items = get_list_fo_line get_int filename in
  let sum = List.fold_left ( + ) 0 items in
  print_int sum
