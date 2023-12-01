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
