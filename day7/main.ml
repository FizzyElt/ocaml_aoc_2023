open Lib.Utils

module HandCard = struct
  module CharMap = Map.Make (Char)

  type hand_type =
    | Five
    | Four
    | FullHouse
    | Three
    | TwoPair
    | OnePair
    | HighCard

  type hand = { cards : string; hand_type : hand_type }

  let hand_order t =
    match t with
    | Five -> 0
    | Four -> 1
    | FullHouse -> 2
    | Three -> 3
    | TwoPair -> 4
    | OnePair -> 5
    | HighCard -> 6

  let card_order card =
    let index_opt =
      List.find_index (Char.equal card)
        [ 'A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J' ]
    in
    match index_opt with Some index -> index | None -> failwith "bad card"

  let collect_cards (hand_str : string) =
    let hand_chars = hand_str |> String.to_seq |> List.of_seq in
    List.fold_left
      (fun acc char ->
        match CharMap.find_opt char acc with
        | Some count -> CharMap.add char (count + 1) acc
        | None -> CharMap.add char 1 acc)
      CharMap.empty hand_chars

  let get_hand_type (cards : int CharMap.t) =
    let jokers, new_cards =
      ( CharMap.find_opt 'J' cards |> Option.value ~default:0,
        CharMap.remove 'J' cards )
    in
    let count_list = CharMap.to_list new_cards |> List.map snd in
    let five = List.exists (Int.equal 5) count_list in
    let four = List.exists (Int.equal 4) count_list in
    let three = List.exists (Int.equal 3) count_list in
    let pairs = List.filter (Int.equal 2) count_list |> List.length in

    if five then Five
    else if four && jokers = 1 then Five
    else if four then Four
    else if three && pairs = 1 then FullHouse
    else if three && jokers = 2 then Five
    else if three && jokers = 1 then Four
    else if three then Three
    else if pairs = 2 && jokers = 1 then FullHouse
    else if pairs = 2 then TwoPair
    else if pairs = 1 && jokers = 3 then Five
    else if pairs = 1 && jokers = 2 then Four
    else if pairs = 1 && jokers = 1 then Three
    else if pairs = 1 then OnePair
    else if jokers = 5 then Five (* JJJJJ *)
    else if jokers = 4 then Five (* JJJJA -> AAAAA *)
    else if jokers = 3 then Four (* JJJQA -> AAAQA *)
    else if jokers = 2 then Three (* JJQA1 -> AAQA1 *)
    else if jokers = 1 then OnePair (* J2QA1 -> A2QA1 *)
    else HighCard

  let make_hand (cards_str : string) : hand =
    let cards = collect_cards cards_str in
    let hand_type = get_hand_type cards in
    { cards = cards_str; hand_type }

  let compare_hand_type (h1 : hand_type) (h2 : hand_type) =
    Int.compare (hand_order h1) (hand_order h2)

  let compare_cards (cards1 : string) (cards2 : string) =
    let cards1 = String.to_seq cards1 |> List.of_seq in
    let cards2 = String.to_seq cards2 |> List.of_seq in
    let rec aux c1 c2 =
      match (c1, c2) with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | c1 :: c1s, c2 :: c2s -> (
          let c1 = card_order c1 in
          let c2 = card_order c2 in
          match c1 - c2 with 0 -> aux c1s c2s | n when n > 0 -> 1 | _ -> -1)
    in
    aux cards1 cards2

  let compare (h1 : hand) (h2 : hand) =
    let cmp = compare_hand_type h1.hand_type h2.hand_type in
    if cmp = 0 then compare_cards h1.cards h2.cards else cmp
end

let parse_input line =
  match String.split_on_char ' ' line with
  | cards :: bid :: _ -> (HandCard.make_hand cards, int_of_string bid)
  | _ -> failwith "bad input"

let part1_solution () =
  let hand_list =
    Sys.argv.(1)
    |> get_list_of_line parse_input
    |> List.sort (fun (h1, _) (h2, _) -> HandCard.compare h2 h1)
  in
  let result =
    hand_list |> List.mapi (fun i (_, bid) -> bid * (i + 1)) |> sum_of_list
  in

  Printf.printf "%d\n" result

let () = part1_solution ()
