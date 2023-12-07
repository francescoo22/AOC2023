let invalid_input = Invalid_argument "Invalid input"

let parse_input str =
  String.split_on_char '\n' str
  |> List.map (fun s ->
         match String.split_on_char ' ' s with
         | [ x; y ] -> (x, y)
         | _ -> raise invalid_input)

let rec get_occurences chars prev amount =
  match chars with
  | [] -> amount :: []
  | c :: cs ->
      if c = prev then get_occurences cs c (amount + 1)
      else amount :: get_occurences cs c 1

let get_occurences_2 chars prev amount wildcards =
  let occ = get_occurences chars prev amount |> List.sort (fun x y -> y - x) in
  match occ with x :: xs -> (x + wildcards) :: xs | _ -> raise invalid_input

let get_score str =
  let chars =
    String.to_seq str |> List.of_seq
    |> List.sort (fun x y -> Char.code x - Char.code y)
  in
  let c, cs =
    match chars with x :: xs -> (x, xs) | [] -> raise invalid_input
  in
  let occurences = get_occurences cs c 1 in
  match (List.length occurences, occurences) with
  | 1, _ -> 7
  | 2, c :: _ -> if c = 1 || c = 4 then 6 else 5
  | 3, c1 :: c2 :: c3 :: _ -> if c1 = 3 || c2 == 3 || c3 == 3 then 4 else 3
  | 4, _ -> 2
  | _, _ -> 1

let get_score_2 str =
  if str = "JJJJJ" then 7
  else
    let chars =
      String.to_seq str |> List.of_seq
      |> List.filter (fun it -> it <> 'J')
      |> List.sort (fun x y -> Char.code x - Char.code y)
    in
    let c, cs =
      match chars with x :: xs -> (x, xs) | [] -> raise invalid_input
    in
    let occurences =
      get_occurences_2 cs c 1 (String.length str - List.length chars)
    in
    match (List.length occurences, occurences) with
    | 1, _ -> 7
    | 2, c :: _ -> if c = 1 || c = 4 then 6 else 5
    | 3, c1 :: c2 :: c3 :: _ -> if c1 = 3 || c2 == 3 || c3 == 3 then 4 else 3
    | 4, _ -> 2
    | _, _ -> 1

let map_seed card =
  match card with
  | 'A' -> 'e'
  | 'K' -> 'd'
  | 'Q' -> 'c'
  | 'J' -> 'b'
  | 'T' -> 'a'
  | _ -> card

let map_seed_2 card =
  match card with
  | 'A' -> 'e'
  | 'K' -> 'd'
  | 'Q' -> 'c'
  | 'J' -> '0'
  | 'T' -> 'a'
  | _ -> card

let compare card1 card2 =
  if get_score card1 <> get_score card2 then get_score card1 - get_score card2
  else String.compare (String.map map_seed card1) (String.map map_seed card2)

let compare_2 card1 card2 =
  if get_score_2 card1 <> get_score_2 card2 then
    get_score_2 card1 - get_score_2 card2
  else
    String.compare (String.map map_seed_2 card1) (String.map map_seed_2 card2)

let part_1 str =
  let sorted_input =
    parse_input str
    |> List.sort (fun (x, _) (y, _) -> compare x y)
    |> List.map (fun (_, it) -> int_of_string it)
  in
  List.fold_left2
    (fun acc l1 l2 -> acc + (l1 * l2))
    0
    (List.init (List.length sorted_input) (fun it -> it + 1))
    sorted_input

let part_2 str =
  let sorted_input =
    parse_input str
    |> List.sort (fun (x, _) (y, _) -> compare_2 x y)
    |> List.map (fun (_, it) -> int_of_string it)
  in
  List.fold_left2
    (fun acc l1 l2 -> acc + (l1 * l2))
    0
    (List.init (List.length sorted_input) (fun it -> it + 1))
    sorted_input

let main input = (part_1 input, part_2 input)
