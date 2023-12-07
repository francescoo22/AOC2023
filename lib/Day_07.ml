let invalid_input = Invalid_argument "Invalid input"

let parse_input str comp =
  String.split_on_char '\n' str
  |> List.map (fun s ->
         match String.split_on_char ' ' s with
         | [ x; y ] -> (x, y)
         | _ -> raise invalid_input)
  |> List.sort (fun (x, _) (y, _) -> comp x y)
  |> List.map (fun (_, it) -> int_of_string it)

let rec get_occurrences chars prev amount =
  match chars with
  | [] -> amount :: []
  | c :: cs ->
      if c = prev then get_occurrences cs c (amount + 1)
      else amount :: get_occurrences cs c 1

let get_occurrences_2 chars prev amount wildcards =
  let occ = get_occurrences chars prev amount |> List.sort (fun x y -> y - x) in
  match occ with x :: xs -> (x + wildcards) :: xs | _ -> raise invalid_input

let compute_score occurrences =
  match (List.length occurrences, occurrences) with
  | 1, _ -> 7
  | 2, c :: _ -> if c = 1 || c = 4 then 6 else 5
  | 3, [ c1; c2; c3 ] -> if c1 = 3 || c2 == 3 || c3 == 3 then 4 else 3
  | 4, _ -> 2
  | _, _ -> 1

let get_score str =
  let chars =
    String.to_seq str |> List.of_seq
    |> List.sort (fun x y -> Char.code x - Char.code y)
  in
  let c, cs =
    match chars with x :: xs -> (x, xs) | [] -> raise invalid_input
  in
  compute_score (get_occurrences cs c 1)

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
    let occurrences =
      get_occurrences_2 cs c 1 (String.length str - List.length chars)
    in
    compute_score occurrences

let map_seed wildcard_val card =
  match card with
  | 'A' -> 'e'
  | 'K' -> 'd'
  | 'Q' -> 'c'
  | 'J' -> wildcard_val
  | 'T' -> 'a'
  | _ -> card

let compare wildcard_val card1 card2 =
  if get_score card1 <> get_score card2 then get_score card1 - get_score card2
  else
    String.compare
      (String.map (map_seed wildcard_val) card1)
      (String.map (map_seed wildcard_val) card2)

let get_total_score sorted_input =
  let _, res =
    List.fold_left
      (fun (index, acc) bid -> (index + 1, acc + (bid * index)))
      (1, 0) sorted_input
  in
  res

let part_1 str = parse_input str (compare 'b') |> get_total_score
let part_2 str = parse_input str (compare '0') |> get_total_score
let main input = (part_1 input, part_2 input)
