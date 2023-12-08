module Network = Map.Make (String)

let invalid_input = Invalid_argument "Invalid input"

let make_map lst =
  List.fold_left
    (fun map nodes ->
      match nodes with
      | [ from; left; right ] -> Network.add from (left, right) map
      | _ -> raise invalid_input)
    Network.empty lst

let parse_input input =
  match Str.split (Str.regexp "\n\n") input with
  | [ x; y ] ->
      ( String.to_seq x |> List.of_seq,
        String.split_on_char '\n' y
        |> List.map (Str.split (Str.regexp "[^A-Z]+"))
        |> make_map,
        String.split_on_char '\n' y
        |> List.map (Str.split (Str.regexp "[^A-Z]+"))
        |> List.flatten
        |> List.filter (fun s -> Str.string_match (Str.regexp "A") s 2) )
  | _ -> raise invalid_input

let next_step map dir cur_node =
  let left, right = Network.find cur_node map in
  if dir = 'L' then left else right

let get_steps directions map to_regexp from_node =
  let rec aux dirs cur_node steps =
    if Str.string_match to_regexp cur_node 0 then steps
    else
      match dirs with
      | [] -> aux directions cur_node steps
      | c :: cs -> aux cs (next_step map c cur_node) (steps + 1)
  in
  aux directions from_node 0

let part_1 str =
  let directions, map, _ = parse_input str in
  get_steps directions map (Str.regexp "ZZZ") "AAA"

let part_2 str =
  let directions, map, start_nodes = parse_input str in
  let end_nodes =
    List.map (get_steps directions map (Str.regexp "[A-Z][A-Z]Z")) start_nodes
  in
  List.fold_left Utils.lcm 1 end_nodes * List.length directions

let main input = (part_1 input, part_2 input)
