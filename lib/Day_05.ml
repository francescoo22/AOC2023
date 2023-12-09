let process_input str =
  Str.split (Str.regexp "\n\n") str |> List.map Utils.get_nats

let rec apply_single_transf seed transf =
  match transf with
  | dest :: source :: size :: xs ->
      if seed >= source && seed < source + size then seed + dest - source
      else apply_single_transf seed xs
  | _ -> seed

let rec get_final_val seed transf =
  match transf with
  | [] -> seed
  | t :: ts -> get_final_val (apply_single_transf seed t) ts

let part_1 str =
  let input = process_input str in
  let seeds = match input with x :: _ -> x | _ -> [] in
  let transf = match input with _ :: xs -> xs | _ -> [] in
  List.fold_left
    (fun acc seed -> Int.min acc (get_final_val seed transf))
    Int.max_int seeds

let rec seed_ranges xs =
  match xs with x_1 :: x_2 :: xs -> (x_1, x_2) :: seed_ranges xs | _ -> []

let intersect seed range =
  match (seed, range) with
  | (from_seed, size_seed), (dest, source, size_range) ->
      let diff = dest - source in
      let max_start = Int.max from_seed source in
      let min_end =
        Int.min (from_seed + size_seed - 1) (source + size_range - 1)
      in
      if max_start > min_end then []
      else [ (max_start + diff, min_end - max_start + 1) ]

let intersect_rem seed range =
  match (seed, range) with
  | (from_seed, size_seed), (_, source, size_range) ->
      List.filter
        (fun it -> it <> [])
        [
          intersect seed (0, 0, source);
          intersect seed
            (source + size_range, source + size_range, from_seed + size_seed);
        ]
      |> List.flatten

let rec apply_single_transf_range seeds transf =
  match transf with
  | dest :: source :: size :: xs ->
      let rem =
        List.fold_left
          (fun acc seed -> acc @ intersect_rem seed (dest, source, size))
          [] seeds
      in
      let int =
        List.fold_left
          (fun acc seed -> acc @ intersect seed (dest, source, size))
          [] seeds
      in
      int @ apply_single_transf_range rem xs
  | _ -> seeds

let min_val ranges =
  List.fold_left (fun acc (from, _) -> Int.min acc from) Int.max_int ranges

let part_2 str =
  let input = process_input str in
  let seeds = match input with x :: _ -> seed_ranges x | _ -> [] in
  let transf = match input with _ :: xs -> xs | _ -> [] in
  List.fold_left
    (fun acc trans -> apply_single_transf_range acc trans)
    seeds transf
  |> min_val

let main input = (part_1 input, part_2 input)
