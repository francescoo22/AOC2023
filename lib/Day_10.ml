let parse_input str =
  String.split_on_char '\n' str
  |> List.map (fun s -> String.to_seq s |> List.of_seq)

let get_links i j c =
  match c with
  | '|' -> [ (i + 1, j); (i - 1, j) ]
  | '-' -> [ (i, j - 1); (i, j + 1) ]
  | 'L' -> [ (i - 1, j); (i, j + 1) ]
  | 'J' -> [ (i - 1, j); (i, j - 1) ]
  | '7' -> [ (i + 1, j); (i, j - 1) ]
  | 'F' | 'S' -> [ (i + 1, j); (i, j + 1) ]
  | _ -> []

let start grid =
  let rec get_start i j grid =
    match grid with
    | [] -> raise (Invalid_argument "Start not found in the given grid")
    | x :: xs -> (
        match x with
        | 'S' :: _ -> (i, j)
        | _ :: ys -> get_start i (j + 1) (ys :: xs)
        | [] -> get_start (i + 1) 0 xs)
  in
  get_start 0 0 grid

let next i j grid prev =
  let candidates = get_links i j grid.(i).(j) in
  match candidates with
  | [ c1; c2 ] -> if c1 = prev then c2 else c1
  | _ -> raise (Invalid_argument "The grid is not well formed")

let get_cycle start goal grid =
  let rec aux (cur_i, cur_j) prev steps =
    let next = next cur_i cur_j grid prev in
    if next = goal then [ (cur_i, cur_j); next ]
    else (cur_i, cur_j) :: aux next (cur_i, cur_j) (steps + 1)
  in
  aux start goal 0

let get_cycle_length start goal grid = List.length @@ get_cycle start goal grid

let part_1 str =
  let input = parse_input str in
  let grid = Array.of_list @@ List.map Array.of_list input in
  let i, j = start input in
  let prev =
    match get_links i j grid.(i).(j) with
    | [ c1; _ ] -> c1
    | _ -> raise (Invalid_argument "The grid is not well formed")
  in
  (1 + get_cycle_length (i, j) prev grid) / 2

let get_tiles line =
  let add_next c1 c2 b =
    let c1 = if c1 = 'S' then 'F' else c1 in
    if (c1 = 'F' && c2 = '7') || (c1 = 'L' && c2 = 'J') then b else not b
  in
  let rec aux line add prev j =
    match line with
    | [] -> 0
    | c :: cs -> (
        match c with
        | '.' ->
            if add then 1 + aux cs add prev (j + 1) else aux cs add prev (j + 1)
        | '-' -> aux cs add prev (j + 1)
        | 'F' | 'S' | 'L' -> aux cs add c (j + 1)
        | _ -> aux cs (add_next prev c add) '.' (j + 1))
  in
  aux line false '.' 0

let map_grid grid walls =
  let res = Array.make_matrix (Array.length grid) (Array.length grid.(0)) '.' in
  let rec update walls =
    match walls with
    | (i, j) :: ws ->
        res.(i).(j) <- grid.(i).(j);
        update ws
    | [] -> ()
  in
  update walls;
  Array.to_list @@ Array.map Array.to_list res

let part_2 str =
  let input = parse_input str in
  let grid = Array.of_list @@ List.map Array.of_list input in
  let i, j = start input in
  let prev =
    match get_links i j grid.(i).(j) with
    | [ c1; _ ] -> c1
    | _ -> raise (Invalid_argument "The grid is not well formed")
  in
  let walls = get_cycle (i, j) prev grid in
  List.fold_left (fun acc line -> acc + get_tiles line) 0 (map_grid grid walls)

let main input = (part_1 input, part_2 input)
