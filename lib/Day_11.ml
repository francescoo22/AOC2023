let parse_input str = Utils.parse_char_grid str

let coords grid =
  let rec aux i j =
    if i >= Array.length grid then []
    else if j >= Array.length grid.(0) then aux (i + 1) 0
    else if grid.(i).(j) = '#' then (i, j) :: aux i (j + 1)
    else aux i (j + 1)
  in
  aux 0 0

let empty_rows grid =
  let rec aux i j =
    if i >= Array.length grid then []
    else if j >= Array.length grid.(0) then i :: aux (i + 1) 0
    else if grid.(i).(j) = '#' then aux (i + 1) 0
    else aux i (j + 1)
  in
  aux 0 0

let empty_cols grid =
  let rec aux i j =
    if j >= Array.length grid.(0) then []
    else if i >= Array.length grid then j :: aux 0 (j + 1)
    else if grid.(i).(j) = '#' then aux 0 (j + 1)
    else aux (i + 1) j
  in
  aux 0 0

let dist (x1, y1) (x2, y2) = Int.abs (x1 - x2) + Int.abs (y1 - y2)

let all_dist_sum coords =
  List.fold_left
    (fun acc coord ->
      acc
      + List.fold_left (fun acc2 coord2 -> acc2 + dist coord coord2) 0 coords)
    0 coords
  / 2

let all_rows coords = List.map (fun (a, _) -> a) coords |> List.sort compare
let all_cols coords = List.map (fun (_, a) -> a) coords |> List.sort compare

let extra_dist points holes rate =
  let rec aux points holes i sz =
    match (points, holes) with
    | p :: ps, h :: hs ->
        if p > h then ((sz - i) * i * rate) + aux (p :: ps) hs i sz
        else aux ps (h :: hs) (i + 1) sz
    | _ -> 0
  in
  aux points holes 0 (List.length points)

let ans str rate =
  let grid = parse_input str in
  let coords = coords grid in
  all_dist_sum coords
  + extra_dist (all_rows coords) (empty_rows grid) rate
  + extra_dist (all_cols coords) (empty_cols grid) rate

let part_1 str = ans str 1
let part_2 str = ans str 999999
let main input = (part_1 input, part_2 input)
