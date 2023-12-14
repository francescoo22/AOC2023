let parse_input str =
  Str.split (Str.regexp "\n\n") str |> List.map Utils.parse_char_grid

let check_row max_diff row grid =
  let comparison_diff xs ys =
    List.fold_left2 (fun acc x y -> if x = y then acc else acc + 1) 0 xs ys
  in
  let rec aux i j diff =
    if diff > max_diff then false
    else if i < 0 || j >= Array.length grid then diff = max_diff
    else
      aux (i - 1) (j + 1)
        (diff
        + comparison_diff (Array.to_list grid.(i)) (Array.to_list grid.(j)))
  in
  aux row (row + 1) 0

let get_ans grid max_diff =
  let rec aux cur grid =
    if cur >= Array.length grid - 1 then -1
    else if check_row max_diff cur grid then cur + 1
    else aux (cur + 1) grid
  in
  let row = aux 0 grid in
  if row <> -1 then row * 100 else aux 0 @@ Utils.rotate_grid grid

let part_1 str =
  List.fold_left (fun acc grid -> acc + get_ans grid 0) 0 @@ parse_input str

let part_2 str =
  List.fold_left (fun acc grid -> acc + get_ans grid 1) 0 @@ parse_input str

let main input = (part_1 input, part_2 input)
