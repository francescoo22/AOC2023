let parse_input str =
  Str.split (Str.regexp "\n\n") str |> List.map Utils.parse_char_grid

let check_row row grid =
  let rec aux i j =
    if i < 0 || j >= Array.length grid then true
    else if
      Utils.string_of_chars @@ Array.to_list grid.(i)
      = Utils.string_of_chars @@ Array.to_list grid.(j)
    then aux (i - 1) (j + 1)
    else false
  in
  aux row (row + 1)

let check_row_2 row grid =
  let comparison_diff xs ys =
    List.fold_left2 (fun acc x y -> if x = y then acc else acc + 1) 0 xs ys
  in
  let rec aux i j diff =
    if diff > 1 then false
    else if i < 0 || j >= Array.length grid then diff = 1
    else
      aux (i - 1) (j + 1)
        (diff
        + comparison_diff (Array.to_list grid.(i)) (Array.to_list grid.(j)))
  in
  aux row (row + 1) 0

let get_ans grid row_checker =
  let rec aux cur grid =
    if cur >= Array.length grid - 1 then -1
    else if row_checker cur grid then cur + 1
    else aux (cur + 1) grid
  in
  let row = aux 0 grid in
  if row <> -1 then row * 100 else aux 0 @@ Utils.rotate_grid grid

let part_1 str =
  List.fold_left (fun acc grid -> acc + get_ans grid check_row) 0
  @@ parse_input str

let part_2 str =
  List.fold_left (fun acc grid -> acc + get_ans grid check_row_2) 0
  @@ parse_input str

let main input = (part_1 input, part_2 input)
