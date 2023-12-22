type cell = { up : bool; down : bool; left : bool; right : bool }
type direction = Up | Down | Left | Right

let ans_grid i j =
  Array.make_matrix i j
    { up = false; down = false; left = false; right = false }

let next_dirs dir c =
  match (dir, c) with
  | _, '.' -> [ dir ]
  | Left, '-' | Right, '-' -> [ dir ]
  | Up, '|' | Down, '|' -> [ dir ]
  | Left, '|' | Right, '|' -> [ Up; Down ]
  | Up, '-' | Down, '-' -> [ Left; Right ]
  | Left, '\\' | Right, '/' -> [ Up ]
  | Up, '\\' | Down, '/' -> [ Left ]
  | Left, '/' | Right, '\\' -> [ Down ]
  | Up, '/' | Down, '\\' -> [ Right ]
  | _ -> raise @@ Invalid_argument "Input is not valid"

let diff i j = function
  | Up -> (i - 1, j)
  | Down -> (i + 1, j)
  | Left -> (i, j - 1)
  | Right -> (i, j + 1)

let is_charged cell = cell.up || cell.down || cell.left || cell.right

let valid i j grid =
  i >= 0 && j >= 0 && i < Array.length grid && j < Array.length grid.(0)

let rec move grid ans_grid dir (i, j) =
  if not (valid i j ans_grid) then ()
  else
    let cur_cell = ans_grid.(i).(j) in
    let continue =
      match dir with
      | Up ->
          if not cur_cell.up then (
            ans_grid.(i).(j) <- { cur_cell with up = true };
            true)
          else false
      | Down ->
          if not cur_cell.down then (
            ans_grid.(i).(j) <- { cur_cell with down = true };
            true)
          else false
      | Left ->
          if not cur_cell.left then (
            ans_grid.(i).(j) <- { cur_cell with left = true };
            true)
          else false
      | Right ->
          if not cur_cell.right then (
            ans_grid.(i).(j) <- { cur_cell with right = true };
            true)
          else false
    in
    if continue then
      List.iter
        (fun it -> move grid ans_grid it (diff i j it))
        (next_dirs dir grid.(i).(j))

let get_ans grid i j dir =
  let ans_grid = ans_grid (Array.length grid) (Array.length grid.(0)) in
  move grid ans_grid dir (i, j);
  Utils.foldij (fun _ _ acc it -> if is_charged it then acc + 1 else acc) 0
  @@ Array.to_list (Array.map Array.to_list ans_grid)

let part_1 str =
  let grid = Utils.parse_char_grid str in
  get_ans grid 0 0 Right

let starting_pos i j =
  List.init i (fun i -> (i, 0, Right))
  @ List.init i (fun i -> (i, j - 1, Left))
  @ List.init j (fun j -> (0, j, Down))
  @ List.init j (fun j -> (i - 1, j, Up))

let part_2 str =
  let grid = Utils.parse_char_grid str in
  let starts = starting_pos (Array.length grid) (Array.length grid.(0)) in
  let rec get_all_ans starts =
    match starts with
    | [] -> 0
    | (i, j, dir) :: starts ->
        Int.max (get_ans grid i j dir) (get_all_ans starts)
  in
  get_all_ans starts

let main input = (part_1 input, part_2 input)
