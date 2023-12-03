let process_input str = 
  String.split_on_char '\n' str |>
  List.map String.to_seq |>
  List.map List.of_seq |>
  List.map Array.of_list |>
  Array.of_list

let is_valid_index i j grid = if i < Array.length grid && i >= 0
  then j < Array.length grid.(i) && j >= 0
  else false
let is_symbol c = not (String.contains "0123456789." c)
let is_numbeer c = String.contains "0123456789" c
let char_to_string c = String.make 1 c

let rec is_part row left right (grid : char array array) = 
  let directions = [(1, 1); (1, 0); (1, -1); (0, 1); (0, -1); (-1, 1); (-1, 0); (-1, -1)] in
  let valid col = List.fold_left 
    (fun acc (i, j) -> acc || (if is_valid_index (row + i) (col + j) grid then is_symbol (grid.(row + i).(col + j)) else false))
    false
    directions in
  if left > right then false else valid left || is_part row (left + 1) right grid

let get_row_numbers (row : char array) : (int * int * string) list =
  let rec aux start cur num = 
    if cur >= Array.length row 
      then if start <> -1 then [(start, cur - 1, num)] else []
      else if is_numbeer row.(cur) 
        then if start = -1
          then aux cur (cur + 1) (char_to_string row.(cur))
          else aux start (cur + 1) (num ^ char_to_string row.(cur))
        else if start <> -1 
          then (start, cur - 1, num) :: (aux (-1) (cur + 1) "")
          else aux start (cur + 1) num
  in aux (-1) 0 ""

let get_all_numbers (grid : char array array) = 
  let (_, res) = Array.fold_left
    (fun (i, lst) row -> (i + 1, lst @ List.map (fun (a,b,c) -> (i,a,b,c)) (get_row_numbers row)))
    (0, [])
    grid in
  res

let get_stars grid =
  let rec aux i j = 
    if i >= Array.length grid 
      then []
      else if j >= Array.length grid.(i) 
        then aux (i + 1) 0
        else if grid.(i).(j) == '*' then (i, j) :: aux i (j + 1) else aux i (j + 1) in
  aux 0 0

let get_gear_val ((star_i, star_j) : int * int) (numbers : (int * int * int * string) list) =
  let is_adj (row, left, right) = 
    if abs (star_i - row) > 1 then false else star_j >= left - 1 && star_j <= right + 1 in
  let adj = List.fold_left
    (fun acc (row, left, right, num) -> if (is_adj (row, left, right)) then (num :: acc) else acc) [] numbers in
  match adj with
  | (a :: b :: []) -> int_of_string a * int_of_string b
  | _ -> 0

let part_1 str =
  let grid = process_input str in
  let numbers = get_all_numbers grid in
  List.fold_left
    (fun acc (row, left, right, num) -> if is_part row left right grid then acc + int_of_string(num) else acc)
    0
    numbers

(* TODO: can be optimized by representig numbers as (int * int * string) list array *)
let part_2 str =
  let grid = process_input str in
  let numbers = get_all_numbers grid in
  let stars = get_stars grid in
  List.fold_left (fun acc star -> acc + get_gear_val star numbers) 0 stars


let main input = (part_1 input, part_2 input)