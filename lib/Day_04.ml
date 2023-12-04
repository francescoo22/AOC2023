open List

let process_input str =
  String.split_on_char '\n' str
  |> map (Str.global_replace (Str.regexp {|Card[ ]+[0-9]+:[ ]+|}) "")
  |> map (Str.split (Str.regexp "[ ]+|[ ]+"))
  |> map (map (Str.split (Str.regexp {|[ ]+|})))
  |> map (map (map int_of_string))
  |> flatten
  |> map (sort Int.compare)

let rec get_card_points_aux f default xs ys =
  match (xs, ys) with
  | x :: xs, y :: ys ->
      if x == y then f (get_card_points_aux f default xs ys)
      else if x > y then get_card_points_aux f default (x :: xs) ys
      else get_card_points_aux f default xs (y :: ys)
  | _, _ -> default

let rec update_mul xs n inc =
  match (xs, n) with
  | _, 0 -> xs
  | [], _ -> []
  | x :: xs, n -> (x + inc) :: update_mul xs (n - 1) inc

let get_card_points_1 xs ys = get_card_points_aux (fun it -> 2 * it) 1 xs ys / 2
let get_card_points_2 xs ys = get_card_points_aux (fun it -> it + 1) 0 xs ys

let part_1 str =
  let rec get_all_points lst =
    match lst with
    | x :: y :: xs -> get_card_points_1 x y + get_all_points xs
    | _ -> 0
  in
  get_all_points (process_input str)

let part_2 str =
  let rec get_all_points lst mul =
    match (lst, mul) with
    | x_1 :: x_2 :: xs, y :: ys ->
        let points = get_card_points_2 x_1 x_2 in
        let new_mul = update_mul ys points y in
        (points * y) + get_all_points xs new_mul
    | _ -> 0
  in
  let input = process_input str in
  let cards_number = length input / 2 in
  get_all_points input (init cards_number (fun _ -> 1)) + cards_number

let main input = (part_1 input, part_2 input)
