open Str

let remove_game str = global_replace (regexp {|Game [0-9]+: |}) "" str

let split_num_obj str =
  match split (regexp " ") str with
  | [ x; y ] -> (int_of_string x, y)
  | _ -> raise (Failure ("invalid argument: " ^ str))

let object_list str =
  remove_game str |> String.split_on_char '\n'
  |> List.map (split (regexp "; "))
  |> List.map (List.map (split (regexp ", ")))
  |> List.map List.flatten
  |> List.map (List.map split_num_obj)

let valid (num, col) =
  match col with
  | "red" -> num <= 12
  | "green" -> num <= 13
  | "blue" -> num <= 14
  | _ -> raise (Failure ("invalid argument: " ^ col))

let rec valid_seq objects =
  match objects with [] -> true | x :: xs -> valid x && valid_seq xs

let sequence_val objects =
  let partial_result (red, blue, green) (num, col) =
    match col with
    | "red" -> (max red num, blue, green)
    | "blue" -> (red, max blue num, green)
    | "green" -> (red, blue, max green num)
    | _ -> raise (Failure ("invalid argument: " ^ col))
  in
  List.fold_left partial_result (0, 0, 0) objects

let part_1 str =
  let partial_result (acc_ind, acc_res) it =
    if valid_seq it then (acc_ind + 1, acc_res + acc_ind)
    else (acc_ind + 1, acc_res)
  in
  let _, result = List.fold_left partial_result (1, 0) (object_list str) in
  result

let part_2 str =
  let prod (r, b, g) = r * g * b in
  List.fold_left
    (fun acc objs -> acc + prod (sequence_val objs))
    0 (object_list str)

let main input = (part_1 input, part_2 input)
