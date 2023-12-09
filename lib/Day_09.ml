let parse_input str = String.split_on_char '\n' str |> List.map Utils.get_ints

let get_val seq =
  let rec aux seq next_seq =
    match seq with
    | [] -> 0
    | [ x ] -> x
    | [ x_1; x_2 ] -> x_2 + aux (next_seq @ [ x_2 - x_1 ]) []
    | x_1 :: x_2 :: xs -> aux (x_2 :: xs) (next_seq @ [ x_2 - x_1 ])
  in
  aux seq []

let is_all_zero lst = List.fold_left (fun acc x -> acc && x = 0) true lst

let get_val_2 seq =
  let rec aux seq next_seq =
    match seq with
    | [] -> []
    | [ x ] -> [ x ] :: []
    | [ x_1; x_2 ] ->
        let new_seq = next_seq @ [ x_2 - x_1 ] in
        if is_all_zero new_seq then [] else new_seq :: aux new_seq []
    | x_1 :: x_2 :: xs -> aux (x_2 :: xs) (next_seq @ [ x_2 - x_1 ])
  in
  List.fold_right
    (fun x acc ->
      match x with
      | x :: _ -> x - acc
      | _ -> raise (Invalid_argument "Invalid input"))
    (seq :: aux seq []) 0

let part_1 str =
  List.fold_left (fun acc x -> acc + get_val x) 0 (parse_input str)

let part_2 str =
  List.fold_left (fun acc x -> acc + get_val_2 x) 0 (parse_input str)

let main input = (part_1 input, part_2 input)
