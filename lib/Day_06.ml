let process_input str = String.split_on_char '\n' str |> List.map Utils.get_ints

let process_input_2 str =
  String.split_on_char '\n' str |> List.map (Str.split (Str.regexp "[^0-9]+"))

let solve_diseq a b c =
  let delta = (b *. b) -. (4.0 *. a *. c) in
  let x1 = (-.b +. sqrt delta) /. -2.0 in
  let x2 = (-.b -. sqrt delta) /. -2.0 in
  floor (x2 -. 1e-10) -. ceil (x1 +. 1e-10) +. 1.0

let part_1 str =
  let times, distances =
    match process_input str with
    | x :: y :: _ -> (List.map float_of_int x, List.map float_of_int y)
    | _ -> raise (Invalid_argument "input is not valid")
  in
  List.fold_left2
    (fun acc time dist -> acc *. solve_diseq (-1.0) time (-.dist))
    1.0 times distances
  |> int_of_float

let part_2 str =
  let time, dist =
    match process_input_2 str with
    | x :: y :: _ -> (List.fold_left ( ^ ) "" x, List.fold_left ( ^ ) "" y)
    | _ -> raise (Invalid_argument "input is not valid")
  in
  solve_diseq (-1.0)
    (float_of_int (int_of_string time))
    (-.float_of_int (int_of_string dist))
  |> int_of_float

let main input = (part_1 input, part_2 input)
