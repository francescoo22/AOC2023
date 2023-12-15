let parse_input str =
  String.split_on_char ',' str |> List.map Utils.chars_of_string

let parse_input_2 str =
  String.split_on_char ',' str |> List.map @@ Str.split @@ Str.regexp "[-=]"

let hash chars =
  List.fold_left (fun acc c -> 17 * (acc + Char.code c) mod 256) 0 chars

let ans_array = Array.make 256 []

let rec remove str lst =
  match lst with
  | (x, y) :: xs -> if x = str then xs else (x, y) :: remove str xs
  | [] -> []

let rec add str amount lst =
  match lst with
  | (x, y) :: xs ->
      if x = str then (x, amount) :: xs else (x, y) :: add str amount xs
  | [] -> [ (str, amount) ]

let operation line =
  match line with
  | [ x ] ->
      Array.set ans_array
        (hash @@ Utils.chars_of_string x)
        (remove x ans_array.(hash @@ Utils.chars_of_string x))
  | [ x1; x2 ] ->
      Array.set ans_array
        (hash @@ Utils.chars_of_string x1)
        (add x1 x2 ans_array.(hash @@ Utils.chars_of_string x1))
  | _ -> raise @@ Invalid_argument "Input is not valid"

let part_1 str =
  List.fold_left (fun acc chars -> acc + hash chars) 0 @@ parse_input str

let part_2 str =
  List.iter operation @@ parse_input_2 str;
  Utils.foldij
    (fun i j acc (_, n) -> acc + ((i + 1) * (j + 1) * int_of_string n))
    0
  @@ Array.to_list ans_array

let main input = (part_1 input, part_2 input)
