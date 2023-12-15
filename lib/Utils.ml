let get_nats str =
  Str.split (Str.regexp "[^0-9]+") str |> List.map int_of_string

let get_ints str =
  Str.split (Str.regexp "[^0-9\\-]+") str |> List.map int_of_string

let chars_of_string str = String.to_seq str |> List.of_seq

let parse_char_grid str =
  String.split_on_char '\n' str
  |> List.map String.to_seq |> List.map List.of_seq |> List.map Array.of_list
  |> Array.of_list

let rotate_grid grid =
  match grid with
  | [||] -> [||]
  | _ ->
      let rows = Array.length grid in
      let cols = Array.length grid.(0) in
      Array.init cols (fun j -> Array.init rows (fun i -> grid.(i).(j)))

let string_of_chars lst = String.of_seq (List.to_seq lst)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a / gcd a b * (b / gcd a b)

let foldi f (acc : 'acc) lst =
  let rec aux i acc lst =
    match lst with [] -> acc | x :: xs -> aux (i + 1) (f i acc x) xs
  in
  aux 0 acc lst

let foldij f (acc : 'acc) mat =
  foldi (fun i acc lst -> foldi (fun j acc el -> f i j acc el) acc lst) acc mat
