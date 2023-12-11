let get_nats str =
  Str.split (Str.regexp "[^0-9]+") str |> List.map int_of_string

let get_ints str =
  Str.split (Str.regexp "[^0-9\\-]+") str |> List.map int_of_string

let parse_char_grid str =
  String.split_on_char '\n' str
  |> List.map String.to_seq |> List.map List.of_seq |> List.map Array.of_list
  |> Array.of_list

let string_of_chars lst = String.of_seq (List.to_seq lst)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a / gcd a b * (b / gcd a b)
