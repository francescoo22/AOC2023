let get_nats str =
  Str.split (Str.regexp "[^0-9]+") str |> List.map int_of_string

let get_ints str =
  Str.split (Str.regexp "[^0-9\\-]+") str |> List.map int_of_string

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a / gcd a b * (b / gcd a b)
