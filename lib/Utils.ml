let get_ints str =
  String.trim str |> Str.split (Str.regexp "[^0-9]+") |> List.map int_of_string
