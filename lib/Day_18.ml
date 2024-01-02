type direction = Up | Down | Left | Right

let direction_of_string = function
  | "R" | "0" -> Right
  | "D" | "1" -> Down
  | "L" | "2" -> Left
  | "U" | "3" -> Up
  | _ -> raise @@ Invalid_argument "Input is not valid"

let parse_hex str =
  let trimmed = Str.global_replace (Str.regexp "[()#]") "" str in
  ( int_of_string @@ "0x" ^ String.sub trimmed 0 5,
    direction_of_string @@ String.sub trimmed 5 1 )

let parse_input str =
  String.split_on_char '\n' str
  |> List.map (String.split_on_char ' ')
  |> List.map (function
       | [ dir; len; hex ] ->
           (direction_of_string dir, int_of_string len, parse_hex hex)
       | _ -> raise @@ Invalid_argument "Input is not valid")

let rec area h = function
  | [] -> 0
  | (dir, len, _) :: xs -> (
      match dir with
      | Up -> area (h + len) xs
      | Down -> area (h - len) xs
      | Left -> (len * h) + area h xs
      | Right -> (-len * h) + area h xs)

let rec perimeter = function [] -> 0 | (_, len, _) :: xs -> len + perimeter xs

let solve parsed_input =
  (Int.abs @@ area 0 parsed_input) + (perimeter parsed_input / 2) + 1

let part_1 str = solve @@ parse_input str

let part_2 str =
  solve @@ List.map (function _, _, (c, d) -> (d, c, ())) (parse_input str)

let main input = (part_1 input, part_2 input)
