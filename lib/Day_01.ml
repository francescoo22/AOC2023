let rec first_digit str = match str with
| [] -> -1
| x :: xs -> 
  if Char.code x >= Char.code '0' && Char.code x <= Char.code '9' 
    then Char.code x - Char.code '0' 
    else first_digit xs

let last_digit str = first_digit (List.rev str)

let sum_int_list lst = List.fold_left (+) 0 lst

let pattern = Str.regexp {|\(one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|1\|2\|3\|4\|5\|6\|7\|8\|9\)|}

let string_to_int str = 
  if String.length str = 1 then int_of_string str else match str with
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> -1

let first_digit_2 str = 
  let _ = Str.search_forward pattern str 0 
  in string_to_int (Str.matched_string str)

let last_digit_2 str = 
  let _ = Str.search_backward pattern str (String.length str - 1)
  in string_to_int (Str.matched_string str)

let part_1 str =
  String.split_on_char '\n' str |>
  List.map String.to_seq |>
  List.map List.of_seq |>
  List.map (fun it -> first_digit it * 10 + last_digit it) |>
  sum_int_list

let part_2 str =
  String.split_on_char '\n' str |>
  List.map (fun it -> first_digit_2 it * 10 + last_digit_2 it) |>
  sum_int_list

let main input = (part_1 input, part_2 input)