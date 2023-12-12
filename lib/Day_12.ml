let parse_input str =
  String.split_on_char '\n' str
  |> List.map (String.split_on_char ' ')
  |> List.map (function
       | [ x; y ] ->
           ( Utils.chars_of_string x,
             String.split_on_char ',' y |> List.map int_of_string )
       | _ -> raise (Invalid_argument "Input is not valid"))

let get_combinations chars ints =
  let memo = Hashtbl.create 2048 in
  let rec aux chars ints cur =
    match Hashtbl.find_opt memo (chars, ints, cur) with
    | Some x -> x
    | None ->
        let res =
          match (chars, ints) with
          | '.' :: cs, x :: xs ->
              if cur = 0 then aux cs ints cur
              else if cur = x then aux cs xs 0
              else 0
          | '.' :: cs, [] -> if cur = 0 then aux cs ints cur else 0
          | '#' :: cs, x :: _ -> if cur >= x then 0 else aux cs ints (cur + 1)
          | '#' :: _, [] -> 0
          | '?' :: cs, _ -> aux ('.' :: cs) ints cur + aux ('#' :: cs) ints cur
          | [], x :: xs -> if cur = x then aux [] xs 0 else 0
          | [], [] -> 1
          | _ -> raise (Invalid_argument "input is not valid")
        in
        Hashtbl.add memo (chars, ints, cur) res;
        res
  in
  aux chars ints 0

let part_1 str =
  let input = parse_input str in
  let rec aux input =
    match input with
    | (chars, ints) :: xs -> get_combinations chars ints + aux xs
    | _ -> 0
  in
  aux input

let part_2 str =
  let input = parse_input str in
  let rec aux input =
    match input with
    | (chars, ints) :: xs ->
        get_combinations
          (chars @ [ '?' ] @ chars @ [ '?' ] @ chars @ [ '?' ] @ chars @ [ '?' ]
         @ chars)
          (ints @ ints @ ints @ ints @ ints)
        + aux xs
    | _ -> 0
  in
  aux input

let main input = (part_1 input, part_2 input)
