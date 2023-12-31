let read_file file = In_channel.with_open_bin file In_channel.input_all
let get_input n = read_file ("test/inputs/Day_" ^ n ^ ".txt")
let args = Sys.argv
let max_day = 16

exception Invalid_day of string

let get_both_ans day input =
  let main =
    match day with
    | "1" -> AOC2023.Day_01.main
    | "2" -> AOC2023.Day_02.main
    | "3" -> AOC2023.Day_03.main
    | "4" -> AOC2023.Day_04.main
    | "5" -> AOC2023.Day_05.main
    | "6" -> AOC2023.Day_06.main
    | "7" -> AOC2023.Day_07.main
    | "8" -> AOC2023.Day_08.main
    | "9" -> AOC2023.Day_09.main
    | "10" -> AOC2023.Day_10.main
    | "11" -> AOC2023.Day_11.main
    | "12" -> AOC2023.Day_12.main
    | "13" -> AOC2023.Day_13.main
    | "14" -> AOC2023.Day_14.main
    | "15" -> AOC2023.Day_15.main
    | "16" -> AOC2023.Day_16.main
    | "17" -> AOC2023.Day_17.main
    (* | "18" -> AOC2023.Day_18.main *)
    (* | "19" -> AOC2023.Day_19.main *)
    (* | "20" -> AOC2023.Day_20.main *)
    (* | "21" -> AOC2023.Day_21.main *)
    (* | "22" -> AOC2023.Day_22.main *)
    (* | "23" -> AOC2023.Day_23.main *)
    (* | "24" -> AOC2023.Day_24.main *)
    (* | "25" -> AOC2023.Day_25.main *)
    | _ ->
        raise
          (Invalid_day
             ("Aoc day is not a number between 1 and " ^ string_of_int max_day))
  in
  main input

let print_ans day =
  let input = get_input @@ if String.length day = 1 then "0" ^ day else day in
  let start_time = Sys.time () in
  let part_1, part_2 = get_both_ans day input in
  Printf.printf "Day %s\nPart 1: %d\nPart 2: %d\nExecution time: %fs\n\n" day
    part_1 part_2
    (Sys.time () -. start_time)

let () =
  if Array.length args = 1 then
    let days = List.init max_day (fun it -> string_of_int (it + 1)) in
    List.iter print_ans days
  else print_ans args.(1)
