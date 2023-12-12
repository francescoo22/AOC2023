let read_file file = In_channel.with_open_bin file In_channel.input_all
let input n = read_file ("test/inputs/Day_" ^ n ^ ".txt")
let args = Sys.argv
let max_day = 12

exception Invalid_day of string

let get_both_ans day =
  match day with
  | "1" -> AOC2023.Day_01.main (input "01")
  | "2" -> AOC2023.Day_02.main (input "02")
  | "3" -> AOC2023.Day_03.main (input "03")
  | "4" -> AOC2023.Day_04.main (input "04")
  | "5" -> AOC2023.Day_05.main (input "05")
  | "6" -> AOC2023.Day_06.main (input "06")
  | "7" -> AOC2023.Day_07.main (input "07")
  | "8" -> AOC2023.Day_08.main (input "08")
  | "9" -> AOC2023.Day_09.main (input "09")
  | "10" -> AOC2023.Day_10.main (input "10")
  | "11" -> AOC2023.Day_11.main (input "11")
  | "12" -> AOC2023.Day_12.main (input "12")
  (* | "13" -> AOC2023.Day_13.main (input "13") *)
  (* | "14" -> AOC2023.Day_14.main (input "14") *)
  (* | "15" -> AOC2023.Day_15.main (input "15") *)
  (* | "16" -> AOC2023.Day_16.main (input "16") *)
  (* | "17" -> AOC2023.Day_17.main (input "17") *)
  (* | "18" -> AOC2023.Day_18.main (input "18") *)
  (* | "19" -> AOC2023.Day_19.main (input "19") *)
  (* | "20" -> AOC2023.Day_20.main (input "20") *)
  (* | "21" -> AOC2023.Day_21.main (input "21") *)
  (* | "22" -> AOC2023.Day_22.main (input "22") *)
  (* | "23" -> AOC2023.Day_23.main (input "23") *)
  (* | "24" -> AOC2023.Day_24.main (input "24") *)
  (* | "25" -> AOC2023.Day_25.main (input "25") *)
  | _ ->
      raise
        (Invalid_day
           ("Aoc day is not a number between 1 and " ^ string_of_int max_day))

let print_ans day =
  let part_1, part_2 = get_both_ans day in
  Printf.printf "Day %s\nPart 1: %d\nPart 2: %d\n\n" day part_1 part_2

let () =
  if Array.length args = 1 then
    let days = List.init max_day (fun it -> string_of_int (it + 1)) in
    List.iter print_ans days
  else print_ans args.(1)
