let read_file file = In_channel.with_open_bin file In_channel.input_all
let input n = read_file ("test/inputs/Day_" ^ n ^ ".txt")
let ans1, ans2 = AOC2023.Day_01.main (input "01")
let () = Printf.printf "(%d %d)\n" ans1 ans2
let ans1, ans2 = AOC2023.Day_02.main (input "02")
let () = Printf.printf "(%d %d)\n" ans1 ans2
let ans1, ans2 = AOC2023.Day_03.main (input "03")
let () = Printf.printf "(%d %d)\n" ans1 ans2
