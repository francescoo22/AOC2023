let move_one grid (mi, mj) i j =
  let valid i j grid =
    i >= 0 && j >= 0
    && i < Array.length grid
    && j < Array.length grid.(0)
    && grid.(i).(j) = '.'
  in
  let rec aux dir i j =
    match (grid.(i).(j), dir) with
    | 'O', (mi, mj) ->
        if valid (i + mi) (j + mj) grid then (
          grid.(i).(j) <- '.';
          grid.(i + mi).(j + mj) <- 'O';
          aux dir (i + mi) (j + mj))
    | _ -> ()
  in
  if mi + mj = -1 then aux (mi, mj) i j
  else aux (mi, mj) (Array.length grid - i - 1) (Array.length grid.(0) - j - 1)

let move_all grid dir =
  Array.iteri
    (fun i line -> Array.iteri (fun j _ -> move_one grid dir i j) line)
    grid

let cylce grid =
  let dirs = [ (-1, 0); (0, -1); (1, 0); (0, 1) ] in
  List.iter (move_all grid) dirs

let map = Hashtbl.create 512
let cycles = 1000000000

let add_entry grid i =
  Hashtbl.add map (List.map Array.to_list (Array.to_list grid)) i

let find_entry grid =
  Hashtbl.find_opt map (List.map Array.to_list (Array.to_list grid))

let score grid =
  let (_, res) = 
  Array.fold_left
    (fun (i, acc) line ->
      ( i - 1,
        acc + Array.fold_left (fun acc c -> if c = 'O' then acc + i else acc) 0 line
      ))
    (Array.length grid, 0) grid in res

let part_1 str =
  let grid = Utils.parse_char_grid str in
  move_all grid (-1, 0);
  score grid

let part_2 str =
  let grid = Utils.parse_char_grid str in
  let rec aux i =
    match find_entry grid with
    | None ->
        add_entry grid i;
        cylce grid;
        aux (i + 1)
    | Some x ->
        let cylce_length = i - x in
        x + ((cycles - x) / cylce_length * cylce_length)
  in
  let rec ans i =
    if i < cycles then (
      cylce grid;
      ans (i + 1))
  in
  ans @@ aux 0;
  score grid

let main input = (part_1 input, part_2 input)
