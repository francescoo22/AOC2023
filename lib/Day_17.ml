open Containers

type direction = Up | Down | Left | Right
type node = { pos : int * int; dist : int; dir : direction; steps : int }

module MinHeap = Heap.Make (struct
  type t = node

  let leq x y = x.dist <= y.dist
end)

let parse_input str =
  Utils.parse_char_grid str
  |> Array.map @@ Array.map @@ fun c -> Char.code c - Char.code '0'

let is_end grid (i, j) =
  i = Array.length grid - 1 && j = Array.length grid.(0) - 1

let hash node = (node.pos, node.dir, node.steps)

let next_pos dir (i, j) =
  match dir with
  | Up -> (i - 1, j)
  | Down -> (i + 1, j)
  | Left -> (i, j - 1)
  | Right -> (i, j + 1)

let easy_dirs = function
  | Up | Down -> [ (Left, 1); (Right, 1) ]
  | Right | Left -> [ (Up, 1); (Down, 1) ]

let next_dirs_1 node =
  if node.steps < 3 then (node.dir, node.steps + 1) :: easy_dirs node.dir
  else easy_dirs node.dir

let next_dirs_2 node =
  if node.steps >= 4 then
    if node.steps < 10 then (node.dir, node.steps + 1) :: easy_dirs node.dir
    else easy_dirs node.dir
  else [ (node.dir, node.steps + 1) ]

let is_valid_index (i, j) grid =
  if i < Array.length grid && i >= 0 then j < Array.length grid.(i) && j >= 0
  else false

let visited = Hashtbl.create 7952400

let next_nodes node grid policy =
  policy node
  |> List.filter (fun (dir, _) -> is_valid_index (next_pos dir node.pos) grid)
  |> List.map (fun (dir, steps) ->
         let next_i, next_j = next_pos dir node.pos in
         {
           pos = (next_i, next_j);
           dist = node.dist + grid.(next_i).(next_j);
           dir;
           steps;
         })
  |> List.filter (fun it ->
         match Hashtbl.find_opt visited (hash it) with
         | None -> true
         | Some () -> false)

let rec dijktra grid policy heap =
  let new_heap, cur_node = MinHeap.take_exn heap in
  if is_end grid cur_node.pos then cur_node.dist
  else
    let next_nodes = next_nodes cur_node grid policy in
    List.iter (fun it -> Hashtbl.add visited (hash it) ()) next_nodes;
    dijktra grid policy @@ MinHeap.add_list new_heap next_nodes

let solve str policy =
  Hashtbl.clear visited;
  let grid = parse_input str in
  let starting_nodes =
    [
      { pos = (0, 1); dist = grid.(0).(1); dir = Right; steps = 1 };
      { pos = (1, 0); dist = grid.(1).(0); dir = Down; steps = 1 };
    ]
  in
  dijktra grid policy @@ MinHeap.of_list starting_nodes

let part_1 str = solve str next_dirs_1
let part_2 str = solve str next_dirs_2
let main input = (part_1 input, part_2 input)
