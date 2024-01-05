type part = { x : int; m : int; a : int; s : int }

type ranged_part = {
  xx : int * int;
  mm : int * int;
  aa : int * int;
  ss : int * int;
}

type category = X | M | A | S
type normal_rule = { cat : category; lt : bool; num : int; target : string }
type rule = Normal of normal_rule | Last of string
type workflow = { name : string; rules : rule list }

let workflows_map = Hashtbl.create 1024

let category_of_string = function
  | "x" -> X
  | "m" -> M
  | "a" -> A
  | "s" -> S
  | _ -> raise @@ Invalid_argument "category_of_string: Input is not valid"

let parse_rule str =
  if not (String.contains str '>' || String.contains str '<') then Last str
  else
    match Str.split (Str.regexp "[<>:]") str with
    | [ cat; num; target ] ->
        Normal
          {
            cat = category_of_string cat;
            lt = String.contains str '<';
            num = int_of_string num;
            target;
          }
    | _ -> raise @@ Invalid_argument "parse_rule: Input is not valid"

let parse_rules str = String.split_on_char ',' str |> List.map parse_rule

let parse_workflows str =
  String.split_on_char '\n' str
  |> List.map (fun it ->
         match Str.split (Str.regexp "[{}]") it with
         | [ name; rules ] -> { name; rules = parse_rules rules }
         | _ -> raise @@ Invalid_argument "parse_workflows: Input is not valid")

let parse_parts str =
  String.split_on_char '\n' str
  |> List.map (fun it ->
         match Utils.get_nats it with
         | [ x; m; a; s ] -> { x; m; a; s }
         | _ -> raise @@ Invalid_argument "parse_parts: Input is not valid")

let parse_input str =
  let workflows, parts =
    match Str.split (Str.regexp "\n\n") str with
    | [ w; p ] -> (w, p)
    | _ -> raise @@ Invalid_argument "parse_input: Input is not valid"
  in
  (parse_workflows workflows, parse_parts parts)

let process_workflows workflows =
  List.iter (fun it -> Hashtbl.add workflows_map it.name it) workflows

let val_of_part part = function
  | X -> part.x
  | M -> part.m
  | A -> part.a
  | S -> part.s

let satisfied part = function
  | Last _ -> true
  | Normal rule ->
      if rule.lt then val_of_part part rule.cat < rule.num
      else val_of_part part rule.cat > rule.num

let rec accept_part workflow part =
  let rec process_part workflow =
    match workflow.rules with
    | [ Last target ] -> target
    | Normal rule :: rules ->
        if satisfied part (Normal rule) then rule.target
        else process_part { workflow with rules }
    | _ -> raise @@ Invalid_argument "accept_part: Input is not valid"
  in
  let single_process = process_part workflow in
  if single_process = "A" then true
  else if single_process = "R" then false
  else accept_part (Hashtbl.find workflows_map single_process) part

let score part =
  if accept_part (Hashtbl.find workflows_map "in") part then
    part.x + part.m + part.a + part.s
  else 0

let range_score part =
  let score (a, b) = b - a + 1 in
  score part.xx * score part.mm * score part.aa * score part.ss

let range_comp (x, y) (sub_x, sub_y) =
  if x = sub_x then (sub_y + 1, y)
  else if y = sub_y then (x, sub_x - 1)
  else raise @@ Invalid_argument "range_comp: Ranges are not valid"

let part_comp new_part part rule =
  match rule.cat with
  | X -> { part with xx = range_comp part.xx new_part.xx }
  | M -> { part with mm = range_comp part.mm new_part.mm }
  | A -> { part with aa = range_comp part.aa new_part.aa }
  | S -> { part with ss = range_comp part.ss new_part.ss }

let sub_range (x, y) lt z = if lt then (x, z - 1) else (z + 1, y)

let replace part satisfied rule =
  let new_part =
    match rule.cat with
    | X -> { part with xx = sub_range part.xx rule.lt rule.num }
    | M -> { part with mm = sub_range part.mm rule.lt rule.num }
    | A -> { part with aa = sub_range part.aa rule.lt rule.num }
    | S -> { part with ss = sub_range part.ss rule.lt rule.num }
  in
  if satisfied then new_part else part_comp new_part part rule

let is_part_valid part =
  let is_range_valid (a, b) = a <= b in
  is_range_valid part.xx && is_range_valid part.mm && is_range_valid part.aa
  && is_range_valid part.ss

let rec process_range part workflow =
  if is_part_valid part then
    match workflow.rules with
    | [ Last target ] -> (
        match target with
        | "A" -> range_score part
        | "R" -> 0
        | _ -> process_range part (Hashtbl.find workflows_map target))
    | Normal rule :: rules -> (
        match rule.target with
        | "A" ->
            (range_score @@ replace part true rule)
            + process_range (replace part false rule) { workflow with rules }
        | "R" -> process_range (replace part false rule) { workflow with rules }
        | _ ->
            process_range (replace part true rule)
              (Hashtbl.find workflows_map rule.target)
            + process_range (replace part false rule) { workflow with rules })
    | _ -> raise @@ Invalid_argument "process_range: Input is not valid"
  else 0

let part_1 str =
  let workflows, parts = parse_input str in
  process_workflows workflows;
  List.fold_left (fun acc part -> acc + score part) 0 parts

let part_2 str =
  let workflows, _ = parse_input str in
  process_workflows workflows;
  let part =
    { xx = (1, 4000); mm = (1, 4000); aa = (1, 4000); ss = (1, 4000) }
  in
  process_range part (Hashtbl.find workflows_map "in")

let main input = (part_1 input, part_2 input)
