let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let () = Day1.day1

type color = 
  | Blue of int
  | Red of int
  | Green of int [@@deriving show]

type game = 
  { 
    id : int;
    rounds : color list list 
  } [@@deriving show]

let _r0 = Str.regexp {|Game [0-9]+|} 
let _r2 = Str.regexp {|[0-9]+|} 
let _r3 = Str.regexp  {| [0-9]+ blue\| [0-9]+ green\| [0-9]+ red|} 
let _r4 = Str.regexp {|blue\|green\|red|}
let _r5 = Str.regexp {|Game [0-9]+\|;|} 

let parse_color s = 
  let _ = Str.search_forward _r2 s 0 in
  let count = int_of_string (Str.matched_string s) in

  let _ = Str.search_forward _r4 s 0 in 
  let color = Str.matched_string s in 
  if (String.compare color "blue") = 0 then Blue count 
  else 
    if (String.compare color "green") = 0 then Green count 
    else Red count 

let f0 s = 
  match s with 
  | Str.Text _ -> ""
  | Str.Delim s -> s 

let parse_game s = 
  let t0 = Str.full_split _r3 s in
  let t1 = List.map f0 t0 in 
  let t2 = List.filter (fun s -> s <> "") t1 in 
  let t3 = List.filter (fun s -> s <> ",") t2 in 
  let t4 = List.filter (fun s -> s <> ":") t3 in 
  List.map parse_color t4


let parse_games s = 
  let _ = Str.search_forward _r0 s 0 in 
  let t0 = Str.matched_string s in  
  let _ = Str.search_forward _r2 t0 0 in 
  let _game_id = int_of_string (Str.matched_string t0) in
  let t0 = Str.split _r5 s in 
  {id=_game_id; rounds=(List.map parse_game t0)}
  
let count_colors (r,g,b) color =
  match color with 
  | Red c -> (r+c,g,b)
  | Green c -> (r,g+c,b)
  | Blue c -> (r,g,b+c)

let check_round r g b round = 
  let (rc,gc,bc) = List.fold_left count_colors (0,0,0) round in
  if (rc > r) || (gc > g) || (bc > b) then false else true 

let check_games r g b game = 
  let round = game.rounds in
  let b = List.map (check_round r g b) round in 
  List.fold_left (fun acc b -> acc && b) true b

let rec get_max_color (r,g,b) colors = 
  match colors with 
  | [] -> (r,g,b) 
  | (Red c)::t -> get_max_color ((max c r), g, b) t
  | (Green c)::t -> get_max_color (r, (max c g), b) t
  | (Blue c)::t -> get_max_color (r, g, max c b) t

let get_bound rounds = 
  List.fold_left get_max_color (0,0,0) rounds

let () = 
  let lines = read_lines "puzzle_inputs/day2.txt" in
  let _games = List.map parse_games lines in  
  
  (* Part 1 *)
  let t0 = List.map show_game _games in
  let _ = List.iter print_endline t0 in
  let t1 = List.filter (check_games 12 13 14) _games in 
  let id_sums = List.fold_left (fun acc g -> acc + g.id) 0 t1 in 
  let _ = print_endline (string_of_int id_sums) in 

  (* Part 2 *)
  let _rounds = List.map (fun x -> x.rounds) _games in 
  let low_bounds = List.map get_bound _rounds in
  let sum_pows = List.fold_left (fun acc (r,g,b) -> acc + (r*g*b)) 0 low_bounds in 
  print_endline (string_of_int sum_pows)







