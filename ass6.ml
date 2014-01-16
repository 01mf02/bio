open List
open Printf
open Scanf

(*** Helper functions ***)

let foldl1 f lst = match lst with 
  | [] -> raise Not_found
  | x::xs -> fold_left f x xs;;

(* taken from OCaml 4.00 *)
let rec mapi i f = function
  [] -> []
| a::l -> let r = f i a in r :: mapi (i + 1) f l
let mapi f l = mapi 0 f l

let min_list = foldl1 min;;
let max_list = foldl1 max;;

let flsum = fold_left (+.) 0.0;;

let last = foldl1 (fun _ y -> y);;

let dist2 (x1, y1) (x2, y2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  sqrt (float_of_int (dx * dx + dy * dy));;

let symmat_get mat x y = mat.(min x y).(max x y);;
let symmat_set mat x y e = mat.(min x y).(max x y) <- e;;

let diceroll probabilities =
  let rec find_element rand sum = function
  | [] -> None
  | (i, p)::tl -> let sum' = sum +. p in
      if sum' >= rand then Some i else find_element rand sum' tl in

  let prob_sum = flsum (map snd probabilities) in
  let rand = Random.float prob_sum in
  match find_element rand 0.0 probabilities with
  | None -> fst (last probabilities)
  | Some i -> i;;


(*** Cities ***)

let rec read_cities ic =
  try
    let line = input_line ic in
    let city =
      try sscanf line " %d %d %d" (fun i x y -> [(i, (x, y))])
      with Scan_failure _ -> [] in
		city @ (read_cities ic)
  with
  | End_of_file -> close_in ic; []
  | e -> close_in_noerr ic; raise e;;

let calc_dists cities =
  map (fun (c1, v1) -> (c1,
    map (fun (c2, v2) -> (c2, dist2 v1 v2)) cities)) cities;;

let pairs_of_path path = combine path (tl path @ [hd path]);;

let round_trip_dist cities dists path =
  flsum (map (fun (c1, c2) -> dists c1 c2) (pairs_of_path path));;



(*** Parameters ***)

let ant_population = 50
and evaporation_factor = 0.8
and iterations = 200
and random_init = 1;;


(*** Ant Colony Optimization ***)

let rec ant pheromone visited = function
  [] -> rev visited
| city::[] -> rev (city :: visited)
| city::to_visit ->
    let to_visit_pheromones =
      map (fun c -> (c, symmat_get pheromone city c)) to_visit in

    let next_visited = city :: visited
    and next_city = diceroll to_visit_pheromones in
    let next_to_visit = filter (fun c -> c <> next_city) to_visit in
    ant pheromone next_visited (next_city::next_to_visit);;

let pheromone_update pheromone path delta =
  let pairs = pairs_of_path path in
  iter (fun (c1, c2) ->
    let old = symmat_get pheromone c1 c2 in
    symmat_set pheromone c1 c2 (old +. delta)) pairs;;

let calculate_min_path cities dists pheromone =
  let ants = Array.to_list (Array.make ant_population 0) in
  let paths = map (fun _ -> ant pheromone [] (map fst cities)) ants in
  let lengths = map (fun p -> (round_trip_dist cities dists p, p)) paths in
  foldl1 min lengths;;

let rec aco cities dists pheromone = function
  0 -> pheromone
| n ->
  let min_path = calculate_min_path cities dists pheromone in
  pheromone_update pheromone (snd min_path) 1.0;

  let pheromone = Array.map (Array.map (( *. ) evaporation_factor)) pheromone in

  printf "Shortest path length: %f\n" (fst min_path);
  flush stdout;

  aco cities dists pheromone (n-1);;

let print_int_list = iter (Printf.printf "%d ");;

let _ =
  Random.init random_init;

  let cities' = read_cities (open_in "att48.tsp") in
  let cities = mapi (fun i (_, v) -> (i, v)) cities' in
  let n_cities = length cities in

  let dists_table = calc_dists cities in
  let dists = (fun i1 i2 -> assoc i2 (assoc i1 dists_table)) in

  let pheromone = Array.make_matrix n_cities n_cities 1.0 in
  let pheromone = aco cities dists pheromone iterations in

  let min_path = calculate_min_path cities dists pheromone in
  print_int_list (map succ (snd min_path));
  print_newline ();
;
