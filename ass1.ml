open List
open Printf
open Scanf

let foldl1 f lst = match lst with 
  | [] -> raise Not_found
  | x::xs -> fold_left f x xs;;

let min_map f = function
  | [] -> raise Not_found
  | hd::tl ->
      fold_left (fun (mx, mv) x -> if f x < mv then (x, f x) else (mx, mv))
        (hd, f hd) tl

let last l = nth l (length l - 1);;
let combine_with f l1 l2 = map (fun (x, y) -> f x y) (combine l1 l2);;
let rec range_excl i j = if i >= j then [] else i :: (range_excl (i+1) j);;

let split list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
                     else aux (i-1) (h :: acc) t  in
  aux n [] list;;

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
  let dist (x1, y1) (x2, y2) =
    let dx = x2 - x1 in
    let dy = y2 - y1 in
    sqrt (float_of_int (dx * dx + dy * dy)) in
  map (fun (c1, v1) -> (c1, map (fun (c2, v2) -> (c2, dist v1 v2)) cities)) cities;;

let rec initial_solution cities dists solution =
	match solution with
	| (hi :: _) -> begin
		let rest_cities =
			filter (fun (i, _) -> i <> hi && not (mem i solution)) cities in
		match rest_cities with
		| [] -> rev solution
		| _  ->
			let min_dist = min_map (fun (i, _) -> dists hi i) rest_cities in
			initial_solution cities dists ((fst (fst min_dist)) :: solution)
		end
	| [] -> raise Not_found;;

let round_trip_dist cities dists solution =
  let dists = combine_with (fun c1 c2 -> dists c1 c2)
    solution (tl solution @ [hd solution]) in
  foldl1 (+.) dists;;


let swap l i1 i2 =
  let (i1, i2) = (min i1 i2, max i1 i2) in
  let (l1, l1') = split l i1 in
  let (l2, l3) = split l1' (i2 - i1) in
  match l2 with h2::t2 -> begin match l3 with h3::t3 ->
    l1 @ [h3] @ t2 @ [h2] @ t3
    | [] -> raise Not_found end | [] -> raise Not_found;;


let neighbor_permutations l =
  map (fun i -> swap l i ((i + 1) mod length l)) (range_excl 0 (length l));;


let rec local_search_solution cities dists solution iterations =
  if iterations <= 0 then solution
  else
    let np = neighbor_permutations (fst solution) in
    let best = min_map (fun p -> round_trip_dist cities dists p) np in
    local_search_solution cities dists best (iterations - 1);;

let _ =
  let cities = read_cities (open_in "a280.tsp") in
  let dists_table = calc_dists cities in
  let dists = (fun i1 i2 -> assoc i2 (assoc i1 dists_table)) in
  let is = initial_solution cities dists [1] in
  let rtd = round_trip_dist cities dists is in

  (* iter (fun (i, (x, y)) -> printf "City: %d %d %d\n" i x y) cities; *)
  (* iter (printf "%d\n") is ; *)
  (* printf "Distance between 18 and 133: %d\n" (dist cities 18 133);
  printf "Distance between 18 and 19 : %d\n" (dist cities 18 19); *)
  printf "Initial round-trip distance: %f\n" rtd;
  
  let lss = local_search_solution cities dists (is, rtd) 10 in
  printf "Local-search round-trip distance: %f\n" (snd lss);
;
