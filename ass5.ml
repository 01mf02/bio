open List;;

(* List/vector helper functions *)

let rec take n = function
  [] -> []
| hd::tl -> if n > 0 then hd::(take (n-1) tl) else [];;

let safe_tl = function
| [] -> []
| hd::tl -> tl;;

let random_from_list l = nth l (Random.int (length l));;

let rec random_float_list min max = function
  0 -> []
| n -> (Random.float (max -. min) +. min) :: (random_float_list min max (n-1));;

let rec range_excl i j = if i >= j then [] else i :: (range_excl (i+1) j);;
let rec range_incl i j = if i >  j then [] else i :: (range_incl (i+1) j);;


(* taken from OCaml 4.00 *)
let rec mapi i f = function
| [] -> []
| a::l -> let r = f i a in r :: mapi (i + 1) f l;;
let mapi f l = mapi 0 f l;;

let map_indices f indices =
  mapi (fun i x -> if mem i indices then f i x else x);;

let lsum  = fold_left ( +. ) 0.0;;
let lprod = fold_left ( *. ) 1.0;;

let vsum  v1 v2 = map (fun (x, y) -> x +. y) (combine v1 v2);;
let vdiff v1 v2 = map (fun (x, y) -> x -. y) (combine v1 v2);;
let vdotprod v1 v2 =
  fold_left (fun a (x1, x2) -> a +. (x1 *. x2)) 0.0 (combine v1 v2);;

let vdist v1 v2 =
  let d = vdiff v1 v2 in sqrt (vdotprod d d);;


(* Optimization functions *)

let calculate_j1 n = filter (fun x -> x mod 2 = 1) (range_incl 2 n);;
let calculate_j2 n = filter (fun x -> x mod 2 = 0) (range_incl 2 n);;

let f12sum jn x x1 nf = lsum (map (fun j ->
	let jf = float_of_int j in
	let xj = float_of_int (nth x j) in
	let fraction = (3.0 *. (jf-.2.0) /. (nf-.2.0)) in
	let exponent = 0.5 *. (1.0 +. fraction) in
	(xj -. x1**exponent)**2.0) jn);;

let f12common x =
  let n  = length x in
  let nf = float_of_int n in
  let x1 = float_of_int (hd x) in
  (n, nf, x1);;

let f1 x =
  let (n, nf, x1) = f12common x in
  let j1 = calculate_j1 n in
  x1 +. (2.0 /. float_of_int (length j1)) *. f12sum j1 x x1 nf;;

let f2 x =
  let (n, nf, x1) = f12common x in
  let j2 = calculate_j2 n in
  1.0 -. sqrt x1 +. (2.0 /. float_of_int (length j2)) *. f12sum j2 x x1 nf;;


(* MOEA/D *)

let rec generate_solutions min max dim = function
  0 -> []
| n -> (random_float_list min max dim) :: generate_solutions min max dim (n-1);;

let calculate_weight_vectors2 n =
  let l = range_incl 0 n in
  let nf = float_of_int n in
  map (fun x -> let xf = float_of_int x in [xf /. nf; (nf -. xf) /. nf]) l;;

let weight_neighbors weights t =
  map (fun w1 ->
    let dists = mapi (fun i2 w2 -> (vdist w1 w2, i2)) weights in
    map snd (take t (safe_tl (sort compare dists)))) weights;;

let dominates v1 v2 =
  let c = combine v1 v2 in
  for_all (fun (x1, x2) -> x1 >= x2) c &&
  exists  (fun (x1, x2) -> x1 >  x2) c;;

let reproduce s1 s2 = s1;;

(* weighted sum approach *)
let g_ws x ws fs = lsum (map (fun (w, f) -> w *. (f x)) (combine ws fs));;

let update solutions neighbors weights (*bests*) fs ep =
  (* reproduction *)
  let k = random_from_list neighbors in
  let l = random_from_list neighbors in
  let y = reproduce (nth solutions k) (nth solutions l) in
  
  (* update bests *)
  (*let bests' = map (fun (b, f) -> max b (f y)) (combine bests fs) in*)

  (* update neighboring solutions *)
  let solutions' = map_indices (fun i s ->
    let w = nth weights i in
    if g_ws y w fs <= g_ws s w fs then y else s)
    neighbors solutions in

  let fsy = map (fun f -> f y) fs in
  let ep' = filter (fun v -> not (dominates fsy v)) ep in
  let ep'' = if exists (fun v -> dominates v fsy) ep' then ep' else fsy::ep' in
  (solutions', ep'');;

let rec update_all solutions_init neighbors weights fs ep_init =
  let rec f i solutions ep =
    if i >= length solutions then (solutions, ep)
    else
      let (solutions', ep') = update solutions (nth neighbors i) weights fs ep in
      f (succ i) solutions' ep' in

  f 0 solutions_init ep_init;;


let rec moead solutions neighbors weights fs ep = function
| 0 -> ep
| n ->
    let (solutions', ep') = update_all solutions neighbors weights fs ep in
    moead solutions' neighbors weights fs ep' (n-1);;

let _ =
  let solutions_n = 300 in
  let neighborhood_size = 2 in
  let iterations = 100 in
  let vec_dimension = 10 in

  let weights = calculate_weight_vectors2 solutions_n in
  let neighbors = weight_neighbors weights neighborhood_size in
  let initial_solutions = generate_solutions 0.0 1.0 vec_dimension solutions_n in

  let fs = [f1; f2] in

  let ep = moead initial_solutions neighbors weights fs [] iterations in

  Printf.printf "Hello world!\n";
;
