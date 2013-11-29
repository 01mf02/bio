open List;;

type point = float list;;
type weight = float list;;
type population = point array;;
type weights = weight list;;


(* List/vector helper functions *)

let rec take n = function
  [] -> []
| hd::tl -> if n > 0 then hd::(take (n-1) tl) else [];;

let safe_tl = function
| [] -> []
| hd::tl -> tl;;

let random_from_list l = nth l (Random.int (length l));;

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


(* MOEA/D *)

let weight_neighbors (ws : weights) t =
  map (fun w1 ->
    let dists = mapi (fun i2 w2 -> (vdist w1 w2, i2)) ws in
    map snd (take t (safe_tl (sort compare dists)))) ws;;

let dominates v1 v2 =
  let c = combine v1 v2 in
  for_all (fun (x1, x2) -> x1 >= x2) c &&
  exists  (fun (x1, x2) -> x1 >  x2) c;;

let reproduce s1 s2 = s1;;

(* weighted sum approach *)
let g_ws x ws fs = lsum (map (fun (w, f) -> w *. (f x)) (combine ws fs));;

let update neighbors solutions weights bests fs ep =
  (* reproduction *)
  let k = random_from_list neighbors in
  let l = random_from_list neighbors in
  let y = reproduce (nth solutions k) (nth solutions l) in
  
  (* update bests *)
  let bests' = map (fun (b, f) -> max b (f y)) (combine bests fs) in

  (* update neighboring solutions *)
  let solutions' = map_indices (fun i s ->
    let w = nth weights i in
    if g_ws y w fs <= g_ws s w fs then y else s)
    neighbors solutions in

  let fsy = map (fun f -> f y) fs in
  let ep' = filter (fun v -> not (dominates fsy v)) ep in
  let ep'' = if exists (fun v -> dominates v fsy) ep' then ep' else fsy::ep' in
  ep'';;


let rec moead ep population = function
| 0 -> ep
| n ->
    moead ep population (n-1)

let _ =
  (* weight vector *)
  let wv = [] in

  let initial_population = [] in

  let ep = moead [] initial_population 10 in

  Printf.printf "Hello world!\n";
;
