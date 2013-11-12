open List;;

(* List helper functions *)

let rec range_excl i j = if i >= j then [] else i :: (range_excl (i+1) j);;
let rec range_incl i j = if i >  j then [] else i :: (range_incl (i+1) j);;

let foldl1 f = function
| [] -> raise Not_found
| x::xs -> fold_left f x xs;;

let lsum  = fold_left ( +. ) 0.0;;
let lprod = fold_left ( *. ) 1.0;;

let vsum  v1 v2 = map (fun (x, y) -> x +. y) (combine v1 v2);;
let vdiff v1 v2 = map (fun (x, y) -> x -. y) (combine v1 v2);;

let sprod s = map (( *. ) s);;


let random_float min max = Random.float (max -. min) +. min;;

let rec random_float_list min max = function
  0 -> []
| n -> random_float min max :: (random_float_list min max (n-1));;


(* Fitness functions *)

let griewank v =
  let xi = combine v (range_incl 1 (length v)) in
  (lsum (map (fun x -> (x**2.0) /. 4000.0) v)) -.
  (lprod (map (fun (x, i) -> cos (x /. (sqrt (float_of_int i)))) xi)) +.
  1.0;;

let fitness = griewank;;


(* Swarm functions *)

let generate_particle range_x range_v dim =
  let x = random_float_list (-. range_x) range_x dim in
  let v = random_float_list (-. range_v) range_v dim in
  let best = x in
  (x, v, best);;

let rec generate_swarm range_x range_v dim = function
  0 -> []
| n -> (generate_particle range_x range_v dim) :: 
        generate_swarm range_x range_v dim (n-1);;

let swarm_best s = foldl1 min
  (map (fun (px, pv, pbest) -> (fitness pbest, pbest)) s);;

let particle_best x1 x2 = foldl1 min
  (map (fun px -> (fitness px, px)) [x1; x2]);;

let outside_bounds bound = exists (fun x -> x > bound || x < (-. bound));;
let clip_to_bounds bound = map (fun x -> max  (min x bound)  (-. bound));;

let update_particle sbest bound (px, pv, pbest) =
  let w  = 0.4 in        (* inertia weight *)
  let c1 = 1.0 in        (* cognitive acceleration coefficient *)
  let c2 = 4.0 -. c1 in  (* social acceleration coefficient *)

  let r1 = Random.float 1.0 in
  let r2 = Random.float 1.0 in

  let new_v = foldl1 vsum [sprod w pv;
    sprod (c1 *. r1) (vdiff pbest px);
    sprod (c2 *. r2) (vdiff sbest px)] in
  let new_x = vsum px new_v in

  let new_v' =
    if outside_bounds bound new_x then sprod (-. 1.0) new_v
    else new_v in

  let new_x' = clip_to_bounds bound new_x in

  let new_best = snd (particle_best pbest new_x') in
  (new_x', new_v', new_best);;

let rec fly swarm bound = function
| 0 -> swarm
| n ->
    let sbest = snd (swarm_best swarm) in
    let updated = map (update_particle sbest bound) swarm in
    fly updated bound (n-1);;


(* Main *)

let _ =
  Random.init 0;
  let iterations = 100 in
  let dimension = 30 in
  let population = 50 in

  let range_x = 600.0 in
  let range_v = 100.0 in

  let initial = generate_swarm range_x range_v dimension population in
  let final = fly initial range_x iterations in
  let best = swarm_best final in

  Printf.printf "Iterations: %d\n" iterations;
  Printf.printf "Best value: %f\n" (fst best);
;
