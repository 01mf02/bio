open List;;

(* List helper functions *)

let rec range_excl i j = if i >= j then [] else i :: (range_excl (i+1) j);;
let rec range_incl i j = if i >  j then [] else i :: (range_incl (i+1) j);;

let rec sum list = match list with
| [] -> 0.0
| head::tail -> head +. sum tail;;

let rec prod list = match list with
| [] -> 1.0
| head::tail -> head *. sum tail;;

let rec take n = function
  [] -> []
| hd::tl -> if n > 0 then hd::(take (n-1) tl) else [];;

let rec random_float_list min max = function
  0 -> []
| n -> (Random.float (max -. min) +. min) :: (random_float_list min max (n-1));;

let random_from_list l = nth l (Random.int (length l));;

let print_float_list = iter (Printf.printf "%f ");;
let print_float_list_list =
  iter (fun l -> print_float_list l; print_newline());;


(* Fitness functions *)

let fitness1 = function 
  x::y::z::[] -> x**2.0 +. (x-.y)**z +. 33.0
| _ -> raise Not_found;;

let fitness2 v =
  let xi = combine v (range_incl 1 (length v)) in
  (sum (map (fun x -> (x**2.0) /. 4000.0) v)) -.
  (prod (map (fun (x, i) -> cos (x /. (sqrt (float_of_int i)))) xi)) +.
  1.0;;


(* Evolution functions *)

let mutate solution div = map (fun x ->
  if Random.float 1.0 < 1.0 /. float_of_int (length solution) then
    x +. (Random.float (div *. 2.0)) -. div
  else x) solution;;

let rec generate_solutions min max dim = function
  0 -> []
| n -> (random_float_list min max dim) :: generate_solutions min max dim (n-1);;

let recombine s1 s2 = map (fun (x1, x2) -> (x1 +. x2) /. 2.0) (combine s1 s2);;

let rec orgy parents = function
  0 -> []
| n -> (recombine (random_from_list parents) (random_from_list parents))::(orgy parents (n-1));;

let rec life solution = function
  0 -> solution
| n -> 
    let offspring = orgy solution 100 in
    let mutated = map (fun x -> mutate x (float_of_int n)) offspring in
    let population = solution @ mutated in
    let sorted = sort compare ((map (fun i -> (fitness2 i, i))) population) in
    let (best_f, best_i) = split (take 100 sorted) in
    life best_i (n-1);;


(* Main *)

let _ =
  Random.init 0;
  let initial = generate_solutions (-. 1000.0) 1000.0 30 100 in
  let solutions = life initial 1000 in
  print_float_list_list solutions;
  Printf.printf "Fitness: %f\n" (fitness2 (hd solutions));
;
