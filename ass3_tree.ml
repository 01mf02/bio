open List;;

module IntMap = Map.Make (Nativeint);;


(* Binary search tree *)

type 'a bstree = 
| Node of 'a * 'a bstree * 'a bstree
| Leaf

let rec insert x = function
| Leaf -> Node (x, Leaf, Leaf)
| Node (y, left, right) as node -> 
  if x < y then
    Node (y, insert x left, right)
  else if x > y then
    Node (y, left, insert x right)
  else
    node

let rec rotate_ccw = function
| Leaf -> Leaf
| Node (x, left, Node (z, zl, zr)) -> Node (z, Node (x, left, zl), zr)
| Node (x, left, Leaf) as n -> n;;

let rec remove_geq_from_tree n = function
| Leaf -> Leaf
| Node (x, left, right) ->
  if x >= n then remove_geq_from_tree n left
  else Node (x, left, remove_geq_from_tree n right);;

let print_tree tree =
  let rec print indent tree =
    match tree with
       Leaf -> ();
     | Node (n, left, right) ->
        Printf.printf "%s----\n" indent;
        print (indent ^ "| ") left;
        Printf.printf "%s%d\n" indent n;
        print (indent ^ "| ") right;
        Printf.printf "%s----\n" indent
  in
  print "" tree

let rec depth = function
| Leaf -> 0
| Node (_, left, right) -> 1 + max (depth left) (depth right);;

let depthsum =
  let rec aux d = function
  | Leaf -> d
  | Node (_, left, right) -> d + aux (d+1) left + aux (d+1) right in
  aux 0;;



let tree_of_list l = fold_left (fun t l -> insert l t) Leaf l;;


(* List helper functions *)

let rec range_excl i j = if i >= j then [] else i :: (range_excl (i+1) j);;
let rec range_incl i j = if i >  j then [] else i :: (range_incl (i+1) j);;

let rec take n = function
  [] -> []
| hd::tl -> if n > 0 then hd::(take (n-1) tl) else [];;

let random_from_list l = nth l (Random.int (length l));;

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

(* Fitness functions *)

let fitness = depthsum;;


(* Evolution functions *)

let rec generate_solutions l = function
  0 -> []
| n -> tree_of_list (shuffle l) :: generate_solutions l (n-1);;

let rec recombine = function
| Node (x, xl, xr) as tx, (Node (y, yl, yr) as ty) ->
  if x = y then Node (x, recombine (xl, yl), recombine (xr, yr))
  else if x < y then Node (y, recombine (remove_geq_from_tree y tx, yl), yr)
  else recombine (ty, tx)
| (Leaf, t) -> Leaf
| (t, Leaf) -> Leaf;;

let rec orgy parents = function
  0 -> []
| n -> (recombine (random_from_list parents, random_from_list parents))::(orgy parents (n-1));;

let rec mutate v = function
| 0 -> v
| n -> mutate (v) (n-1);;

let rec life elders = function
  0 -> elders
| n -> 
    let children = map (fun x -> mutate x 1) (orgy elders 100) in
    let population = elders @ children in
    let sorted = sort compare ((map (fun i -> (fitness i, i))) population) in
    let (best_f, best_i) = split (take 100 sorted) in
    Printf.printf "Best fitness: %d\n" (hd best_f);
    life best_i (n-1);;


(* Main *)

let _ =
  Random.init 1;
  let l = range_incl 0 10 in
  let iterations = 50 in
  let initial = generate_solutions l 100 in
  let solutions = life initial iterations in

  (* let t1 = tree_of_list [3; 1; 2; 4] in
  let t2 = tree_of_list [2; 1; 4; 3] in
  print_tree t1;
  print_tree t2;
  print_tree (recombine (t1, t2)); *)

  Printf.printf "Iterations: %d\n" iterations;
  (* print_string "Solution vector: ";
  print_float_list (hd solutions); *)
  print_tree (hd solutions);
  Printf.printf "\nDepth: %d\n" (depth (hd solutions));
;
