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

let rec sum  = fold_left ( +. ) 0.0;;
let rec prod = fold_left ( *. ) 1.0;;

let rec take n = function
  [] -> []
| hd::tl -> if n > 0 then hd::(take (n-1) tl) else [];;

let random_from_list l = nth l (Random.int (length l));;

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let split' list n =
  let rec aux i acc = function
  | [] -> List.rev acc, []
  | h :: t as l ->
    if i = 0 then List.rev acc, l else aux (i-1) (h :: acc) t in
    aux n [] list;;

let swap l i1 i2 =
  let (i1, i2) = (min i1 i2, max i1 i2) in
  let (l1, l1') = split' l i1 in
  let (l2, l3) = split' l1' (i2 - i1) in
  match l2 with h2::t2 ->
    begin match l3 with h3::t3 ->
      l1 @ [h3] @ t2 @ [h2] @ t3
    | [] -> raise Not_found end
  | [] -> l;;

let rec replace_first (x, y) = function
| [] -> []
| hd::tl -> if hd = x then y::tl else hd::(replace_first (x, y) tl);;


(* Fitness functions *)

let fitness l = depthsum (tree_of_list l);;


(* Evolution functions *)

let rec generate_solutions l = function
  0 -> []
| n -> (shuffle l) :: generate_solutions l (n-1);;

let recombine s1 s2 =
  let elems_init = fold_left (fun acc x -> IntMap.add (Nativeint.of_int x) 0 acc) (IntMap.empty) s1 in
  let pos = Random.int (length s1) in
  let (s1a, s1b) = split' s1 pos in
  let (s2a, s2b) = split' s2 pos in
  let sc = s1a @ s2b in
  
  let elems = fold_left (fun acc x -> IntMap.add (Nativeint.of_int x) ((IntMap.find (Nativeint.of_int x) acc) + 1) acc) elems_init sc in
  let (nonex, rest) = IntMap.partition (fun k v -> v = 0) elems in
  let doubles       = IntMap.filter    (fun k v -> v = 2) rest in

  let tif = (fun x -> Nativeint.to_int (fst x)) in

  let nx = map tif (IntMap.bindings nonex) in
  let db = map tif (IntMap.bindings doubles) in
  let tt = combine db nx in
  fold_left (fun acc x -> replace_first x acc) sc tt;;

let rec orgy parents = function
  0 -> []
| n -> (recombine (random_from_list parents) (random_from_list parents))::(orgy parents (n-1));;

let rec mutate v = function
| 0 -> v
| n -> mutate (swap v (Random.int (length v)) (Random.int (length v))) (n-1);;

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
  Random.init 0;
  let l = range_incl 0 100 in
  let iterations = 50 in
  let initial = generate_solutions l 100 in
  let solutions = life initial iterations in

  Printf.printf "Iterations: %d\n" iterations;
  (* print_string "Solution vector: ";
  print_float_list (hd solutions); *)
  Printf.printf "\nDepth: %d\n" (depth (tree_of_list (hd solutions)));
;
