open List;;

let list_elements = 15;;

(* Binary tree *)

type 'a btree = 
| Node of 'a * 'a btree * 'a btree
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

let rec rotate_cw = function
| Node (x, Node (y, yl, yr), right) -> Node (y, yl, Node (x, yr, right))
| t -> t;;

let rec rotate_ccw = function
| Node (x, left, Node (z, zl, zr)) -> Node (z, Node (x, left, zl), zr)
| t -> t;;

let rec size_of_tree = function
| Node (x, left, right) -> 1 + size_of_tree left + size_of_tree right
| Leaf -> 0;;

let rec keep_smaller_of_tree n = function
| Leaf -> Leaf
| Node (x, left, right) ->
  if x < n then Node (x, left, keep_smaller_of_tree n right)
  else keep_smaller_of_tree n left;;

let rec keep_greater_of_tree n = function
| Leaf -> Leaf
| Node (x, left, right) ->
  if x > n then Node (x, keep_greater_of_tree n left, right)
  else keep_greater_of_tree n right;;

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

let rec execute_in_tree f x = function
| Leaf -> Leaf
| Node (v, left, right) as node ->
  if v = x then f node
  else if v < x then Node (v, left, execute_in_tree f x right)
  else Node (v, execute_in_tree f x left, right);;

let rec depth = function
| Leaf -> 0
| Node (_, left, right) -> 1 + max (depth left) (depth right);;

let rec elements_at_depth d = function
| Leaf -> 0
| Node (_, left, right) ->
  if d <= 1 then 1
  else elements_at_depth (d-1) left +
       elements_at_depth (d-1) right;;

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

let fitness t = let d = depth t in (d, elements_at_depth d t);;


(* Evolution functions *)

let rec generate_solutions l = function
  0 -> []
| n -> tree_of_list (shuffle l) :: generate_solutions l (n-1);;

let rec recombine_greater = function
| Node (x, xl, xr) as tx, (Node (y, yl, yr) as ty) ->
  if x = y then Node (x, recombine_greater (xl, yl), recombine_greater (xr, yr))
  else if x < y then Node (y, recombine_greater (keep_smaller_of_tree y tx, yl), yr)
  else recombine_greater (ty, tx)
| _ -> Leaf;;

let rec recombine_smaller = function
| Node (x, xl, xr) as tx, (Node (y, yl, yr) as ty) ->
  if x = y then Node (x, recombine_smaller (xl, yl), recombine_smaller (xr, yr))
  else if x < y then Node (x, xl, recombine_smaller (keep_greater_of_tree x ty, xr))
  else recombine_smaller (ty, tx)
| _ -> Leaf;;

let recombine = if Random.bool () then recombine_greater else recombine_smaller;;

let sex m f =
  execute_in_tree (fun sm -> recombine (sm, f)) (Random.int list_elements) m;;

let rec orgy parents = function
  0 -> []
| n -> (sex (random_from_list parents) (random_from_list parents))::(orgy parents (n-1));;

let rec mutate_solution = execute_in_tree
  (if Random.bool () then rotate_ccw else rotate_cw);;

let rec mutate v = function
| 0 -> v
| n -> mutate (mutate_solution (Random.int list_elements) v) (n-1);;

let rec life elders = function
  0 -> elders
| n -> 
    let children = map (fun x -> mutate x 1) (orgy elders 100) in
    let population = elders @ children in
    let sorted = sort compare ((map (fun i -> (fitness i, i))) population) in
    let (best_f, best_i) = split (take 100 sorted) in
    Printf.printf "Best fitness: %d, %d\n" (fst (hd best_f)) (snd (hd best_f));
    flush stdout;
    life best_i (n-1);;


(* Main *)

let _ =
  Random.init 1;
  let l = range_excl 0 list_elements in
  let iterations = 100 in
  let initial = generate_solutions l 1000 in
  let solutions = life initial iterations in

  (* let t1 = tree_of_list [3; 1; 2; 4] in
  let t2 = tree_of_list [2; 1; 4; 3] in
  print_tree t1;
  print_tree t2;
  print_tree (recombine (t1, t2)); *)

  Printf.printf "Iterations: %d\n" iterations;
  print_string "Solution tree:\n";
  print_tree (hd solutions);
  Printf.printf "\nDepth: %d\n" (depth (hd solutions));
;
