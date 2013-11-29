open List;;

type point = float list;;
type weight = float list;;
type population = point array;;
type weights = weight list;;


(* List/vector helper functions *)

let rec take n = function
  [] -> []
| hd::tl -> if n > 0 then hd::(take (n-1) tl) else [];;

(* taken from OCaml 4.00 *)
let rec mapi i f = function
| [] -> []
| a::l -> let r = f i a in r :: mapi (i + 1) f l;;
let mapi f l = mapi 0 f l;;

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
    map snd (take t (sort compare dists))) ws;;


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
