open List;;

(* TODO *)
let distance v1 v2 = 0;;

let rec moead ep population = function
| 0 -> ep
| n -> moead ep population (n-1)

let _ =
  (* weight vector *)
  let wv = [] in
  (* weight vector distances *)
  let wvd = [] in
  (* weight vector neighbors *)
  let wvn = [] in

  let initial_population = [] in

  let ep = moead [] initial_population 10 in

  Printf.printf "Hello world!\n";
;
