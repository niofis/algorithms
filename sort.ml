let rec rnds x = match x with
  |x when x = 0 -> []
  | _ -> [(Random.float 9999.)]@(rnds (x - 1))

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let rec qsort = function
  | [] -> []
  | h::t -> (qsort (List.filter (fun x -> x < h) t))@[h]@(qsort (List.filter (fun y -> y >= h) t))

let rec msort l = match l with
  [] -> []
  | _ -> 
  let rec extract s e l = let () = Printf.printf "(%i %i %i) " s e (List.length l) in match s,e with
  | s, e when s < e -> (List.nth l s)::extract (s+1) e l
  | _ -> []
  in
  let rec merge l1 l2 = match l1, l2 with
  | [], b -> b
  | a, [] -> a
  | (a::aa), (b::bb) -> (if a < b then a::(merge aa (b::bb)) else b::(merge (a::aa) bb))
  in
  let left = extract 0 ((List.length l |> float_of_int) /. 2. +. 1.|> floor |> truncate) l in
  let right = extract ((List.length l |> float_of_int) /. 2. |> ceil |> truncate) (List.length l) l in
  merge (msort left) (msort right)
