let rec rnds x = match x with
  |x when x = 0 -> []
  | _ -> [(Random.float 9999.)]@(rnds (x - 1))

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let rec quick_sort = function
  | [] -> []
  | h::t -> (qsort (List.filter (fun x -> x < h) t))@[h]@(qsort (List.filter (fun y -> y >= h) t))

let rec merge_sort l =
  let rec extract s e l = match s,e with
  | s, e when s < e -> (List.nth l s)::extract (s+1) e l
  | _ -> []
  in
  let rec merge l1 l2 = match l1, l2 with
  | [], b -> b
  | a, [] -> a
  | (a::aa), (b::bb) -> (if a < b then a::(merge aa (b::bb)) else b::(merge (a::aa) bb))
  in
  if List.length l < 2 then
    l
  else
    let left = extract 0 ((List.length l + 1 |> float_of_int) /. 2.|> floor |> truncate) l in
    let right = extract ((List.length l |> float_of_int) /. 2. |> ceil |> truncate) (List.length l) l in
    merge (merge_sort left) (merge_sort right)
