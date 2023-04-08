(*********************************************)
(*     Useful functions and data types       *)
(*********************************************)

(* ----------  Week 01 functions  ---------- *)

(* Extract a value from an option *)

let get_exn (o : 'a option) : 'a =
  match o with
  | Some e -> e
  | _ -> failwith "Empty option!"

(*  Auxiliary functions on lists *)

let rec remove_first (ls : 'a list) (n : int) : 'a list =
  if n <= 0
  then ls
  else (
    match ls with
    | [] -> []
    | _ :: t -> remove_first t (n - 1))

let is_suffix (xs : 'a list) (ls : 'a list) : bool =
  let n1 = List.length xs in
  let n2 = List.length ls in
  let diff = n2 - n1 in
  if diff < 0
  then false
  else (
    let ls_tail = remove_first ls diff in
    ls_tail = xs)

(* ----------  Week 02 functions  ---------- *)

let is_min (ls : 'a list) (min : 'a) : bool = List.for_all (fun e -> min <= e) ls

(* ----------  Week 03 functions  ---------- *)

(* Measuring execution time *)

let time (f : 'a -> 'b) (x : 'a) : 'b =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution elapsed time: %f sec\n" (Sys.time () -. t);
  fx
