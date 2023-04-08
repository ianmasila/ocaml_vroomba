(*********************************************)
(*        Useful functions on arrays         *)
(*********************************************)

open Util

(* Swapping two elements in an array *)
let swap (arr : 'a array) (i : int) (j : int) : unit =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp

(* Printing a sub-array of elements *)
let print_int_subarray (l : int) (u : int) (arr : int array) : unit =
  assert (l <= u);
  assert (u <= Array.length arr);
  Printf.printf "[| ";
  for i = l to u - 1 do
    Printf.printf "%d" arr.(i);
    if i < u - 1 then Printf.printf "; " else ()
  done;
  Printf.printf " |] "

(* Print the entire array *)
let print_int_array (arr : int array) : unit =
  let len = Array.length arr in
  print_int_subarray 0 len arr

(* Some examples in a nested module *)
module ArrayExamples = struct
  let a1 : int array = [|6; 8; 5; 2; 3; 7; 0|]

  (* An array of 10 zeroes *)
  let a2 : int array = Array.make 10 0

  let a3 : int array = [|5; 6; 1; 3; 3; 2; 2; 2; 15; 12|]
end

(*******************************************************)
(* Auxiliary functions for checking if array is sorted *)
(*******************************************************)

let rec sorted (ls : 'a list) : bool =
  match ls with
  | [] -> true
  | h :: t -> List.for_all (fun e -> e >= h) t && sorted t

(* Convert an array to list *)
let subarray_to_list (l : int) (u : int) (arr : 'a array) : 'a list =
  assert (l <= u);
  let res = ref [] in
  let i = ref (u - 1) in
  while l <= !i do
    res := arr.(!i) :: !res;
    i := !i - 1
  done;
  !res

(* Converting the entire array to list *)
let array_to_list (arr : 'a array) : 'a list = subarray_to_list 0 (Array.length arr) arr

(* Check if a sub-array [l...u) is sorted *)
let subarray_sorted (l : int) (u : int) (arr : 'a array) : bool =
  let ls = subarray_to_list l u arr in
  sorted ls

(* Sorting the entire array *)
let array_sorted (arr : 'a array) : bool = subarray_sorted 0 (Array.length arr) arr

(* Ensuring that two lists have same elements *)
let same_elems (ls1 : 'a list) (ls2 : 'a list) : bool =
  List.for_all
    (fun e -> List.find_all (fun e' -> e = e') ls2 = List.find_all (fun e' -> e = e') ls1)
    ls1
  && List.for_all
       (fun e ->
         List.find_all (fun e' -> e = e') ls2 = List.find_all (fun e' -> e = e') ls1)
       ls2

(* Check a minimum in a sub array *)
let is_min_subarray (l : int) (u : int) (arr : 'a array) (min : 'a) =
  let ls = subarray_to_list l u arr in
  is_min ls min

(* Generic testing procedure *)
let generic_array_sort_tester (sorter : 'a array -> unit) (a : 'a array) : bool =
  let a1 = Array.copy a in
  let ls1 = array_to_list a1 in
  sorter a1;
  let ls2 = array_to_list a1 in
  array_sorted a1 && same_elems ls1 ls2

(*********************************************)
(*     Generating random elements            *)
(*********************************************)

(* An array of random integrs within a boundary `bound` *)
let generate_keys (bound : int) (len : int) : int list =
  let acc = ref [] in
  for _i = 0 to len - 1 do
    acc := Random.int bound :: !acc
  done;
  !acc

(* A number `num` of words of a given length `length` *)
let generate_words (length : int) (num : int) : string list =
  let random_ascii_char _ =
    let rnd = Random.int 26 + 97 in
    Char.chr rnd
  in
  let random_string _ =
    let buf = Buffer.create length in
    for _i = 0 to length - 1 do
      Buffer.add_char buf (random_ascii_char ())
    done;
    Buffer.contents buf
  in
  let acc = ref [] in
  for _i = 0 to num - 1 do
    acc := random_string () :: !acc
  done;
  !acc

(* A list [0; ...; n] *)
let iota (n : int) : int list =
  let rec walk acc m = if m < 0 then acc else walk (m :: acc) (m - 1) in
  walk [] n

(* A better version of zipping two lists *)
let list_zip (ls1 : 'a list) (ls2 : 'b list) : ('a * 'b) list =
  let rec walk xs1 xs2 k =
    match (xs1, xs2) with
    | h1 :: t1, h2 :: t2 -> walk t1 t2 (fun acc -> k ((h1, h2) :: acc))
    | _ -> k []
  in
  walk ls1 ls2 (fun x -> x)

let list_to_array (ls : 'a list) : 'a array =
  match ls with
  | [] -> [||]
  | h :: _ ->
    let arr = Array.make (List.length ls) h in
    List.iteri (fun i v -> arr.(i) <- v) ls;
    arr

(*********************************************)
(*              Generating arrays            *)
(*********************************************)

(* Random array of integers *)
let generate_int_array (len : int) : int array = generate_keys len len |> list_to_array

(* Random array of strings *)
let generate_string_array (len : int) : string array =
  generate_words 5 len |> list_to_array

(* Random array of key-value pairs *)
let generate_key_value_array (len : int) : (int * string) array =
  let kvs = list_zip (generate_keys len len) (generate_words 5 len) in
  list_to_array kvs

let print_kv_array (arr : (int * string) array) (lo : int) (hi : int) : unit =
  let open Printf in
  printf "[|";
  for i = lo to hi - 1 do
    printf "(%d, %s)" (fst arr.(i)) (snd arr.(i));
    if i < hi - 1 then printf "; "
  done;
  printf "|]"

let random_sorting_test_int (sorter : int array -> unit) (len : int) : bool =
  let a = generate_int_array len in
  generic_array_sort_tester sorter a

let random_sorting_test_string (sorter : string array -> unit) (len : int) : bool =
  let a = generate_string_array len in
  generic_array_sort_tester sorter a

let random_sorting_test_kv (sorter : (int * string) array -> unit) (len : int) : bool =
  let a = generate_key_value_array len in
  generic_array_sort_tester sorter a

(***********************************************)
(*          A functor for printing arrays      *)
(***********************************************)

module ArrayPrinter =
functor
  (P : sig
     type t

     val pp : t -> string
   end)
  ->
  struct
    (* Printing machinery *)
    let print_sub_array (l : int) (u : int) (arr : P.t array) : unit =
      assert (l <= u);
      assert (u <= Array.length arr);
      Printf.printf "[| ";
      for i = l to u - 1 do
        Printf.printf "%s" (P.pp arr.(i));
        if i < u - 1 then Printf.printf "; " else ()
      done;
      Printf.printf " |] "

    let print_array (arr : P.t array) : unit =
      let len = Array.length arr in
      print_sub_array 0 len arr
  end

(***********************************************)
(*     Checking whether an array is sorted     *)
(***********************************************)

module SortChecker =
functor
  (C : sig
     type t

     val comp : t -> t -> int
   end)
  ->
  struct
    let rec sorted (ls : C.t list) : bool =
      match ls with
      | [] -> true
      | h :: t -> List.for_all (fun e -> C.comp e h >= 0) t && sorted t

    let sub_array_sorted (l : int) (u : int) (arr : C.t array) : bool =
      let ls = subarray_to_list l u arr in
      sorted ls

    let array_sorted (arr : C.t array) : bool = sub_array_sorted 0 (Array.length arr) arr

    let sorted_spec (arr1 : C.t array) (arr2 : C.t array) : bool =
      array_sorted arr2 && same_elems (array_to_list arr1) (array_to_list arr2)
  end

(***********************************************)
(*            Comparing and Printing           *)
(***********************************************)

module type CompareAndPrint = sig
  (* Type of array elements *)
  type t

  (* For comparison *)
  val comp : t -> t -> int

  (* For pretty-printing *)
  val pp : t -> string
end

(*  Sorting via keys  *)
let key_order_asc (x : 'a * 'b) (y : 'a * 'b) : int =
  if fst x < fst y
  then -1
  else if fst x = fst y
  then if snd x < snd y then -1 else if snd x > snd y then 1 else 0
  else 1
