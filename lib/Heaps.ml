open ArrayUtil

(*********************************************)
(*      Inmplementation of Binary heaps      *)
(*********************************************)

(* Binary heaps as arrays *)
module Heaps (C : CompareAndPrint) = struct
  open Util
  open C

  open ArrayPrinter (C)

  type t = C.t

  (* 1. Main heap operations *)
  let parent arr i =
    if i = 0
    then (0, arr.(i))
    else (
      let j = ((i + 1) / 2) - 1 in
      (j, arr.(j)))

  let left arr i =
    let len = Array.length arr in
    let j = (2 * (i + 1)) - 1 in
    if j < len then Some (j, arr.(j)) else None

  let right arr i =
    let len = Array.length arr in
    let j = 2 * (i + 1) in
    if j < len then Some (j, arr.(j)) else None

  (* 2. Testing whether something is a heap *)
  let is_heap arr =
    let len = Array.length arr - 1 in
    let res = ref true in
    let i = ref 0 in
    while !i <= (len / 2) - 1 && !res do
      let this = arr.(!i) in
      let l = left arr !i in
      let r = right arr !i in
      (* Parent >= than left child *)
      let is_left = l = None || comp this (snd (get_exn l)) >= 0 in
      (* Parent >= than right child *)
      let is_right = l = None || comp this (snd (get_exn r)) >= 0 in
      res := !res && is_left && is_right;
      i := !i + 1
    done;
    !res

  (* The same with printing *)
  let is_heap_print ?(print = false) arr =
    let open Printf in
    let len = Array.length arr - 1 in
    let res = ref true in
    let i = ref 0 in
    while !i <= (len / 2) - 1 && !res do
      let this = arr.(!i) in
      let l = left arr !i in
      let r = right arr !i in
      let is_left = l = None || comp this (snd (get_exn l)) >= 0 in
      let is_right = r = None || comp this (snd (get_exn r)) >= 0 in
      res := !res && is_left && is_right;
      if (not !res) && print
      then (
        let li, ll = get_exn l in
        let ri, rr = get_exn r in
        printf "Out-of-order elements:\n";
        printf "Parent: (%d, %s)\n" !i (pp this);
        printf "Left: (%d, %s)\n" li (pp ll);
        printf "Right: (%d, %s)\n" ri (pp rr));
      i := !i + 1
    done;
    !res

  (* 3. Restoring the heap property for an element i *)
  let rec max_heapify heap_size arr i =
    let len = Array.length arr in
    (* assert (heap_size <= Array.length arr); *)
    if i > (len - 1) / 2 || i >= heap_size
    then ()
    else (
      let ai = arr.(i) in
      let largest = ref (i, arr.(i)) in
      let l = left arr i in
      (* Shall we swap with the left child?.. *)
      if l <> None
         && fst (get_exn l) < heap_size
         && comp (snd (get_exn l)) (snd !largest) > 0
      then largest := get_exn l;
      (* May be the right child is even bigger? *)
      let r = right arr i in
      if r <> None
         && fst (get_exn r) < heap_size
         && comp (snd (get_exn r)) (snd !largest) > 0
      then largest := get_exn r;
      if !largest <> (i, ai) (* Okay, there is a necessity to progress further... *)
      then (
        swap arr i (fst !largest);
        max_heapify heap_size arr (fst !largest)))

  (* Question: Why does max_heapify terminate? *)

  (* 4: building a heap from an array *)
  let build_max_heap arr =
    let len = Array.length arr in
    for i = (len - 1) / 2 downto 0 do
      max_heapify len arr i
    done

  (* Question: Why does the for-loop start only from
               i = (len - 1) / 2, not from i = (len - 1)/? *)

  (* 5. Heapsort *)
  let heapsort arr =
    let len = Array.length arr in
    let heap_size = ref len in
    build_max_heap arr;
    for i = len - 1 downto 1 do
      swap arr 0 i;
      heap_size := !heap_size - 1;
      max_heapify !heap_size arr 0
    done
end

module KV = struct
  type t = int * string

  let comp = key_order_asc

  let pp (k, v) = Printf.sprintf "(%d, %s)" k v
end

module KVHeaps = Heaps (KV)
