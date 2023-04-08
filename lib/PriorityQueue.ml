open ArrayUtil
open Heaps

module PriorityQueue (C : CompareAndPrint) = struct
  (* Need to lift C to options *)
  module COpt = struct
    type t = C.t option

    let comp x y =
      match (x, y) with
      | Some a, Some b -> C.comp a b
      | None, Some _ -> -1
      | Some _, None -> 1
      | None, None -> 0

    let pp x =
      match x with
      | Some x -> C.pp x
      | None -> "None"
  end

  module H = Heaps (COpt)

  (* Do not inline, just include *)
  open H

  type heap = {
    heap_size : int ref;
    arr : H.t array;
  }

  let mk_empty_queue size =
    assert (size >= 0);
    { heap_size = ref 0; arr = Array.make size None }

  (* Make a priority queue from an array *)
  let mk_queue a =
    let ls = List.map (fun e -> Some e) (Array.to_list a) in
    let a' = list_to_array ls in
    build_max_heap a';
    { heap_size = ref (Array.length a); arr = a' }

  module P = ArrayPrinter (COpt)

  let print_heap h = P.print_array h.arr

  (* Dereferencing the record *)
  let heap_maximum h = h.arr.(0)

  (* Fix this atrocity in the return type! *)
  let heap_extract_max h =
    if !(h.heap_size) < 1
    then None
    else (
      let a = h.arr in
      (* Take the maximum element *)
      let max = a.(0) in
      (* Restore heap-ness *)
      a.(0) <- a.(!(h.heap_size) - 1);
      a.(!(h.heap_size) - 1) <- None;
      h.heap_size := !(h.heap_size) - 1;
      max_heapify !(h.heap_size) h.arr 0;
      (* Return the result *)
      max)

  let heap_increase_key h i key =
    let a = h.arr in
    let c = COpt.comp key a.(i) >= 0 in
    if not c
    then (
      Printf.printf "A new key is smaller than current key!";
      assert false);
    a.(i) <- key;
    let j = ref i in
    while !j > 0 && COpt.comp (snd (H.parent a !j)) a.(!j) < 0 do
      let pj = fst (H.parent a !j) in
      swap a !j pj;
      j := pj
    done

  let max_heap_insert h elem =
    let hs = !(h.heap_size) in
    if hs >= Array.length h.arr then raise (Failure "Maximal heap capacity reached!");
    h.heap_size := hs + 1;
    heap_increase_key h hs (Some elem)
end

module PQ = PriorityQueue (KV)
