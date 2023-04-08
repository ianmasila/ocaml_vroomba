open Util

(* 1. OCaml's universal hashing *)

(* Hashtbl.hash *)

(* 2. New signature for hash table *)

module type HashTable = sig
  type key

  type 'v hash_table

  val mk_new_table : int -> (key * 'v) hash_table

  val insert : (key * 'v) hash_table -> key -> 'v -> unit

  val get : (key * 'v) hash_table -> key -> 'v option

  val remove : (key * 'v) hash_table -> key -> unit

  val print_hash_table
    :  (key -> string) ->
    ('v -> string) ->
    (key * 'v) hash_table ->
    unit
end

(* 3. Testing hash-tables *)

module HashTableTester (H : HashTable) = struct
  module MyHT = H
  open MyHT

  let mk_test_table_from_array_length (a : key array) (m : int) : (key * key) hash_table =
    let n = Array.length a in
    let ht = mk_new_table m in
    for i = 0 to n - 1 do
      insert ht a.(i) a.(i)
    done;
    ht

  let test_table_get (ht : (key * key) hash_table) (a : key array) : bool =
    let len = Array.length a in
    for i = 0 to len - 1 do
      let e = get ht a.(i) in
      assert (e <> None);
      let x = get_exn e in
      assert (x = a.(i))
    done;
    true
end

(* 4. Exposing keys *)

module type KeyType = sig
  type t
end

(* 5. Redefining our hash-table *)

(* Redefining our hash-table *)
module SimpleListBasedHashTable (K : KeyType) : HashTable with type key = K.t = struct
  type key = K.t

  type 'v hash_table = {
    buckets : 'v list array;
    capacity : int;
  }

  let mk_new_table cap =
    let buckets = Array.make cap [] in
    { buckets; capacity = cap }

  let insert ht k v =
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in
    let bucket = ht.buckets.(bnum) in
    let clean_bucket = List.filter (fun (k', _) -> k' <> k) bucket in
    ht.buckets.(bnum) <- (k, v) :: clean_bucket

  let get ht k =
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in
    let bucket = ht.buckets.(bnum) in
    let res = List.find_opt (fun (k', _) -> k' = k) bucket in
    match res with
    | Some (_, v) -> Some v
    | _ -> None

  (* Slow remove - introduce for completeness *)
  let remove ht k =
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in
    let bucket = ht.buckets.(bnum) in
    let clean_bucket = List.filter (fun (k', _) -> k' <> k) bucket in
    ht.buckets.(bnum) <- clean_bucket

  let print_hash_table ppk ppv ht =
    let open Printf in
    print_endline @@ sprintf "Capacity: %d" ht.capacity;
    print_endline "Buckets:";
    let buckets = ht.buckets in
    for i = 0 to ht.capacity - 1 do
      let bucket = buckets.(i) in
      if bucket <> []
      then (
        (* Print bucket *)
        let s =
          List.fold_left
            (fun acc (k, v) -> acc ^ (sprintf "(%s, %s); ") (ppk k) (ppv v))
            ""
            bucket
        in
        printf "%d -> [ %s]\n" i s)
    done
end

(* Testing our simple implementation *)
module IntString = struct
  type t = int * string
end

module SHT = SimpleListBasedHashTable (IntString)
module SimpleHTTester = HashTableTester (SHT)

(*************************)
(* Resizeable hash table *)
(*************************)

module ResizableListBasedHashTable (K : KeyType) : HashTable with type key = K.t = struct
  type key = K.t

  type 'v hash_table = {
    buckets : 'v list array ref;
    size : int ref;
    capacity : int ref;
  }

  let mk_new_table cap =
    let buckets = Array.make cap [] in
    { buckets = ref buckets; capacity = ref cap; size = ref 0 }

  let rec insert ht k v =
    let hs = Hashtbl.hash k in
    let bnum = hs mod !(ht.capacity) in
    let bucket = !(ht.buckets).(bnum) in
    let clean_bucket = List.filter (fun (k', _) -> k' <> k) bucket in
    let new_bucket = (k, v) :: clean_bucket in
    !(ht.buckets).(bnum) <- new_bucket;
    (* Increase size *)
    if List.length bucket < List.length new_bucket then ht.size := !(ht.size) + 1;
    (* Resize *)
    if !(ht.size) > !(ht.capacity) + 1 then resize_and_copy ht

  and resize_and_copy ht =
    let new_capacity = !(ht.capacity) * 2 in
    let new_buckets = Array.make new_capacity [] in
    let new_ht =
      { buckets = ref new_buckets; capacity = ref new_capacity; size = ref 0 }
    in
    let old_buckets = !(ht.buckets) in
    let len = Array.length old_buckets in
    for i = 0 to len - 1 do
      let bucket = old_buckets.(i) in
      List.iter (fun (k, v) -> insert new_ht k v) bucket
    done;
    ht.buckets := !(new_ht.buckets);
    ht.capacity := !(new_ht.capacity);
    ht.size := !(new_ht.size)

  let get ht k =
    let hs = Hashtbl.hash k in
    let bnum = hs mod !(ht.capacity) in
    let bucket = !(ht.buckets).(bnum) in
    let res = List.find_opt (fun (k', _) -> k' = k) bucket in
    match res with
    | Some (_, v) -> Some v
    | _ -> None

  (* Slow remove - introduce for completeness *)
  let remove ht k =
    let hs = Hashtbl.hash k in
    let bnum = hs mod !(ht.capacity) in
    let bucket = !(ht.buckets).(bnum) in
    let clean_bucket = List.filter (fun (k', _) -> k' <> k) bucket in
    !(ht.buckets).(bnum) <- clean_bucket;
    if List.length bucket > List.length clean_bucket then ht.size := !(ht.size) - 1;
    assert (!(ht.size) >= 0)

  let print_hash_table ppk ppv ht =
    let open Printf in
    print_endline @@ sprintf "Capacity: %d" !(ht.capacity);
    print_endline @@ sprintf "Size:     %d" !(ht.size);
    print_endline "Buckets:";
    let buckets = !(ht.buckets) in
    for i = 0 to !(ht.capacity) - 1 do
      let bucket = buckets.(i) in
      if bucket <> []
      then (
        (* Print bucket *)
        let s =
          List.fold_left
            (fun acc (k, v) -> acc ^ (sprintf "(%s, %s); ") (ppk k) (ppv v))
            ""
            bucket
        in
        printf "%d -> [ %s]\n" i s)
    done
end

module RHT = ResizableListBasedHashTable (IntString)
module ResizableHTTester = HashTableTester (RHT)
