open ArrayUtil

(**********************************************************)
(*                      Bloom Filters                     *)
(**********************************************************)

module type BloomHashing = sig
  type t

  val hash_functions : (t -> int) list
end

(* Bloom filter signature *)
module type BloomFilter = functor (H : BloomHashing) -> sig
  type t

  val mk_bloom_filter : int -> t

  val insert : t -> H.t -> unit

  val contains : t -> H.t -> bool

  val print_filter : t -> unit
end

(* Bloom filter implementation *)
module BloomFilterImpl : BloomFilter =
functor
  (H : BloomHashing)
  ->
  struct
    (* Type of filter *)
    type t = {
      slots : bool array;
      size : int;
    }

    let mk_bloom_filter n =
      let a = Array.make n false in
      { slots = a; size = n }

    let insert f e =
      let n = f.size in
      List.iter
        (fun hash ->
          let h = hash e mod n in
          f.slots.(h) <- true)
        H.hash_functions

    let contains f e =
      if H.hash_functions = []
      then false
      else (
        let n = f.size in
        let res = ref true in
        List.iter
          (fun hash ->
            let h = hash e mod n in
            res := !res && f.slots.(h))
          H.hash_functions;
        !res)

    module BP = ArrayPrinter (struct
      type t = bool

      let pp b = if b then "1" else "0"
    end)

    let print_filter t =
      let open BP in
      print_array t.slots
  end

module IntStringHashing = struct
  type t = int * string

  let hash1 (k, _) = Hashtbl.hash k

  let hash2 (_, v) = Hashtbl.hash v

  let hash3 (k, _) = k

  let hash_functions = [hash1; hash2; hash3]
end

(********************************************************)
(* Using Bloom filter to speed up the simple hash table *)
(********************************************************)

open BetterHashTable

module BloomHashTable (K : BloomHashing) = struct
  type key = K.t

  (* Adding bloom filter *)
  module BF = BloomFilterImpl (K)

  type 'v hash_table = {
    buckets : 'v list array;
    capacity : int;
    filter : BF.t;
  }

  let mk_new_table cap =
    let buckets = Array.make cap [] in
    (* Pick reasonably large BF size *)
    let filter = BF.mk_bloom_filter 15000 in
    { buckets; capacity = cap; filter }

  let insert ht k v =
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in
    let bucket = ht.buckets.(bnum) in
    let filter = ht.filter in
    let clean_bucket =
      (* New stuff *)
      if BF.contains filter k (* Only filter if ostensibly contains key *)
      then List.filter (fun (k', _) -> k' <> k) bucket
      else bucket
    in
    (* Missed in the initial the implementation *)
    BF.insert filter k;
    ht.buckets.(bnum) <- (k, v) :: clean_bucket

  let get ht k =
    let filter = ht.filter in
    if BF.contains filter k
    then (
      let hs = Hashtbl.hash k in
      let bnum = hs mod ht.capacity in
      let bucket = ht.buckets.(bnum) in
      let res = List.find_opt (fun (k', _) -> k' = k) bucket in
      match res with
      | Some (_, v) -> Some v
      | _ -> None)
    else None

  (* Cannot remove *)
  let remove _ _ = raise (Failure "Removal is deprecated!")

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

module BHT = BloomHashTable (IntStringHashing)
module BHTTester = HashTableTester (BHT)
