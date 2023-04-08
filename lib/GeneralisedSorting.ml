open ArrayUtil

(*****************************************)
(*   Sorting with arbitrary comparator   *)
(*****************************************)
let generic_quick_sort arr ~comp =
  let partition arr lo hi =
    if hi <= lo
    then lo
    else (
      let pivot = arr.(hi - 1) in
      let i = ref lo in
      for j = lo to hi - 2 do
        if comp arr.(j) pivot <= 0
        then (
          swap arr !i j;
          i := !i + 1)
      done;
      swap arr !i (hi - 1);
      !i)
  in
  let rec sort arr lo hi =
    if hi - lo <= 1
    then ()
    else (
      let mid = partition arr lo hi in
      sort arr lo mid;
      sort arr mid hi)
  in
  sort arr 0 (Array.length arr)

let int_order_asc x y = if x < y then -1 else if x = y then 0 else 1

(*****************************************)
(*          Sorting via functor          *)
(*****************************************)

module type Comparable = sig
  type t

  val comp : t -> t -> int
end

module Sorting (Comp : Comparable) = struct
  open Comp

  let sort arr =
    let partition arr lo hi =
      if hi <= lo
      then lo
      else (
        let pivot = arr.(hi - 1) in
        let i = ref lo in
        for j = lo to hi - 2 do
          if comp arr.(j) pivot <= 0
          then (
            swap arr !i j;
            i := !i + 1)
        done;
        swap arr !i (hi - 1);
        !i)
    in
    let rec sort_aux arr lo hi =
      if hi - lo <= 1
      then ()
      else (
        let mid = partition arr lo hi in
        sort_aux arr lo mid;
        sort_aux arr mid hi)
    in
    sort_aux arr 0 (Array.length arr)
end

module IntAsc = struct
  type t = int

  let comp = int_order_asc
end

module AscIntSorting = Sorting (IntAsc)
