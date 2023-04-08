open Util
open Points
open Polygons
open Stacks
open GraphicUtil

(*************************************)
(*        Auxiliary opoerations      *)
(*************************************)

module StackX (S : AbstractStack) = struct
  include S

  let top s =
    match pop s with
    | None -> None
    | Some x ->
      push s x;
      Some x

  let next_to_top s =
    match pop s with
    | None -> None
    | Some x ->
      let y = top s in
      push s x;
      y

  let list_of_stack s =
    let res = ref [] in
    while not (is_empty s) do
      let e = get_exn @@ pop s in
      res := e :: !res
    done;
    !res
end

(*************************************)
(*           Convex hull             *)
(*************************************)

(* Sort by axis Y *)
let axis_y_sorter (Point (x1, y1)) (Point (x2, y2)) =
  if y1 < y2
  then -1
  else if y1 > y2
  then 1
  else if x1 < x2
  then -1
  else if x1 > x1
  then 1
  else 0

(* Sort by polar angle wrt p0 *)
let polar_angle_sorter p0 p1 p2 =
  let (Polar (r1, a1)) = p1 -- p0 |> polar_of_cartesian in
  let (Polar (r2, a2)) = p2 -- p0 |> polar_of_cartesian in
  if a1 < a2
  then -1
  else if a1 > a2
  then 1
  else if r1 < r2
  then -1
  else if r1 > r2
  then 1
  else 0

module CHStack = StackX (ListBasedStack)

(* Graham's Scan *)
let convex_hull points =
  (* At least three points *)
  assert (List.length points >= 3);
  let y_sorted = List.sort axis_y_sorter points in
  let p0 = y_sorted |> List.hd in
  match List.tl y_sorted |> List.sort (polar_angle_sorter p0) with
  | p1 :: p2 :: rest ->
    let open CHStack in
    let s = mk_stack 0 in
    push s p0;
    push s p1;
    push s p2;
    let non_left_turn p =
      let q1 = next_to_top s |> get_exn in
      let q2 = top s |> get_exn in
      direction q1 q2 p >= 0
    in
    (* Main algorithm *)
    List.iter
      (fun p ->
        while non_left_turn p do
          let _ = pop s in
          ()
        done;
        push s p)
      rest;
    list_of_stack s
  | _ -> failwith "Cannot happen"

(* Question: what is the complexity *)

(*************************************)
(*        Testing Convex hulls       *)
(*************************************)

let gen_random_points ?(dim = 550.) n =
  let res = ref [] in
  for _ = 0 to n - 1 do
    let p = gen_random_point dim in
    res := p :: !res
  done;
  !res

(*************************************)
(*     Tracing convex hulls          *)
(*************************************)

let draw_stack_and_points s ps =
  let open CHStack in
  if is_empty s
  then ()
  else (
    let l = list_of_stack s in
    List.iter (fun e -> push s e) l;
    let ll = all_pairs l in
    List.iter draw_point ps;
    List.iter (draw_segment ~color:Graphics.red) ll;
    Unix.sleepf 0.3)

let convex_hull_with_tracing ?(cur = false) points =
  (* At least three points *)
  assert (List.length points >= 3);
  let y_sorted = List.sort axis_y_sorter points in
  let p0 = y_sorted |> List.hd in
  match List.tl y_sorted |> List.sort (polar_angle_sorter p0) with
  | p1 :: p2 :: rest ->
    let open CHStack in
    let s = mk_stack 0 in
    push s p0;
    push s p1;
    push s p2;
    let non_left_turn p =
      let q1 = next_to_top s |> get_exn in
      let q2 = top s |> get_exn in
      direction q1 q2 p >= 0
    in
    List.iter
      (fun p ->
        while non_left_turn p do
          let _ = pop s in
          ()
        done;
        push s p;
        clear_screen ();
        if cur then draw_segment ~color:Graphics.blue (p0, p);
        draw_stack_and_points s points)
      rest;
    let res = list_of_stack s in
    clear_screen ();
    List.iter draw_point points;
    draw_polygon ~color:Graphics.red res;
    res
  | _ -> failwith "Cannot happen"

(*****************************************)
(*        Testing Convex Hulls           *)
(*****************************************)

let test_random_ch n =
  let ps = gen_random_points n in
  let ch = convex_hull ps in
  assert (is_convex ch);
  assert (List.for_all (point_within_polygon ch) ps)
