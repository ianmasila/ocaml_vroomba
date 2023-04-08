(*  Operations with floating points   *)

let eps : float = 0.0000001

let ( =~= ) x y = abs_float (x -. y) < eps

let ( <=~ ) x y = x =~= y || x < y

let ( >=~ ) x y = x =~= y || x > y

let is_zero x = x =~= 0.0

(******************************************)
(*            Points and vectors          *)
(******************************************)

type point = Point of float * float

let get_x (Point (x, _y)) = x

let get_y (Point (_x, y)) = y

open GraphicUtil

let draw_point ?(color = Graphics.black) (Point (x, y)) =
  let open Graphics in
  let a, b = current_point () in
  let ix = int_of_float x + fst origin in
  let iy = int_of_float y + snd origin in
  moveto ix iy;
  set_color color;
  fill_circle ix iy 3;
  moveto a b;
  set_color black

(*

TODO: Draw a point

*)

(* Some test points *)
module TestPoints = struct
  let p = Point (100., 150.)

  let q = Point (-50., 75.)

  let r = Point (50., 30.)

  let s = Point (75., 60.)

  let t = Point (75., 90.)
end

(* Move the point *)
let ( ++ ) (Point (x, y)) (dx, dy) = Point (x +. dx, y +. dy)

(*

TODO:
- Draw multiple points.
- Draw a line.
- Draw a triangle.

*)

(************************************)
(*        Point as a vector         *)
(************************************)

let vec_length (Point (x, y)) = sqrt ((x *. x) +. (y *. y))

let draw_vector (Point (x, y)) =
  let ix = int_of_float x + fst origin in
  let iy = int_of_float y + snd origin in
  go_to_origin ();
  Graphics.lineto ix iy;
  go_to_origin ()

(* Subtract vectors *)
let ( -- ) (Point (x1, y1)) (Point (x2, y2)) = Point (x1 -. x2, y1 -. y2)

(***************************************)
(* Scalar product and its applications *)
(***************************************)

(*

* Question:
  What is the graphical interpretation of the dot-product?

*)

let dot_product (Point (x1, y1)) (Point (x2, y2)) = (x1 *. x2) +. (y1 *. y2)

let angle_between v1 v2 =
  let l1 = vec_length v1 in
  let l2 = vec_length v2 in
  if is_zero l1 || is_zero l2
  then 0.0
  else (
    let p = dot_product v1 v2 in
    let a = p /. (l1 *. l2) in
    assert (abs_float a <=~ 1.);
    acos a)

(* To polar representation *)

let pi = 4. *. atan 1.

type polar = Polar of (float * float)

let polar_of_cartesian (Point (x, y) as p) =
  let r = vec_length p in
  let phi = atan2 y x in
  let phi' = if phi =~= ~-.pi then phi +. (pi *. 2.) else phi in
  assert (phi' > ~-.pi && phi' <=~ pi);
  Polar (r, phi')

let cartesian_of_polar (Polar (r, phi)) =
  let x = r *. cos phi in
  let y = r *. sin phi in
  Point (x, y)

let rotate_by_angle p a =
  let (Polar (r, phi)) = polar_of_cartesian p in
  let p' = Polar (r, phi +. a) in
  cartesian_of_polar p'

(*

TODO: Graphical experiments with the angles

* Rotate one vector to another
* How to determine the angle?
* Draw something cool using polar coordinates.

*)

(************************************)
(*          Cross-product           *)
(************************************)

let cross_product (Point (x1, y1)) (Point (x2, y2)) = (x1 *. y2) -. (x2 *. y1)

let sign p = if p =~= 0. then 0 else if p < 0. then -1 else 1

(* Where should we turning p *)
let dir_clock p1 p2 =
  let prod = cross_product p1 p2 in
  sign prod

(*

TODO: Determine the direction to rotate a point.

*)

let rotate_to p1 p2 =
  let a = angle_between p1 p2 in
  let d = dir_clock p1 p2 |> float_of_int in
  rotate_by_angle p1 (a *. d)

(* Determining turns *)

(*
 1 - turning right (clock-wise)
-1 - turning left  (counter-clock-wise)
 0 - no turn

*)
let direction p0 p1 p2 = cross_product (p2 -- p0) (p1 -- p0) |> sign

(*
TODO:
experiment with directions.

*)

(******************************************)
(*    Segments and operations on them     *)
(******************************************)

type segment = point * point

(* Draw a segment *)
let draw_segment ?(color = Graphics.black) (a, b) =
  let open Graphics in
  let (Point (ax, ay)) = a in
  let (Point (bx, by)) = b in
  draw_point ~color a;
  draw_point ~color b;
  let iax = int_of_float ax + fst origin in
  let iay = int_of_float ay + snd origin in
  moveto iax iay;
  set_color color;
  let ibx = int_of_float bx + fst origin in
  let iby = int_of_float by + snd origin in
  lineto ibx iby;
  go_to_origin ()

module TestSegments = struct
  open TestPoints

  let s0 = (q, p)

  let s1 = (p, s)

  let s2 = (r, s)

  let s3 = (r, t)

  let s4 = (t, p)

  let s5 = (Point (-100., -100.), Point (100., 100.))

  let s6 = (Point (-100., 100.), Point (100., -100.))
end

(*****************************************)
(*  Generating random points on segments *)
(*****************************************)

let gen_random_point f =
  let ax = Random.float f in
  let ay = Random.float f in
  let o = Point (f /. 2., f /. 2.) in
  Point (ax, ay) -- o

let gen_random_segment f = (gen_random_point f, gen_random_point f)

let gen_random_point_on_segment seg =
  let p1, p2 = seg in
  let (Point (dx, dy)) = p2 -- p1 in
  let f = Random.float 1. in
  let p = p1 ++ (dx *. f, dy *. f) in
  p

(******************************************)
(*              Collinearity              *)
(******************************************)

(* Checking if segments are collinear *)
let collinear s1 s2 =
  let p1, p2 = s1 in
  let p3, p4 = s2 in
  let d1 = direction p3 p4 p1 in
  let d2 = direction p3 p4 p2 in
  d1 = 0 && d2 = 0

(* Checking if a point is on a segment *)
let point_on_segment s p =
  let a, b = s in
  if not (collinear (a, p) (p, b))
  then false
  else (
    let (Point (ax, ay)) = a in
    let (Point (bx, by)) = b in
    let (Point (px, py)) = p in
    min ax bx <=~ px && px <=~ max ax bx && min ay by <=~ py && py <=~ max ay by)

(******************************************)
(*         Checking for intersections     *)
(******************************************)

(* Checking if two segments intersect on a segment *)

let intersect_as_collinear s1 s2 =
  if not (collinear s1 s2)
  then false
  else (
    let p1, p2 = s1 in
    let p3, p4 = s2 in
    point_on_segment s1 p3
    || point_on_segment s1 p4
    || point_on_segment s2 p1
    || point_on_segment s2 p2)

(* Checking if two segments intersect *)
let segments_intersect s1 s2 =
  if collinear s1 s2
  then intersect_as_collinear s1 s2
  else (
    let p1, p2 = s1 in
    let p3, p4 = s2 in
    let d1 = direction p3 p4 p1 in
    let d2 = direction p3 p4 p2 in
    let d3 = direction p1 p2 p3 in
    let d4 = direction p1 p2 p4 in
    if ((d1 < 0 && d2 > 0) || (d1 > 0 && d2 < 0))
       && ((d3 < 0 && d4 > 0) || (d3 > 0 && d4 < 0))
    then true
    else if d1 = 0 && point_on_segment s2 p1
    then true
    else if d2 = 0 && point_on_segment s2 p2
    then true
    else if d3 = 0 && point_on_segment s1 p3
    then true
    else if d4 = 0 && point_on_segment s1 p4
    then true
    else false)

(******************************************)
(*      Finding intersection points       *)
(******************************************)

(* Finding an intersection point of two
   non-collinear intersecting segments *)
let find_intersection s1 s2 =
  let p1, p2 = s1 in
  let p3, p4 = s2 in
  if not (segments_intersect s1 s2)
  then None
  else if collinear s1 s2
  then
    if point_on_segment s1 p3
    then Some p3
    else if point_on_segment s1 p4
    then Some p4
    else if point_on_segment s2 p1
    then Some p1
    else Some p2
  else (
    let r = Point (get_x p2 -. get_x p1, get_y p2 -. get_y p1) in
    let s = Point (get_x p4 -. get_x p3, get_y p4 -. get_y p3) in
    assert (not @@ is_zero @@ cross_product r s);
    (*
     (p1 + t r) × s = (p3 + u s) × s,

      s x s = 0, hence

      t = (p3 − p1) × s / (r × s)
    *)
    let t = cross_product (p3 -- p1) s /. cross_product r s in
    let (Point (rx, ry)) = r in
    let p = p1 ++ (rx *. t, ry *. t) in
    Some p)

(*

TODO:
Fun with intersections and random segments on a plot

let s1 = (Point (113.756053827471192, -175.292497988606272),
 Point (18.0694083766823042, 124.535770332375932));;

let s2 = (Point (59.0722072343553464, -171.91124390306868),
   Point (139.282462974003465, 20.2804812244832249));;

*)
