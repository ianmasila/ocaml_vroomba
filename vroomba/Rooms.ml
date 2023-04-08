open Points
open Polygons

open Util
open BetterHashTable

module EdgeTable = ResizableListBasedHashTable (struct
  type t = int * int
end)

(*********************************************)
(*         Representation of Rooms           *)
(*********************************************)

(* Your implementation of internal room data type *)
(* You may modify it to describe the room conveniently for solving. *)

(*********************************************)
(*               Room as Graph               *)
(*********************************************)

(* A room is composed of tiles *)
(* A `tile` is a record of a tile's bottom-left corner coordinates (x, y) and its clean status *)
(* type tile = {
  pt: point;
  clean : ref bool;
} *)

(* A `room` is a graph of tiles. We choose a graph data structure because Vroomba is really a graph traversal problem *)

(* type room = {
  x_len : int;
  y_len : int;
  start : int * int;
  map : bool option array array;   (* for easily checking whether a tile is in the room/ clean *)
  tiles : (tile, int) LinkedGraphs.graph  (* polygon with points having clean boolean field: for easy Vroomba traversal *)
} *)


(*  Building a Room graph for a x_len by y_len room 
 *  Node payload: tile: bottom-left corner coordinates (x * y)
 *  Edge payload: int  
 *)

(* let mk_room x_len y_len start_x start_y : room =
  (* Make empty room *)
  let tiles = LinkedGraphs.mk_graph () in
  (* Add tile nodes to fill room *)
  for y = 0 to y_len - 1 do
    for x = 0 to x_len - 1  do
      let tl = { pt = Point(x, y); clean = false }
      in LinkedGraphs.add_node tiles tl;
    done
  done;
  (* Add edges *)
*)

(*********************************************)
(*          Room as Array of Arrays          *)
(*********************************************)

type room = {
  x_len : int;
  y_len : int;
  start : int * int;
  map : bool option array array;
}

let mk_room x_len y_len start_x start_y =
  let map = Array.make y_len (Array.make 0 None) in
  (* We do this to make sure that each array is
     distinct and not using the same reference *)
  for i = 0 to y_len - 1 do
    map.(i) <- Array.make x_len None
  done;
  { x_len; y_len; start = (start_x, start_y); map }

(* Checks whether a character is a valid (either number or minus sign) *)
let valid_num c =
  let ascii = Char.code c in
  if (ascii >= 48 && ascii <= 57) || ascii = 45 then true else false

(*  Read a polygon from a string of coordinates as in resources/basic.txt  *)
(*  A string can be ill-formed! *)
let string_to_polygon (s : string) : polygon option =
  let len = String.length s in
  let rec expect_bracket i p =
    if i = len
    then raise (Failure "Incomplete coordinate string")
    else (
      match s.[i] with
      | '(' -> expect_first_num (i + 1) p (i + 1)
      | ' ' -> expect_bracket (i + 1) p
      | c -> raise (Failure (Printf.sprintf "Expected '(' but got %c. i = %d" c i)))
  and expect_first_num i p start =
    if i = len
    then raise (Failure "Incomplete coordinate string")
    else (
      match s.[i] with
      | ',' ->
        let x = int_of_string @@ String.sub s start (i - start) in
        expect_second_num (i + 1) p (i + 1) x
      | ' ' ->
        if i = start
        then expect_first_num (i + 1) p (start + 1)
        else raise (Failure (Printf.sprintf "Extra spacing. i = %d" i))
      | c ->
        if valid_num c
        then expect_first_num (i + 1) p start
        else raise (Failure (Printf.sprintf "Expected number but got %c. i = %d" c i)))
  and expect_second_num i p start x =
    if i = len
    then raise (Failure "Incomplete coordinate string")
    else (
      match s.[i] with
      | ')' ->
        let y = int_of_string @@ String.sub s start (i - start) in
        expect_semicolon (i + 1) ((x, y) :: p)
      | ' ' ->
        if i = start
        then expect_second_num (i + 1) p (start + 1) x
        else raise (Failure (Printf.sprintf "Extra spacing. i = %d" i))
      | c ->
        if valid_num c
        then expect_second_num (i + 1) p start x
        else raise (Failure (Printf.sprintf "Expected number but got '%c'. i = %d" c i)))
  and expect_semicolon i p =
    if i = len
    then p
    else (
      match s.[i] with
      | ';' -> expect_bracket (i + 1) p
      | ' ' -> expect_semicolon (i + 1) p
      | c -> raise (Failure (Printf.sprintf "Expected ';' but got %c. i = %d" c i)))
  in
  try
    let p = expect_bracket 0 [] in
    for i = 0 to List.length p - 1 do
      let prev_i = ref (i - 1) in
      if i = 0 then prev_i := List.length p - 1;
      let x1, y1 = List.nth p !prev_i in
      let x2, y2 = List.nth p i in
      if x1 <> x2 && y1 <> y2
      then
        raise
          (Failure
             "Coordinates result in a slanted line (room edges can only be horizontal or \
              vertical)")
    done;
    Some (polygon_of_int_pairs (List.rev p))
  with
  | Failure s ->
    Printf.printf "Reason for None: %s\n" s;
    None

(*  Read all polygons from a file  *)
let file_to_polygons (path : string) : polygon list =
  let open ReadingFiles in
  (* Split into separate strings based on \n and
     converts each string to polygon *)
  let strings = List.filter (fun s -> s <> "") (read_file_to_strings path) in
  List.map
    (fun s ->
      match string_to_polygon s with
      | None -> raise (Failure "Ill-formed string")
      | Some p -> p)
    strings

(* Rounds a float to the nearest integer
   Note rounding 0.5 will result in 1*)
let round f =
  let floor = Float.floor f in
  let ceil = Float.ceil f in
  if ceil -. f <= f -. floor then int_of_float ceil else int_of_float floor

(* Checks whether a float is almost an integer
   e.g. 25.00000000000001 or 19.9999999999999
   and returns the rounded integer*)
let almost_int f =
  let rounded_f = round f in
  if f =~= float rounded_f
  then rounded_f
  else
    raise
      (Failure
         "Polygon contains non-integer values. Does not make sense for room coordinates")

let polygon_to_string (p : polygon) : string = 
  (* type polygon = point list 
   * type point = Point of float * float
   *)
  let point_to_string (pt : point) : string =
    let x, y = get_x pt, get_y pt in
    Printf.sprintf "(%d, %d); " (round x) (round y)
  in 
  let buff = Buffer.create 16 in
  let strings = List.map (point_to_string) p in
  List.iter (Buffer.add_string buff) strings;
  let s = Buffer.contents buff in
  (* remove the ending ;_  *)
  String.sub s 0 (String.length s - 2)

let write_polygons_to_file (ps : polygon list) (path : string) : unit =
  let open ReadingFiles in
  let lines = List.map (polygon_to_string) ps in
  write_strings_to_file path lines

(*********************************************)
(*           Rooms and polygons              *)
(*********************************************)

(* Returns properties of the bounding box around a polygon *)
let min_max p =
  let min_x = ref max_float in
  let min_y = ref max_float in
  let max_x = ref min_float in
  let max_y = ref min_float in
  List.iter
    (fun (Point (x, y)) ->
      min_x := min !min_x x;
      min_y := min !min_y y;
      max_x := max !max_x x;
      max_y := max !max_y y)
    p;
  (round !min_x, round !min_y, round !max_x, round !max_y)

(*  Convert a polygon to a room data type  *)
let polygon_to_room (p : polygon) : room =
  (* check whether all coordinates are valid, i.e. almost integers *)
  List.iter
    (fun (Point (x, y)) ->
      let _ = (almost_int x, almost_int y) in
      ())
    p;
  (* find bounding box of polygon *)
  let min_x, min_y, max_x, max_y = min_max p in
  let x_len = max_x - min_x in
  let y_len = max_y - min_y in
  let x_offset = float min_x +. 0.5 in
  let y_offset = float min_y +. 0.5 in
  let start_x = round (-.x_offset) in
  let start_y = round (-.y_offset) in
  (* make room using bounding box of polygon *)
  let room = mk_room x_len y_len start_x start_y in
  (* Check whether each coordinate in the bounding box belongs inside the polygon *)
  for i = 0 to room.y_len - 1 do
    for j = 0 to room.x_len - 1 do
      (* `point` is point in the middle of the target tile hence offset has +. 0.5 *)
      let point = Point (float j +. x_offset, float i +. y_offset) in
      if point_within_polygon p point then room.map.(i).(j) <- Some false
    done
  done;
  room

(* Checks whether a coordinate is outside the bounding box of the room *)
let out_of_bounds r (x, y) = x >= r.x_len || y >= r.y_len || x < 0 || y < 0

(* Checks whether a coordinate is part of the room *)
let is_part_of_room r (x, y) =
  if out_of_bounds r (x, y) then false else r.map.(y).(x) <> None

(*  Convert a room to a list of polygon coordinates  *)
let room_to_polygon (r : room) : polygon = 
  let x_offset = fst r.start in
  let y_offset = snd r.start in
  (* Printf.printf "r.start_x, r.start_y (%d, %d)\n" x_offset y_offset; *)
  (* Find edge points for all edge tiles of the room *)
  let edge_points = EdgeTable.mk_new_table 1000 in
  let origin, origin_found = ref (0, 0), ref false in
  for y = 0 to r.y_len - 1 do
    for x = 0 to r.x_len - 1 do 
      (* check if tile exists in room *)
      if is_part_of_room r (x, y) then (
        (* Printf.printf "is_part_of_room (%d, %d)\n" x y; *)
        (* check for None from the 4 neighbours, i.e. down, right, up, and left of tile. if None, add edge points *)
        (* down neighbour *)
        if not (is_part_of_room r (x, y - 1)) 
        then (
          if not !origin_found then (
            origin := (x, y);
            origin_found := true;
          );
          EdgeTable.insert edge_points (x, y) (x + 1, y);
        );
        (* right neighbour *)
        if not (is_part_of_room r (x + 1, y))
        then (
          if not !origin_found then (
            origin := (x + 1, y);
            origin_found := true;
          );
          EdgeTable.insert edge_points (x + 1, y) (x + 1, y + 1);
        );
        (* up neighbour *)
        if not (is_part_of_room r (x, y + 1)) 
        then (
          if not !origin_found then (
            origin := (x + 1, y + 1);
            origin_found := true;
          );
          EdgeTable.insert edge_points (x + 1, y + 1) (x, y + 1);
        );
        (* left neighbour *)
        if not (is_part_of_room r (x - 1, y))
        then (
          if not !origin_found then (
            origin := (x, y + 1);
            origin_found := true;
          );
          EdgeTable.insert edge_points (x, y + 1) (x, y);
        )  
      )
    done
  done; 
  (* Printf.printf "origin: (%d, %d)\n" (fst !origin) (snd !origin); *)
  let rec trace_polygon (start : int * int) (origin : int * int) (lookup_map : ((int * int) * (int * int)) EdgeTable.hash_table) (coordinates : (int * int) list) : polygon =
    if start = origin && (List.length coordinates >= 4) 
    then polygon_of_int_pairs coordinates 
    else (
      let next_coordinate = get_exn (EdgeTable.get lookup_map start) in
      trace_polygon next_coordinate origin lookup_map (next_coordinate :: coordinates)
    )
  in let raw_polygon = List.rev (trace_polygon !origin !origin edge_points [!origin]) in
  (* Printf.printf "raw_polygon_string\n%s\n" polygon_to_string raw_polygon; *)
  let rec get_corner_polygon (p : polygon) (p' : polygon) : polygon =
    match p with
    | p0 :: ((p1 :: p2 :: _) as tl) -> 
      let sgn = direction p0 p1 p2 in
      get_corner_polygon tl (if sgn <> 0 then (p1 :: p') else p')
    | _ -> p'
  in let unshifted_polygon = List.rev (get_corner_polygon raw_polygon [List.hd raw_polygon]) in
  let actual_polygon = shift_polygon (-.(float_of_int x_offset), -.(float_of_int y_offset)) unshifted_polygon in
  (* Printf.printf "actual_polygon_length: %d\n" (List.length actual_polygon); *)
  (* Printf.printf "actual_polygon_string\n%s\n\n" (polygon_to_string actual_polygon); *)
  actual_polygon

(*********************************************)
(*                     Tests                 *)
(*********************************************)

let%test "Conversion testing (using given rooms)" =
  let polygon_list = file_to_polygons "../../../resources/rooms.txt" in
  for i = 0 to List.length polygon_list - 1 do
    let p = List.nth polygon_list i in
    (* Printf.printf "rooms.txt polygon_length: %d\n" (List.length p); *)
    (* Printf.printf "rooms.txt polygon_string\n%s\n\n" (polygon_to_string p); *)
    let r = polygon_to_room p in
    let p' = room_to_polygon r in
    let r' = polygon_to_room p' in
    assert (r = r')
  done;
  true
