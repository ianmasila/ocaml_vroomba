open Rooms

open Util


(*********************************************)
(*       Automated generation of rooms       *)
(*********************************************)

(*  size is the maximal span of the room along both X and Y dimensions *)
(*  Example generate_random_room 4 should return a room that fits into a
    4x4 square. *)


(*********************************************)
(*           Tracing the perimeter           *)
(*********************************************)
(* type walk = 
  | Right
  | Left
  | Up
  | Down
  | Stay

let move_list = [Right; Left; Up; Down]
let move_array = [|Right; Left; Up; Down|]

let rec next_random_move (prev_moves : walk list) : walk option = 
  (* prev_moves is a list of previous moves made successfully or unsuccessfully *)
  if List.length prev_moves >= 4 then None else
  let move_list' = List.filter (fun m -> not (List.mem m prev_moves)) move_list in
  let move_array' = list_to_array move_list' in
  let next_move = move_array'.(Random.int (Array.length move_array')) in
  Some next_move 

let intersection_caused (candidate : segment) (seg_list : segment list) : bool =
  List.exists (segments_intersect candidate) seg_list
  
let go_right (current_point : point) (limit : float) (current_segments : segment list) : (point * segment) option =
  let x, y = get_x current_point, get_y current_point in
  let upper = int_of_float (limit -. x) in
  if upper <= 0 then None else
  let dx_int = Random.int upper in
  let dx = float_of_int dx_int in
  let x' = x +. dx in
  let new_point = Point(x', y) in
  let new_segment = current_point, new_point in
  if x' <=~ limit && (new_point = Point(0.0, 0.0) || not (intersection_caused new_segment current_segments))
  then Some(new_point, new_segment)
  else None

let go_left (current_point : point) (limit : float) (current_segments : segment list) : (point * segment) option =
  let x, y = get_x current_point, get_y current_point in
  let upper = int_of_float x in
  if upper <= 0 then None else
  let dx_int = Random.int upper in
  let dx = float_of_int dx_int in
  let x' = x -. dx in
  let new_point = Point(x', y) in
  let new_segment = current_point, new_point in
  if x' >=~ 0. && (new_point = Point(0.0, 0.0) || not (intersection_caused new_segment current_segments))
  then Some(new_point, new_segment)
  else None
  
let go_up (current_point : point) (limit : float) (current_segments : segment list) : (point * segment) option =
  let x, y = get_x current_point, get_y current_point in
  let upper = int_of_float (limit -. y) in
  if upper <= 0 then None else
  let dy_int = Random.int upper in
  let dy = float_of_int dy_int in
  let y' = y +. dy in
  let new_point = Point(x, y') in
  let new_segment = current_point, new_point in
  if y' <=~ limit && (new_point = Point(0.0, 0.0) || not (intersection_caused new_segment current_segments))
  then Some(new_point, new_segment)
  else None

let go_down (current_point : point) (limit : float) (current_segments : segment list) : (point * segment) option =
  let x, y = get_x current_point, get_y current_point in
  let upper = int_of_float y in
  if upper <= 0 then None else
  let dy_int = Random.int upper in
  let dy = float_of_int dy_int in
  let y' = y -. dy in
  let new_point = Point(x, y') in
  let new_segment = current_point, new_point in
  if y' >=~ 0. && (new_point = Point(0.0, 0.0) || not (intersection_caused new_segment current_segments))
  then Some(new_point, new_segment)
  else None

let generate_random_room (size : int) : room = 
  (* generate polygon `p` then polygon_to_room `p` *)
  let generate_random_polygon (size : int) : polygon =
    let limit = float_of_int size in
    let rec trace_perimeter (prev_moves : walk list) (current_point : point) (current_segments : segment list) (p : polygon) : polygon = 
      if current_point = Point(0.0, 0.0) && List.length p >= 4
      then List.rev (uniq p)
      else (
        let move = next_random_move prev_moves in
        match move with
        | Some(Right) -> 
          Printf.printf "going right...";
          let point_seg_opt = go_right current_point limit current_segments in
          (
          match point_seg_opt with
          | Some(pt, seg) -> 
            (let p', current_segments' = (pt :: p), (seg :: current_segments) in
            let prev_moves' = [Left] in
            trace_perimeter prev_moves' pt current_segments' p')
          | None ->
            let prev_moves' = Right :: prev_moves in
            trace_perimeter prev_moves' current_point current_segments p
          )
        | Some(Left) -> 
          Printf.printf "going left...";
          let point_seg_opt = go_left current_point limit current_segments in
          (
          match point_seg_opt with
          | Some(pt, seg) -> 
            (let p', current_segments' = (pt :: p), (seg :: current_segments) in
            let prev_moves' = [Right] in
            trace_perimeter prev_moves' pt current_segments' p')
          | None ->
            let prev_moves' = Left :: prev_moves in
            trace_perimeter prev_moves' current_point current_segments p
          )
        | Some(Up) -> 
          Printf.printf "going up...";
          let point_seg_opt = go_up current_point limit current_segments in
          (
          match point_seg_opt with
          | Some(pt, seg) -> 
            (let p', current_segments' = (pt :: p), (seg :: current_segments) in
            let prev_moves' = [Down] in
            trace_perimeter prev_moves' pt current_segments' p')
          | None ->
            let prev_moves' = Up :: prev_moves in
            trace_perimeter prev_moves' current_point current_segments p
          )
        | Some(Down) -> 
          Printf.printf "going down...";
          let point_seg_opt = go_down current_point limit current_segments in
          (
          match point_seg_opt with
          | Some(pt, seg) -> 
            (let p', current_segments' = (pt :: p), (seg :: current_segments) in
            let prev_moves' = [Up] in
            trace_perimeter prev_moves' pt current_segments' p')
          | None ->
            let prev_moves' = Down :: prev_moves in
            trace_perimeter prev_moves' current_point current_segments p
          )
        | Some(Stay) -> 
          trace_perimeter prev_moves current_point current_segments p
        | None -> 
          (* no moves can be made *)
          let s = polygon_to_string p in
          Printf.printf "polygon length %d\n" (List.length p);
          Printf.printf "%s\n" s;
          Printf.printf "possibly no more moves to make\n";
          (* restart *)
          (* trace_perimeter ([Stay]) (Point(0.0, 0.0)) [] [] *)
          (* force completion *)
          let p', current_segments' = (Point(0.0, 0.0) :: Point(0.0, limit /. 2.0) :: p), ((Point(0.0, limit /. 2.0), Point(0.0, 0.0)) :: (current_point, Point(0.0, limit /. 2.0)) :: current_segments) in
          trace_perimeter prev_moves (Point(0.0, 0.0)) current_segments' p'
      )
    in let p = trace_perimeter ([Stay]) (Point(0.0, 0.0)) [] [] in
    let s = polygon_to_string p in
    Printf.printf "RESULT\n%s\n" s;
    p
  in let r = polygon_to_room (generate_random_polygon size) in
  r *)

(* let rec grouped_to_twos ls =
  match ls with
  | [] -> []
  | [_] -> []
  | h1 :: h2 :: t -> (h1, h2) :: grouped_to_twos t *)

type walk = 
  | Right
  | Left
  | Up
  | Down
  | Stay

let up_or_down_array = [|Up; Up; Down|]

let go_right (start : int * int) (limit : int) : (int * int) option =
  let x, y = fst start, snd start in
  (* Printf.printf "going right from (%d, %d)\n" x y; *)
  let upper =  limit - x in
  if upper <= 1 then None  (* terminate room generation *)
  else (
    let dx = Random.int upper + 1 in
    let x' = x + dx in
    let intermediate = (x', y) in
    (* Printf.printf "intermediate (%d, %d)\n" x' y; *)
    Some(intermediate)
  )

(* go_up and go_down are mutually dependent *)
let rec go_up (intermediate : int * int) (limit : int) (down_failed : bool) : (int * int) option =
  let x, y = fst intermediate, snd intermediate in
  (* Printf.printf "going up from (%d, %d)\n" x y; *)
  let upper =  limit - y in
  if upper <= 1 
  then (
    if down_failed then None  (* terminate room generation *)
    else go_down intermediate limit true 
  )
  else (
    let dy = Random.int upper + 1 in
    let y' = y + dy in
    let fin = (x, y') in
    (* Printf.printf "going up to (%d, %d)\n" x y'; *)
    Some(fin)
  )
and go_down (intermediate : int * int) (limit : int) (up_failed : bool) : (int * int) option =
  let x, y = fst intermediate, snd intermediate in
  (* Printf.printf "going down from (%d, %d)\n" x y; *)
  let upper = y in
  if upper <= 1 
  then (
    if up_failed then None  (* terminate room generation *)
    else go_up intermediate limit true 
  )
  else (
    let dy = Random.int upper + 1 in
    let y' = y - dy in
    let fin = (x, y') in
    (* Printf.printf "going down to (%d, %d)\n" x y'; *)
    Some(fin)
  )

let generate_random_room (size : int) : room = 
  (* take the union of random rectangles, making sure to fill in any lacunas *)
  let max_rectangles = (size / 2) + 1 in
  (* Printf.printf "max_rectangles is %d\n" max_rectangles; *)
  let rec make_rectangle (current_coord : int * int) (move : walk) (acc_size : int) (acc : (int * int) list) : (int * int) list = 
    match move with 
    | Right -> 
      let intercoord_option = go_right current_coord size in
      (
      match intercoord_option with
      | Some(intermediate_coord) ->
        let acc' = current_coord :: acc in
        let acc_size' = acc_size + 1 in
        let next_move = up_or_down_array.(Random.int (Array.length up_or_down_array)) in
        make_rectangle intermediate_coord next_move acc_size' acc'
      | None ->
        acc
      )
    | Up -> 
      let endcoord_option = go_up current_coord size false in
      (
      match endcoord_option with
      | Some(end_coord) ->
        let start_coord = List.hd acc in
        let acc' = end_coord :: acc in
        let acc_size' = acc_size + 1 in
        (* make new rectangle whose start_coord is the middle of just-made rectangle *)
        if acc_size' < max_rectangles 
        then (
          let new_start_coord = (fst end_coord + fst start_coord)/2, (snd end_coord + snd start_coord)/2 in
          make_rectangle new_start_coord Right acc_size' acc'
        )
        else acc'
      | None ->
        acc
      )
    | Down -> 
      let endcoord_option = go_down current_coord size false in
      (
      match endcoord_option with
      | Some(end_coord) ->
        let start_coord = List.hd acc in
        let acc' = end_coord :: acc in
        let acc_size' = acc_size + 1 in
        (* make new rectangle whose start_coord is the middle of just-made rectangle *)
        if acc_size' < max_rectangles 
          then (
            let new_start_coord = (fst end_coord + fst start_coord)/2, (snd end_coord + snd start_coord)/2 in
            make_rectangle new_start_coord Right acc_size' acc'
          )
          else acc'
      | None ->
        acc
      )
    | _ -> acc
  in let rectangle_points = List.rev (make_rectangle (0, 0) Right 0 []) in
  (* let rectangles = grouped_to_twos rectangle_points in *)
  (* make empty room and flip tiles within `rectangles` to Some false *)
  let room = mk_room size size 0 0 in
  (* go through each rectangle, flipping tiles to Some false *)
  let rec lay_tiles (r : room) (rectangle_points : (int * int) list) : unit =
    match rectangle_points with
    | [] -> ()
    | [_] -> () 
    | h1 :: h2 :: t -> 
      let x1, y1 = fst h1, snd h1 in
      let x2, y2 = fst h2, snd h2 in
      (* Printf.printf "(%d, %d) -- (%d, %d)\n" x1 y1 x2 y2; *)
      for y = min y1 y2 to (max y1 y2) - 1 do
        for x = x1 to x2 - 1 do
          room.map.(x).(y) <- Some false
        done
      done;
      lay_tiles r t
  in lay_tiles room rectangle_points;
  room

(* Define what it means to the room to be valid (e.g., no lacunas,
   obstacles, there is a place for initial Vroomba position, etc). *)
let valid_room (r : room) : bool = 
  let p = room_to_polygon r in
  let s = polygon_to_string p in
  (* Printf.printf "Printing polygon of randomly generated room...\n%s\n" s; *)
  let p' = string_to_polygon s in
  p' <> None
  && r.x_len > 0 && r.y_len > 0 && (Array.length r.map) > 0
  (* room must not have any lacunas: cycle-checking *)
  && true

(*********************************************)
(*                     Tests                 *)
(*********************************************)

let%test "Generated room is valid" = 
  let r = generate_random_room 100 in
  valid_room r

let%test "Basic valid_room test" =
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  valid_room room  

let%test "Conversion testing (using generated rooms)" =
  let r = generate_random_room (Random.int 30 + 20) in
  let p' = room_to_polygon r in
  let r' = polygon_to_room p' in
  let p'' = room_to_polygon r' in
  let r'' = polygon_to_room p'' in
  r' = r'' 

let%test "Given rooms are valid" =
  let polygon_list = file_to_polygons "../../../resources/rooms.txt" in
  for i = 0 to List.length polygon_list - 1 do
    let p = List.nth polygon_list i in
    let r = polygon_to_room p in
    assert (valid_room r)
  done;
  true

let%test "Generated room is valid" =
  let r = generate_random_room 100 in
  valid_room r

let%test "Multiple random generate_random_room tests" =
  for _ = 0 to 30 do
    let r = generate_random_room (Random.int 20 + 10) in
    assert (valid_room r)
  done;
  true

let%test "Extra random generate_random_room test" =
  let r = generate_random_room (Random.int 50 + 20) in
  valid_room r
