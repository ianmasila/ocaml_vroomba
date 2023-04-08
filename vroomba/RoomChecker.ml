open Util
open Rooms
open BST

(*********************************************)
(*         Movements of Vroomba              *)
(*********************************************)

type move =
  | Up
  | Left
  | Down
  | Right

(* Print the move *)
let pp_move = function
  | Up -> "W"
  | Left -> "A"
  | Down -> "S"
  | Right -> "D"

(* This is a data-type representing the state of the room at a
   certain point of cleaning. It should include the room and keep
   track of Vroomba's current position, parts that are already cleaned
   and that are remaining to be cleaned. Use this data type internally
   in the function `check_solution` *)

module Set = BinarySearchTree

type 'a set = 'a Set.tree

type state = {
  room : room;
  position : (int * int) ref;
  uncleaned : (int * int) set;
}

(* Instantiate a state object, based on a particular room *)
let mk_state r =
  let uncleaned = Set.mk_tree () in
  for i = 0 to r.y_len - 1 do
    for j = 0 to r.x_len - 1 do
      if r.map.(i).(j) <> None
      then (
        ignore (Set.insert uncleaned (j, i));
        (* Ensures that all squares start with being uncleaned *)
        r.map.(i).(j) <- Some false)
    done
  done;
  { room = r; position = ref r.start; uncleaned }

(*********************************************)
(*            Checking solution              *)
(*********************************************)

(*  Get a trace of Vroomba from a string  *)
(*  A string can be ill-formed! *)
let string_to_solution (s : string) : move list option =
  let len = String.length s in
  let moves = ref [] in
  try
    for i = 0 to len - 1 do
      match s.[i] with
      | 'W' -> moves := Up :: !moves
      | 'A' -> moves := Left :: !moves
      | 'S' -> moves := Down :: !moves
      | 'D' -> moves := Right :: !moves
      | _ -> raise (Failure "Unrecognised character")
    done;
    Some (List.rev !moves)
  with
  | Failure _ -> None

let solution_to_string (moves : move list) : string =
  let b = Buffer.create (List.length moves) in
  let rec visit m =
    match m with
    | [] -> Buffer.contents b
    | move :: m' ->
      Buffer.add_string b (pp_move move);
      visit m'
  in
  visit moves

let new_position (x, y) move =
  match move with
  | Up -> (x, y + 1)
  | Down -> (x, y - 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let legal_move r (x, y) move = is_part_of_room r (new_position (x, y) move)

(*  Checks whether a square is not yet cleaned  *)
let is_uncleaned r (x, y) = is_part_of_room r (x, y) && r.map.(y).(x) = Some false

(*   Check that all squares have been cleaned  *)
let all_cleaned (s : state) : bool =
  let res = ref true in
  if Set.elements s.uncleaned <> []
  then false
  else (
    for i = 0 to s.room.y_len - 1 do
      for j = 0 to s.room.x_len - 1 do
        res := !res && not (is_uncleaned s.room (j, i))
      done
    done;
    !res)

type clean_or_unclean =
  | Cleaned
  | Uncleaned

(*  Returns a list of neighbours which are accessible (cleanable) and also not yet cleaned  *)
let get_neighbours clean_or_unclean r (x, y) =
  let root_cleanable = ref false in
  let up_cleanable = ref false in
  let down_cleanable = ref false in
  let left_cleanable = ref false in
  let right_cleanable = ref false in
  let up_left_cleanable = ref false in
  let up_right_cleanable = ref false in
  let down_left_cleanable = ref false in
  let down_right_cleanable = ref false in
  let cleanable_neighbours =
    [|
      root_cleanable;
      up_cleanable;
      down_cleanable;
      left_cleanable;
      right_cleanable;
      up_left_cleanable;
      up_right_cleanable;
      down_left_cleanable;
      down_right_cleanable;
    |]
  in
  let surrounding =
    [|(0, 0); (0, 1); (0, -1); (-1, 0); (1, 0); (-1, 1); (1, 1); (-1, -1); (1, -1)|]
  in
  let all_neighbours =
    Array.map (fun (x_add, y_add) -> (x + x_add, y + y_add)) surrounding
  in
  for i = 0 to 4 do
    if is_part_of_room r all_neighbours.(i) then cleanable_neighbours.(i) := true
  done;
  let up_left = (x - 1, y + 1) in
  let up_right = (x + 1, y + 1) in
  let down_left = (x - 1, y - 1) in
  let down_right = (x + 1, y - 1) in
  if is_part_of_room r up_left && !up_cleanable && !left_cleanable
  then up_left_cleanable := true;
  if is_part_of_room r up_right && !up_cleanable && !right_cleanable
  then up_right_cleanable := true;
  if is_part_of_room r down_left && !down_cleanable && !left_cleanable
  then down_left_cleanable := true;
  if is_part_of_room r down_right && !down_cleanable && !right_cleanable
  then down_right_cleanable := true;
  let neighbours = ref [] in
  Array.iteri
    (fun i cleanable ->
      if !cleanable
      then (
        match clean_or_unclean with
        | Cleaned ->
          if not (is_uncleaned r all_neighbours.(i))
          then neighbours := all_neighbours.(i) :: !neighbours
        | Uncleaned ->
          if is_uncleaned r all_neighbours.(i)
          then neighbours := all_neighbours.(i) :: !neighbours))
    cleanable_neighbours;
  !neighbours

(*  Cleans the neighbour squares + root square based on the current position  *)
let clean s =
  let x, y = !(s.position) in
  let uncleaned_neighbours = get_neighbours Uncleaned s.room (x, y) in
  List.iter
    (fun (x, y) ->
      s.room.map.(y).(x) <- Some true;
      let n = get_exn @@ Set.search s.uncleaned (x, y) in
      Set.delete_node s.uncleaned n)
    uncleaned_neighbours

(*  Check that the sequence of moves is valid  *)
let check_solution (r : room) (moves : move list) : bool = 
  let s = mk_state r in 
  let rec visit m =
    match m with 
    | [] -> clean s;
            if all_cleaned s 
            then true
            else false (*base case*)
    | m :: m' -> 
      if not (legal_move r !(s.position) m) 
      then false
      else (clean s;
            s.position := new_position !(s.position) m;
            visit m')
  in
  visit moves

(*  Top-level validator  *)
let validate r s =
  match string_to_solution s with
  | None -> false
  | Some moves -> check_solution r moves


let%test "Basic validate test" =
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "WDDDDDD"

let%test "Validate false because not fully cleaned" =
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "WDDDDD" = false

let%test "Validate false because illegal move" =
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "DDDDDD" = false

(* let%test "Validate test (using given rooms)" =
  let polygon_list = file_to_polygons "../../../resources/rooms.txt" in
  let rooms = List.map polygon_to_room polygon_list in
  let open ReadingFiles in
  let solutions =
    List.filter (fun s -> s <> "") (read_file_to_strings "../../../resources/rooms.sol")
  in
  assert (List.length rooms = List.length solutions);
  for i = 0 to List.length rooms - 1 do
    let r = List.nth rooms i in
    let s = List.nth solutions i in
    assert (validate r s)
  done;
  true 

let%test "Validate fail test (using given rooms)" =
  let polygon_list = file_to_polygons "../../../resources/rooms.txt" in
  let rooms = List.map polygon_to_room polygon_list in
  let open ReadingFiles in
  let solutions =
    List.filter (fun s -> s <> "") (read_file_to_strings "../../../resources/rooms.sol")
  in
  assert (List.length rooms = List.length solutions);
  for i = 0 to List.length rooms - 1 do
    let r = List.nth rooms i in
    let s = List.nth solutions i in
    assert (not @@ validate r s)
  done;
  true *)
