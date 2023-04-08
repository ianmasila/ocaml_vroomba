open Rooms
open RoomGenerator
open RoomChecker
open BST

(*********************************************)
(*              Room solver                  *)
(*********************************************)

(* This is a complex task. Feel free to introduce whatever functions
  and data types you consider necessary, and also rely on data
  structures and algorithms from the lectures (see folder `lib` of
   this project). *)

(* Solve the room and produce the list of moves. *)
(* Make use of RoomChecker.state state type internally in your solver *)
let solve_room (r : room) : move list = 
  let movelist = ref [] in
  let tiles = BinarySearchTree.mk_tree () in
  let s = mk_state r in

  let check_move (m:move)  = 
    legal_move r !(s.position) m
  in

  let add_move (m:move) = 
    movelist := m::!movelist;
    s.position := new_position !(s.position) m;
  in

  let visited (i :int * int)= 
    match BinarySearchTree.search tiles i with 
    | None -> 
              false
    | Some _ ->
              true 
  in

  let add_visited pos= 
  match BinarySearchTree.insert tiles pos with 
  | true -> ();
  | false -> ();

  in

  let reverse (m:move): move = 
    match m with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left
  in



  let first_move = 
    if (check_move Up)
    then Up 
    else if (check_move Down)
    then Down 
    else if (check_move Left)
    then Left
    else Right
    in

    let rec dfs (m: move): unit =
      clean s;
          if (all_cleaned s)
          then (
                ();)
          else (
                if (not (visited !(s.position)))
                then (
                  add_visited !(s.position);
                  if (check_move Up && not (visited (new_position !(s.position) Up)) )
                  then (add_move Up;
                        dfs Up);
                  if (check_move Down && not (visited (new_position !(s.position) Down)))
                  then (add_move Down; 
                    dfs Down);
                  if (check_move Left && not (visited (new_position !(s.position) Left)))
                  then (add_move Left;  
                    dfs Left);
                  if (check_move Right && not (visited (new_position !(s.position) Right)))
                  then (add_move Right; 
                    dfs Right);
                if (not (all_cleaned s))
                then (
                      add_move (reverse m);))
                else(
                )
          )
          
        in
        dfs first_move; (*make sure first move is valid move*)
        List.rev (!movelist);; 


(*********************************************)
(*               Testing                     *)
(*********************************************)

let%test "Solver testing (using given rooms)" =
  let polygon_list = file_to_polygons "../../../resources/rooms.txt" in
  for i = 0 to List.length polygon_list - 1 do
    let polygon = List.nth polygon_list i in
    let r = polygon_to_room polygon in
    let moves = solve_room r in
    let s = solution_to_string moves in
    assert (check_solution r moves && validate r s);
  done;
  true


let%test "Randomised solver testing" =
  let r = generate_random_room 30 in
  let moves = solve_room r in
  let s = solution_to_string moves in
  check_solution r moves && validate r s

let%test "Randomised solver testing (Large)" =
  let r = generate_random_room 100 in
  let moves = solve_room r in
  let s = solution_to_string moves in
  check_solution r moves && validate r s

let%test "Extra Randomised solver testing" =
  Random.self_init ();
  for _ = 0 to Random.int 10 + 10 do
    let r = generate_random_room (Random.int 71 + 20) in
    let moves = solve_room r in
    let s = solution_to_string moves in
    assert (check_solution r moves && validate r s)
  done;
  true
