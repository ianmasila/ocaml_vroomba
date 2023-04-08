open RoomRendering

open Rooms
open RoomGenerator
open RoomChecker
open RoomSolver
open ReadingFiles

let vroom_vroom (() : unit) : unit =
  print_endline
    "Hello, I'm Vroomba. The input files for all modes can contain a single room or \
     multiple rooms. Here's what you can do:";
  print_endline "";
  print_endline "1) Solve a room, finding a path that cleans the entire room";
  print_endline "  solve input_file output_file";
  print_endline "";
  print_endline "2) Check the solution for a room";
  print_endline "  check input_file solutions_file";
  print_endline "";
  print_endline "3) Generate n number of rooms, of size by size";
  print_endline "  generate n size output_file";
  print_endline "";
  print_endline "4) Solve the room yourself!";
  print_endline "  play input_file output_file";
  print_endline "";
  print_endline "5) Animate the room cleaning with a provided solution";
  print_endline "  render input_file solution_file"

let (() : unit) =
  if Array.length Sys.argv = 1 || Sys.argv.(1) = "help"
  then vroom_vroom ()
  else if Array.length Sys.argv < 4
  then (
    print_endline "Not enough input provided";
    print_endline "Possible formats: ";
    print_endline "  solve input_file output_file";
    print_endline "  check input_file solutions_file";
    print_endline "  generate n size output_file";
    print_endline "  play input_file output_file";
    print_endline "  render input_file output_file";
    print_endline "  animate input_file solution_file";
    print_endline "  help")
  else (
    let command = Sys.argv.(1) in
    match command with
    | "solve" ->
      (* bin/vroomba solve input_file output_file *)
      let input_file = Sys.argv.(2) in
      let output_file = Sys.argv.(3) in
      let polygon_list = ref [] in
      (try polygon_list := file_to_polygons input_file with 
      | Sys_error _ -> print_endline "Cannot find input file. Please try again.");
      let rec make_move_strings polygon_list acc =
        match polygon_list with
        | [] -> acc
        | p :: t ->
          let r = polygon_to_room p in
          let moves = solve_room r in
          let s = solution_to_string moves in
          make_move_strings t (s :: acc)
      in let move_strings = List.rev (make_move_strings !polygon_list []) in
      (try write_strings_to_file output_file move_strings with
      | Sys_error _ -> print_endline "Cannot write to output file. Please try again.")
    | "check" -> failwith "Implement me!"
    | "generate" -> 
      (* bin/vroomba generate num size output_file *)
      let num = int_of_string Sys.argv.(2) in
      let size = int_of_string Sys.argv.(3) in
      let output_file = Sys.argv.(4) in
      let room_strings = ref [] in
      for _i = 0 to num - 1 do
        let r = generate_random_room size in
        let p = room_to_polygon r in
        let s = polygon_to_string p in
        room_strings := s :: !room_strings;
      done;
      (try write_strings_to_file output_file !room_strings with
      | Sys_error _ -> print_endline "Cannot write to output file. Please try again.")
    | "play" ->
      (* bin/vroomba play input_file output_file *)
      let input_file = Sys.argv.(2) in
      let output_file = Sys.argv.(3) in
      (try render_games input_file output_file with
      | Sys_error _ -> print_endline "Cannot find file. Please try again.")
    | "render" ->
      let input_file = Sys.argv.(2) in
      let solution_file = Sys.argv.(3) in
      (try render_animation input_file solution_file with
      | Sys_error _ -> print_endline "Cannot find file. Please try again.")
    | _ -> vroom_vroom ())
