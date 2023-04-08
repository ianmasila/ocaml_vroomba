open Rooms
open RoomChecker
open RoomSolver
open Graphics

(*********************************************)
(*           Gamifying the solver            *)
(*********************************************)

let write_string_to_file filename text =
  let outc = Core.Out_channel.create ~append:true filename in
  Core.Out_channel.output_string outc text;
  Core.Out_channel.close outc

let set_gray x = Graphics.rgb x x x

let gray1 = set_gray 100

and gray2 = set_gray 170

and gray3 = set_gray 240

let set_colour x y z = Graphics.rgb x y z

let palette_grey = set_colour 39 46 56

and palette_grey2 = set_colour 70 84 97

and palette_lightblue = set_colour 235 243 245

and palette_blue = set_colour 196 220 224

and palette_emerald = set_colour 115 156 162

and palette_orange = set_colour 254 137 58

and palette_white = set_colour 254 248 248

type box_config = {
  x : int;
  y : int;
  w : int;
  h : int;
  active : bool;
}

let draw_box_outline bcf col =
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w bcf.h

let draw_box bcf =
  let x1 = bcf.x
  and y1 = bcf.y in
  if bcf.active
  then (
    Graphics.set_color palette_lightblue;
    Graphics.fill_rect x1 y1 bcf.w bcf.h;
    draw_box_outline bcf palette_emerald)
  else (
    Graphics.set_color palette_grey;
    Graphics.fill_rect x1 y1 bcf.w bcf.h;
    draw_box_outline bcf palette_grey)

type position =
  | Left
  | Center
  | Right

let draw_string_in_box pos str bcf col =
  let w, h = Graphics.text_size str in
  let ty = bcf.y + ((bcf.h - h) / 2) in
  (match pos with
  | Center -> Graphics.moveto (bcf.x + ((bcf.w - w) / 2)) ty
  | Right ->
    let tx = bcf.x + bcf.w - w - 1 in
    Graphics.moveto tx ty
  | Left ->
    let tx = bcf.x + 1 in
    Graphics.moveto tx ty);
  Graphics.set_color col;
  Graphics.draw_string str

let create_grid vb room b scale offset_x offset_y =
  for i = room.y_len - 1 downto 0 do
    for j = 0 to room.x_len - 1 do
      let nx = (j * scale) - offset_x in
      let ny = (i * scale) - offset_y in
      match room.map.(i).(j) with
      | None -> vb.(i).(j) <- { b with x = nx; y = ny; active = false }
      | Some _ -> vb.(i).(j) <- { b with x = nx; y = ny; active = true }
    done
  done;
  vb

let check_legal move vroomba_pos room =
  match move with
  | 'w' -> legal_move room !vroomba_pos Up
  | 'a' -> legal_move room !vroomba_pos Left
  | 's' -> legal_move room !vroomba_pos Down
  | 'd' -> legal_move room !vroomba_pos Right
  | _ -> false

let rec colour_cleaned neighbourslist vb =
  match neighbourslist with
  | (x, y) :: t ->
    (Graphics.set_color palette_blue;
     let bcf = vb.(y).(x) in
     let x1 = bcf.x
     and y1 = bcf.y in
     Graphics.fill_rect x1 y1 bcf.w bcf.h;
     draw_string_in_box Center "C" bcf palette_grey2;
     draw_box_outline bcf palette_emerald);
    colour_cleaned t vb
  | _ -> ()

let draw_text s x y colour =
  moveto x y;
  Graphics.set_color colour;
  draw_string s

let win_text = "You win!"

let next_text = "Press 'n' to move on to the next room."

let tease_text = "Are you better than the algorithm?"

let set_box_color vb color (x, y) =
  Graphics.set_color color;
  let bcf = vb.(y).(x) in
  let x1 = bcf.x
  and y1 = bcf.y in
  Graphics.fill_rect x1 y1 bcf.w bcf.h

let rec wait_until_n_pressed _ =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q'
  then exit 0
  else if event.key == 'n'
  then ()
  else wait_until_n_pressed ()

let rec wait_until_q_pressed state vb vroomba_pos moves output_path current_score =
  let room = state.room in
  let event = wait_next_event [Key_pressed] in
  let x = fst !vroomba_pos in
  let y = snd !vroomba_pos in
  let try_to_move (new_x, new_y) dir =
    if legal_move room !vroomba_pos dir
    then (
      set_box_color vb gray2 (x, y);
      draw_string_in_box Center "V" vb.(y).(x) Graphics.white;
      vroomba_pos := (new_x, new_y);
      moves := pp_move dir :: !moves;
      draw_text (Format.sprintf "Your score: %d" !current_score) 783 845 palette_grey;
      current_score := !current_score + 1;
      draw_text (Format.sprintf "Your score: %d" !current_score) 783 845 palette_white;
      state.position := !vroomba_pos;
      clean state;
      colour_cleaned (get_neighbours Cleaned room !vroomba_pos) vb;
      set_box_color vb palette_orange !vroomba_pos;
      draw_string_in_box
        Center
        "V"
        vb.(snd !vroomba_pos).(fst !vroomba_pos)
        Graphics.black;
      if all_cleaned state
      then (
        write_string_to_file output_path (String.concat "" (List.rev !moves));
        write_string_to_file output_path "\n";
        draw_text win_text 400 800 palette_white;
        draw_text next_text 330 780 palette_white;
        wait_until_n_pressed ())
      else wait_until_q_pressed state vb vroomba_pos moves output_path current_score)
    else wait_until_q_pressed state vb vroomba_pos moves output_path current_score
  in
  if event.key == 'q'
  then exit 0
  else if event.key == 'n'
  then ()
  else (
    match event.key with
    | 'w' -> try_to_move (x, y + 1) Up
    | 'a' -> try_to_move (x - 1, y) Left
    | 's' -> try_to_move (x, y - 1) Down
    | 'd' -> try_to_move (x + 1, y) Right
    | _ -> wait_until_q_pressed state vb vroomba_pos moves output_path current_score)

(*  Top-level file for starting the game  *)
let render_games (input_path : string) (output_path : string) : unit =
  open_graph " 900x900";
  let polylist = file_to_polygons input_path in
  let len = List.length polylist in
  for i = 0 to len - 1 do
    clear_graph ();
    Graphics.set_color palette_grey;
    Graphics.draw_rect 0 0 (size_x ()) (size_y ());
    Graphics.fill_rect 0 0 (size_x ()) (size_y ());
    let room = polygon_to_room (List.nth polylist i) in
    (* let solution = solve_room room in *)
    let state = mk_state room in
    let vroomba_pos = ref (fst room.start, snd room.start) in
    let moves = ref [] in
    (* let high_score = List.length solution in *)
    let current_score = ref 0 in
    let scale = min (900 / (room.x_len * 5 / 3)) (900 / (room.y_len * 5 / 3)) in
    let offset_x = ((900 / scale) - (room.x_len * scale)) / 3 in
    let offset_y = ((900 / scale) - (room.y_len * scale)) / 4 in
    let b =
      { x = fst room.start; y = snd room.start; w = scale; h = scale; active = false }
    in
    let vb_init = Array.make room.y_len (Array.make 0 b) in
    for i = 0 to room.y_len - 1 do
      vb_init.(i) <- Array.make room.x_len b
    done;
    let vb = create_grid vb_init room b scale offset_x offset_y in
    for i = 0 to room.y_len - 1 do
      Array.iter draw_box vb.(i)
    done;
    clean state;
    colour_cleaned (get_neighbours Cleaned room !vroomba_pos) vb;
    set_box_color vb palette_orange !vroomba_pos;
    draw_string_in_box Center "V" vb.(snd !vroomba_pos).(fst !vroomba_pos) Graphics.black;
    draw_text tease_text 680 875 palette_white;
    (* draw_text (Format.sprintf "Score to beat: %d" high_score) 780 860 palette_white; *)
    draw_text (Format.sprintf "Your score: %d" !current_score) 783 845 palette_white;
    wait_until_q_pressed state vb vroomba_pos moves output_path current_score
  done

let render_move state vb vroomba_pos dir =
  let room = state.room in
  let x = fst !vroomba_pos in
  let y = snd !vroomba_pos in
  let try_to_move (new_x, new_y) =
    if legal_move room !vroomba_pos dir
    then (
      set_box_color vb gray2 (x, y);
      draw_string_in_box Center "V" vb.(y).(x) Graphics.white;
      vroomba_pos := (new_x, new_y);
      state.position := !vroomba_pos;
      clean state;
      colour_cleaned (get_neighbours Cleaned room !vroomba_pos) vb;
      set_box_color vb yellow !vroomba_pos;
      draw_string_in_box
        Center
        "V"
        vb.(snd !vroomba_pos).(fst !vroomba_pos)
        Graphics.black)
  in
  try_to_move (new_position (x, y) dir)

let animate rooms solutions =
  let len = List.length rooms in
  if len <> List.length solutions
  then raise (Failure "Number of rooms and solutions don't match");
  for i = 0 to len - 1 do
    clear_graph ();
    Graphics.set_color palette_grey;
    Graphics.draw_rect 0 0 (size_x ()) (size_y ());
    Graphics.fill_rect 0 0 (size_x ()) (size_y ());
    let room = List.nth rooms i in
    let move_list = List.nth solutions i in
    let state = mk_state room in
    let vroomba_pos = ref (fst room.start, snd room.start) in
    let scale = min (900 / (room.x_len * 5 / 3)) (900 / (room.y_len * 5 / 3)) in
    let offset_x = ((900 / scale) - (room.x_len * scale)) / 3 in
    let offset_y = ((900 / scale) - (room.y_len * scale)) / 4 in
    let b =
      { x = fst room.start; y = snd room.start; w = scale; h = scale; active = false }
    in
    let vb_init = Array.make room.y_len (Array.make 0 b) in
    for i = 0 to room.y_len - 1 do
      vb_init.(i) <- Array.make room.x_len b
    done;
    let vb = create_grid vb_init room b scale offset_x offset_y in
    for i = 0 to room.y_len - 1 do
      Array.iter draw_box vb.(i)
    done;
    clean state;
    colour_cleaned (get_neighbours Cleaned room !vroomba_pos) vb;
    set_box_color vb yellow !vroomba_pos;
    draw_string_in_box Center "V" vb.(snd !vroomba_pos).(fst !vroomba_pos) Graphics.black;
    let rec move moves =
      match moves with
      | [] -> ()
      | next_move :: remaining_moves ->
        render_move state vb vroomba_pos next_move;
        Thread.delay 0.05;
        move remaining_moves
    in
    move move_list;
    draw_text next_text 330 830 palette_white;
    while (wait_next_event [Key_pressed]).key <> 'n' do
      ()
    done
  done

let animate_solution (input_path : string) (output_path : string) : unit =
  let open ReadingFiles in
  open_graph " 900x900";
  let polylist = file_to_polygons input_path in
  let rooms = List.map polygon_to_room polylist in
  let solutions = List.map solve_room rooms in
  let solution_strings = List.map solution_to_string solutions in
  write_strings_to_file output_path solution_strings;
  animate rooms solutions

let render_animation (input_path : string) (solution_path : string) : unit =
  let open ReadingFiles in
  open_graph " 900x900";
  let polylist = file_to_polygons input_path in
  let rooms = List.map polygon_to_room polylist in
  let solution_strings =
    List.filter (fun s -> s <> "") (read_file_to_strings solution_path)
  in
  let solutions =
    List.map
      (fun s ->
        match string_to_solution s with
        | None -> raise (Failure "Unrecognised solution")
        | Some soln -> soln)
      solution_strings
  in
  animate rooms solutions
