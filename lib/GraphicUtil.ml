open Graphics

let origin : int * int = (400, 300)

let go_to_origin (() : unit) : unit =
  let x = fst origin in
  let y = snd origin in
  moveto x y;
  set_color black

let draw_axes (() : unit) : unit =
  let x = fst origin in
  let y = snd origin in
  set_color green;
  moveto 0 y;
  lineto (x * 2) y;
  moveto x 0;
  lineto x (y * 2);
  moveto x y;
  set_color black

let mk_screen (() : unit) : unit =
  open_graph " 800x600";
  draw_axes ()

let clear_screen (() : unit) : unit =
  clear_graph ();
  draw_axes ()
