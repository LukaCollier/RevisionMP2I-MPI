open Types
open Graphics

(*************************)
(* Fonctions d'affichage *)
(*************************)

let display_card (deck : (card, coord list) Hashtbl.t) (str : card) : unit =
  let map = Array.make_matrix 5 5 ' ' in
  map.(2).(2) <- '#';
  List.iter (fun (x,y) -> map.(-y+2).(x+2) <- 'X') (Hashtbl.find deck str);
  Array.iter (fun x -> Array.iter print_char x; print_newline ()) map

let display_grid (g : grid) : unit =
  let map = Array.make_matrix 5 5 ' ' in
  let (x,y) = g.p1king in map.(-y+2).(x+2) <- 'O';
  let (x,y) = g.p2king in map.(-y+2).(x+2) <- 'X';
  List.iter (fun (x,y) -> map.(-y+2).(x+2) <- 'o') g.p1pawns;
  List.iter (fun (x,y) -> map.(-y+2).(x+2) <- 'x') g.p2pawns;
  Array.iter (fun x -> Array.iter print_char x; print_newline ()) map;;

let draw_grids () =
  remember_mode true;
  for i = 0 to 4 do
    for j = 0 to 4 do
      draw_rect (20+100*i) (20+100*j) 100 100;
      draw_rect (540+i*20) (20+j*20) 20 20;
      draw_rect (660+i*20) (20+j*20) 20 20;
      draw_rect (540+i*20) (420+j*20) 20 20;
      draw_rect (660+i*20) (420+j*20) 20 20;
    done
  done;
  moveto (540+40) (20+40); lineto (540+60) (20+60);
  moveto (540+60) (20+40); lineto (540+40) (20+60);
  moveto (660+40) (20+40); lineto (660+60) (20+60);
  moveto (660+60) (20+40); lineto (660+40) (20+60);
  moveto (540+40) (420+40); lineto (540+60) (420+60);
  moveto (540+60) (420+40); lineto (540+40) (420+60);
  moveto (660+40) (420+40); lineto (660+60) (420+60);
  moveto (660+60) (420+40); lineto (660+40) (420+60);
  remember_mode false

let draw_card (deck : (card, coord list) Hashtbl.t) (c : card) (x : int) (y : int) (d : direction) =
  let (w,h) = text_size c in
  begin match d with
  | Up -> moveto (x+50-w/2) (y+100); draw_string c;
  | Down -> moveto (x+50-w/2) (y-h); draw_string c;
  end;
  for i = 0 to 4 do
    for j = 0 to 4 do
      draw_rect (x+i*20) (y+j*20) 20 20;
    done
  done;
  moveto (x+40) (y+40); lineto (x+60) (y+60);
  moveto (x+60) (y+40); lineto (x+40) (y+60);
  match d with
  | Up -> List.iter (fun (i,j) -> fill_rect (x+40+i*20) (y+40+j*20) 20 20) (Hashtbl.find deck c)
  | Down -> List.iter (fun (i,j) -> fill_rect (x+40-i*20) (y+40-j*20) 20 20) (Hashtbl.find deck c)

let draw_mid_card (deck : (card, coord list) Hashtbl.t) (c : card) (p : player) =
  let x, y = 600, if p = P1 then 170 else 270 in
  for i = 0 to 4 do
    for j = 0 to 4 do
      draw_rect (x+i*20) (y+j*20) 20 20
    done
  done;
  moveto (x+40) (y+40); lineto (x+60) (y+60);
  moveto (x+60) (y+40); lineto (x+40) (y+60);
  draw_card deck c x y (if p = P1 then Up else Down)

let pick_card (deck : (card, coord list) Hashtbl.t) (c : card) (left : bool) =
  let x, y = if left then 540, 20 else 660, 20 in
  set_color blue; fill_rect x y 100 100;
  set_color black; draw_card deck c x y Up

let draw_piece (p : piece) (i : int) (j : int) : unit =
  let size = if p = King then 40 else 20 in
  draw_circle (270 + 100 * i) (270 + 100 * j) size

let pick_piece (p : piece) (i : int) (j : int) : unit =
  set_color blue; fill_rect (220+100*i) (220+100*j) 100 100;
  set_color black; draw_piece p i j

let display_state deck (g, (c11, c12), cmid, (c21, c22), p) =
  synchronize ();
  let (x,y) = g.p1king in
  draw_circle (270 + 100*x) (270 + 100*y) 40;
  let (x,y) = g.p2king in
  draw_rect (234 + 100*x) (234 + 100*y) 72 72;
  List.iter (fun (x,y) -> draw_circle (270 + 100*x) (270 + 100*y) 20) g.p1pawns;
  List.iter (fun (x,y) -> draw_rect (252 + 100*x) (252 + 100*y) 36 36) g.p2pawns;
  draw_card deck c11 540 20 Up;
  draw_card deck c12 660 20 Up;
  draw_card deck c21 540 420 Down;
  draw_card deck c22 660 420 Down;
  draw_mid_card deck cmid p

(********************)
(* Input user moves *)
(********************)

let click_anywhere () : int * int =
  let stat = wait_next_event [Button_down] in
  stat.mouse_x, stat.mouse_y

let pxl_to_board (x,y) =
  ((x-20)/100-2, (y-20)/100-2)

let on_board (x,y) =
  let (i,j) = pxl_to_board (x,y) in
  -2 <= i && i <= 2 && -2 <= j && j <= 2

let on_left_card (x,y) =
  20 <= y && y <= 120 && 540 <= x && x <= 640

let on_right_card (x,y) =
  20 <= y && y <= 120 && 660 <= x && x <= 760

let click_on_left_card () =
  let p = click_anywhere () in
  if on_left_card p then true
  else if on_right_card p then false
  else raise Retry

let click_board () =
  let p = click_anywhere () in
  if on_board p then pxl_to_board p else raise Retry
