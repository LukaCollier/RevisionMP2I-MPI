open Graphics;;

(* Global variables *)
let go_mode = ref false;;
let points = ref [];;
let k = int_of_string Sys.argv.(1);;
let reps = Array.make k (-1, -1);;
let vreps = Array.make k [];;

(* Distance euclidienne *)
let dist (a, b) (c, d) =
  let a1 = c - a in
  let b1 = d - b in
  a1 * a1 + b1 * b1;;

(* Représentant le plus proche *)
let nearest_rep tab (a, b) =
  let rec aux i tmpd m =
    if i >= Array.length tab then m
    else
      let d = dist tab.(i) (a, b) in
      if d < tmpd then aux (i + 1) d i else aux (i + 1) tmpd m
  in
  if Array.length tab = 0 then failwith "Empty array in nearest_rep";
  aux 0 max_int 0;;

let get_random_int n = Random.int n;;

let classe (x, y) = nearest_rep reps (x, y);;

let barycentre l =
  let rec aux x y count = function
    | [] -> if count = 0 then (-1, -1) else (x / count, y / count)
    | (a, b) :: q -> aux (x + a) (y + b) (count + 1) q
  in
  aux 0 0 0 l;;

(* Display functions *)
let color =
  let colortable = Array.init k (fun _ -> rgb (Random.int 256) (Random.int 256) (Random.int 256)) in
  fun i -> colortable.(i);;

let draw_point c (x, y) =
  set_color c;
  fill_circle x y 5;
  set_color black;;

let draw_rep c (x, y) =
  set_color c;
  fill_rect (x - 2) (y - 2) 10 10;
  set_color black;;

let display_points () = List.iter (draw_point black) !points;;

let draw_class (x, y) = draw_point (color (classe (x, y))) (x, y);;

let display_state () =
  clear_graph ();
  for i = 0 to k - 1 do
    draw_rep (color i) reps.(i)
  done;
  List.iter draw_class !points;;

(* k-means step-by-step *)
let init () =
  let n = List.length !points in
  if k > n then failwith "Number of clusters exceeds number of points";
  let selected = Hashtbl.create k in
  let rec pick i =
    if i >= k then ()
    else
      let m = get_random_int n in
      let p = List.nth !points m in
      if not (Hashtbl.mem selected p) then (
        reps.(i) <- p;
        Hashtbl.add selected p true;
        pick (i + 1)
      ) else pick i
  in
  pick 0;;

let next_step () =
  Array.iteri (fun i _ -> vreps.(i) <- []) vreps;
  List.iter (fun p ->
      let i = classe p in
      vreps.(i) <- p :: vreps.(i)
    ) !points;
  Array.iteri (fun i _ ->
      let ni = barycentre vreps.(i) in
      reps.(i) <- ni
    ) reps;;

(* Main loop *)
let manage_events status =
  if status.keypressed && status.key = 'g' then (
    go_mode := true;
    init ();
    display_state ()
  ) else if !go_mode && status.button then (
    next_step ();
    display_state ()
  ) else if status.button then (
    let p = (status.mouse_x, status.mouse_y) in
    points := p :: !points;
    draw_point black p
  );;

open_graph "";;
loop_at_exit [Button_down; Key_pressed] manage_events;;
