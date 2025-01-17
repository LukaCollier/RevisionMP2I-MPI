open Graphics;;

(* Global variables *)
let go_mode = ref false;;
let points = ref [];;
let k = int_of_string Sys.argv.(1);;
let reps = Array.make k (-1, -1)
let vreps = Array.make k []
(*distance euclidienne *)
let dist (a,b) (c,d) =
  let a1= c-a in
  let b1 = d-b in
  a1*a1+b1*b1
(*representant le plus proche *)
let nearest_repvl l (a,b) =
  let rec aux l tmp tmpd =
    match l with
    |[] -> tmp
    |t::q -> let d = dist t (a,b) in
             if tmpd>d then aux q t d
             else aux l tmp tmpd
  in
  match l with
  |[] -> assert false
  |t::q -> let d = dist t (a,b) in
           aux q t d

let nearest_rep tab (a,b) k =
  let rec aux tab i tmpd m =
    if i>=k then (*tmp*) m
    else let d = dist tab.(i) (a,b) in
         if tmpd>d then aux tab (i+1) d i
         else aux tab (i+1) tmpd m
  in
  let t= tab.(0) in
  let d = dist t (a,b) in
  let i =aux tab 1 d 0 in
  vreps.(i) <-(a,b)::vreps.(i); i
let getrint n = Random.int (n-1)
let classe (x,y)= nearest_rep reps (x,y) k

let barycentrel l =
  let rec aux x y l =
    match l with
    |[] -> (x , y)
    |(a,b)::q -> aux (x+a) (y+b) q
  in
  let (x,y) =aux 0 0 l in
  let n =List.length l in
  (x/n,y/n)
(* Display functions *)
let color=
  let colortable = Array.init k (fun i -> rgb (Random.int 256) (Random.int 256) (Random.int 256)) in
  fun i -> colortable.(i)
let draw_point c (x,y) =
  set_color c; fill_circle x y 5; set_color black;;
let draw_rep c (x,y) =
  set_color c; fill_rect (x-2) (y-2) 10 10; set_color black;;
let display_points () = List.iter (draw_point black) !points;;
let draw_class (x,y) = draw_point (color (classe (x,y))) (x,y)
let display_state () = clear_graph();
                       for i = 0 to k-1 do
                         draw_rep (color i) reps.(i)
                       done;
                       List.iter draw_class !points

(* k-means step-by-step *)
let init () = 
  let n =List.length !points in
  let i= ref 0 in
  assert(k < n);
  while !i < k do
    let m = getrint n in
    let p = List.nth !points m in
    if not(Array.mem p reps) then reps.(!i) <- p; incr i
  done
let next_step () =
  (*let flag = ref true in*)
  for i=0 to k-1 do
    let ni = barycentrel vreps.(i) in
    if ni <> reps.(i) then begin
        reps.(i) <- ni;
        vreps.(i) <- [];
      end
    else vreps.(i) <-[]
  done


(* main *)
let manage_events status =
  if status.keypressed && status.key = 'g' then
    begin
      go_mode := true;
      init ();
      display_state ();
    end
  else if !go_mode && status.button then
    begin
      next_step ();
      display_state ();
    end
  else if status.button then
    begin
      let p = (status.mouse_x, status.mouse_y) in
      points := p::!points;
      draw_point black p;
    end
;;

open_graph "";;
loop_at_exit [Button_down; Key_pressed] manage_events;;
