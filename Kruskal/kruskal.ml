type 'a graph = 'a list * 'a edge list
and 'a edge = 'a * weight * 'a
and weight = int

(* Voici la signature du module Unionfind
 * type elem
 * val make: unit -> elem
 * val find: elem -> elem
 * val eq: elem -> elem -> bool
 * val union: elem -> elem -> unit *)
let split l =
  let rec auxsplit l a b=
    match l with
      |[] -> a , b
      |t::q -> auxsplit q (t::b) a
  in
  auxsplit l [] []
let rec fusion l1 l2=
  match l1,l2 with
    |t,[] -> t
    |[],t -> t
    |(s1,t,e1)::q,(s2,t1,e2)::q1 when t<t1 -> (s1,t,e1)::(fusion q l2)
    |(s1,t,e1)::q,(s2,t1,e2)::q1 when t1 < t -> (s2,t1,e2)::(fusion l1 q1)
let rec tri_fusion l=
  match l with
  |[]|[_] ->l
  | _ -> let l1,l2 = split l in
         let l1 = tri_fusion l1 in
         let l2 = tri_fusion l2 in
         fusion l1 l2

let kruskal (g : 'a graph) : 'a edge list =
  (* À implémenter *)
  let (vert,edg)= g in
  let sorted_edges= tri_fusion edg in
  let elements= Hashtbl.create (List.length vert) in
  List.iter(fun v ->Hashtbl.add elements v (Unionfind.make()) ) vert;
  let rec process_edges todo tree =
    match todo with
      |[]-> tree
      | (s,w,t)::q ->let e1 = Hashtbl.find elements s in
                     let e2 = Hashtbl.find elements t in
                     if Unionfind.eq (Unionfind.find e1) (Unionfind.find e2) then
                       process_edges q tree
                     else
                       begin
                         Unionfind.union e1 e2;
                         process_edges q ((s,w,t)::tree)
                       end
  in
  process_edges sorted_edges []



(* Exemple, kruskal exemple doit renvoyer
 * [(4,8,5);(0,5,4);(0,4,1);(2,2,3);(1,1,3)] *)
let exemple : int graph =
  ([0;1;2;3;4;5],
   [
     (0,4,1);
     (0,6,2);
     (0,5,4);
     (1,3,2);
     (1,1,3);
     (1,9,5);
     (2,2,3);
     (2,10,5);
     (4,8,5);
  ])

let () = Graphics.open_graph ""

let points: (int*int) list ref = ref []
let edges: (int*int) edge list ref = ref []

let draw_point (x,y) = Graphics.fill_circle x y 5
let draw_edge ((x1,y1),_,(x2,y2)) =
  Graphics.moveto x1 y1; Graphics.lineto x2 y2
let draw_tree = List.iter draw_edge

let click (status : Graphics.status) =
  let (x,y) = (status.mouse_x, status.mouse_y) in
  Graphics.clear_graph ();
  (* Ajouter les nouvelles arêtes *)
  begin
    draw_point (x,y);
    points := (x,y) :: !points;
    List.iter draw_point !points;
  end;
  (* a terminer chez soi l'affichage*)
  (* Trouver l'arbre couvrant et l'afficher *)
  ()

let () = Graphics.loop_at_exit [Graphics.Button_down] click
