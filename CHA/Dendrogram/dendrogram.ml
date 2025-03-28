open Unix
open Graphics

(* DATA TYPES *)

type tree = Leaf of string | Node of float * tree * tree
let db : (string, string list) Hashtbl.t = Hashtbl.create 101

(* UTILITIES *)

let rec filter_last = function
    (* removes last element from a list *)
  | [] -> assert false
  |[t] -> []
  |t::q-> t::filter_last q
  (* À compléter *)

let rec depth t =
  (* depth of a tree *)
  match t with
  |Leaf(s) -> 0
  |Node(s,g,d) -> let dg =depth g in
                  let dd = depth d in
                  1+ max dg dd

let rec size t =
  (* size of a tree *)
  match t with
  |Leaf(s) -> 1
  |Node(s,g,d) ->  size g + size d

(* READING DATA *)

let rec read_lines f n = match n with
  | 0 -> []
  | n -> let l = input_line f in
         match String.split_on_char ',' l with
         | t::q -> Hashtbl.add db t (filter_last q);
                   t::read_lines f (n-1)
         | _ -> assert false

let rec distp a1 a2 =
  match a1,a2 with
  |[],[] -> 0.
  |[],_|_,[] -> assert false
  |t::q,r::s -> if t <> r then 1.+. distp q s
                else distp q s

let data animal = Hashtbl.find db animal;;
let distanarb a arb =
  let d = ref max_float in
  let rec aux arb =
    match arb with
    |Leaf(s) -> let d1 = distp (data s) a in
                if !d>d1 then d:=d1
    |Node(_,g,d) -> aux g; aux d
  in
  aux arb;
  !d

let delta f1 f2 =
  let d = ref max_float in
  let rec aux arb =
    match arb with
    |Leaf(s) -> let d1 = distanarb (data s) f2 in
                if d1< !d then
                  d:=d1
    |Node(_,g,d)-> aux g;aux d
  in
  aux f1;
  !d
let rec couples l =
  match l with
  |[] -> []
  |t::q -> List.map (fun x -> (t,x)) q @ couples q

let rec closest l =
  let ajoute_dist (f1,f2) = (delta f1 f2, f1,f2) in
  let cp_d = List.map ajoute_dist (couples l) in
  List.hd (List.sort compare cp_d)
let f = open_in "zoo.data"
let animaux = read_lines f 101
let ()= close_in f


(* GRAPHICS *)

let draw str = let (w,_) = text_size str in
               draw_string str; rmoveto (-w) 0

let upto d = size_x () - 200 - int_of_float (50. *. d)

let dendrogramme tree offset =
  clear_graph (); moveto (upto 0.) offset;
  let rec dendro_aux tree x y = match tree with
    | Leaf a -> lineto (upto (-1.)) y; draw a; moveto (upto x) y
    | Node (d, tl, tr) ->
       let dy = 20 * size tl in
       lineto (upto d) y;
       dendro_aux tl d y;
       lineto (upto d) (y - dy);
       dendro_aux tr d (y - dy);
       moveto (upto x) y;
  in dendro_aux tree 2000. offset


(* CLASSIFICATION *)

let classify l =
  let familles =List.map (fun x -> Leaf x) l in
  let rec build l =
    match l with
    |[] ->assert false
    |[t] -> t
    | _ -> let (d,f1,f2) = closest l in
           build (Node (d,f1,f2) :: List.filter (fun x -> x <> f1 && x <> f2) l)
  in
  build familles
(* MAIN *)

let data animal = Hashtbl.find db animal;;

let classif = classify animaux

let () = open_graph ""

let draw tree =
  Unix.sleep 2;
  for offset = 0 to 60 do
    dendrogramme classif (size_y () - 20 + 50*offset);
    Unix.sleep 2;
  done

let () =draw classif
