type exemple = {lbl : string; attr : float array}

let parse line =
  match String.split_on_char ',' line with
  | a::b::c::d::e::[] ->
     { lbl = e; attr = Array.map float_of_string [|a;b;c;d|] }
  | _ -> assert false

let rec read f =
  try let line = input_line f in
      parse line :: read f
  with End_of_file -> []

let data =
  let f = open_in "iris.data" in
  let data = read f in
  close_in f; data

(* TODO *)

let distance a1 a2 =
  let da=Array.map2 (-.) a1 a2 in
  let dasqr = Array.map (fun x -> x*.x) da in
  let somme = Array.fold_left (+.) 0.0 dasqr in
  sqrt somme

let mediane l =
  let n = List.length l in
  let sorted=List.sort compare l in
  List.nth sorted (n/2)

(* type 'a array kdtree = *)

let rec split l x =
  match l with
  |[] -> [],[]
  | t::q -> let l1,l2 = split q x in
            if t<x then
              (t::l1),l2
            else if t> x then
              l1,(t::l2)
            else
              l1,l2

type kdtree = Empty | Node of (kdtree * exemple * kdtree)
let kdtree l =
  let rec aux l i =
    match l with
    |[] -> Empty
    |_ ->
      let addxi x = x.attr.(i mod (Array.length x.attr)),x in
      let xix = List.map addxi l in
      let r= mediane xix in
      let xg,xd = split xix r in
      let g,d= List.map snd xg ,List.map snd xd in
      Node (aux g (i+1),snd r, aux d (i+1))
  in
  aux l 0

let addxi i x  = x.attr.(i mod (Array.length x.attr)),x
let distpl i r a =
  let k = Array.length a in
  Float.abs (a.(i mod k) -. r.(i mod k)) (* @@ meme chose que de faire ' ' juste prio plus faible Float.abs @@ a.(i mod k) -. ri *)

let closest a t =
  let rec closer i best dmin  =
    function
      |Empty -> best,dmin
      |Node(g,r,d) ->
        let first, second = if addxi i {lbl = ""; attr = a} <= addxi i r then g, d else d,g in
        let best' ,dmin' = closer (i+1) best dmin first in
        if dmin' <= distpl i r.attr a then
          best' , dmin'
        else
          let d_ar = distance a r.attr in
          let best'', dmin'' = if d_ar <dmin' then r , d_ar
                               else best', dmin'
          in
          closer (i+1) best'' dmin'' second
  in
  match t with
  |Empty -> raise (Invalid_argument "Empty tree")
  |Node (g,r,d) -> closer 0 r (distance a r.attr) t
(* type fileprio = *)
let l =

