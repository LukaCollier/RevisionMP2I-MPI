type ('a,'b) transitions= ('a,(('b * 'a) list)) Hashtbl.t

type ('a,'b) automate = { i: 'a; transitions: ('a,'b) transitions; f: 'a list}   (*'a et 'b représente implicitement Q et sigma *)

let is_digit c = '0' <= c && c <= '9'

let digit c = int_of_char c - int_of_char '0'
let code d= char_of_int (d + int_of_char '0')
let exemple : (int,char) automate =
  let tr= Hashtbl.create 7 in
  let voisin i=
    let v= ref [] in
    for j=0 to 9 do
      v:= (code j,(i*10 + j ) mod 7):: !v
    done;
    !v
  in
  for i=0 to 6 do
    Hashtbl.add tr i (voisin i)
  done;
  {i= 0 ; transitions= tr ; f= [0]}

let rec find a= 
  function
    |[] -> None
    |(j,t)::q -> if j=a then Some t
                 else find a q
let delta (auto : ('a,'b) automate) (q:'a) (a : 'b) : 'a option=
  try
    let voisins=Hashtbl.find auto.transitions q in
    let q=find a voisins in
    q
  with
    Not_found -> None

let rec delta_star (auto : ('a,'b) automate) (q :'a) (w:'b list) :'a option=
  match w with
  |[] -> Some q
  |x::u -> let nq=delta auto q x in
           match nq with
             |None -> None
             |Some a -> delta_star auto a u


let rec mem x = function
  |[] -> false
  |t::q -> if t=x then true
           else mem x q

let reco auto (w: 'b list) :bool =
  let q= delta_star auto auto.i w in
  match q with
    |None -> false
    |Some a -> mem a auto.f

let str_to_charlist str=
  let acc = ref [] in
  for i=String.length str - 1 downto 0 do
    acc := str.[i] :: !acc
  done;
  !acc

let test s =reco exemple (str_to_charlist s)


type 'a graph=('a,('a list)) Hashtbl.t


let graph_of_auto (auto:('a, 'b) automate) :'a graph=
  let tr=Hashtbl.create 7 in
  Hashtbl.iter (fun k v -> Hashtbl.add tr k (List.map snd v)) auto.transitions; (* snd prends le second éléments d'un 2- uplet *)
  tr

let list_of_hash ht =
  Hashtbl.fold (fun k v acc -> k :: acc ) ht []

let dfs g b =(*algo de parcours en profondeur*) (*O(|A|+|b|) *)
  let dejavu= Hashtbl.create 1 in
  let rec parcours v =
    Hashtbl.add dejavu v true;
    if Hashtbl.mem dejavu v then
      let voi=Hashtbl.find g v in
      List.iter parcours_opt voi
  and parcours_opt v=
    if not (Hashtbl.mem dejavu v) then parcours v
  in
  List.iter (fun t -> parcours t ) b;
  list_of_hash dejavu

let transpose (g:'a graph) : 'a graph= (*O(|A|)*)
  let g1 = Hashtbl.create 1 in
  let ajts u v =
    let voisins =
      try
        Hashtbl.find g1 u;
      with
        Not_found -> [] 
    in
    Hashtbl.replace g1 u (v::voisins)
  in
  Hashtbl.iter (fun s v -> (List.iter (fun v1 ->ajts v1 s) v ) ) g ;
  g1




let accessible auto=
  let gauto = graph_of_auto auto in
  dfs gauto [auto.i]

let coaccessible auto =
  let gauto = graph_of_auto auto in
  let gautoT = transpose gauto in
  dfs gautoT auto.f


let emonder auto = (* a refaire à la maison *)
  let acc = accessible auto in
  let coacc= coaccessible auto in
  let est_ok q = mem q acc && mem q coacc in
  let tr_v (a,q) = est_ok q in
  let tr =auto.transitions in
  Hashtbl.iter (fun k v -> if est_ok k then Hashtbl.add tr k (List.filter tr_v v) ) auto.transitions;
  {i=auto.i ; transitions=tr; f=List.filter est_ok auto.f}
