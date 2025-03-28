(* Arbres binaires de recherches, utilisés pour représenter des ensembles sans doublons *)
type 'a abr = Empty | Node of ('a abr * 'a * 'a abr)

let leaf x = Node (Empty, x, Empty)

let rec iter (f : 'a -> unit) (a : 'a abr) : unit =
  match a with
  |Empty -> ()
  |Node(sag,n,sad) ->iter f sag ;iter f sad; f n

let rec mem x a =
  match a with
  |Empty -> false
  |Node(g,s,d) -> x=s || mem x g || mem x d
let rec add n a =
    match a with
    |Empty -> Node(Empty,n,Empty)
    |Node(g,x,d) -> if x>n then
                      Node(add n g,x,d)
                    else
                      if n>x then
                        Node(g,x,add n d)
                      else
                        a
let union (a1 : 'a abr) (a2 : 'a abr) : 'a abr =
  (* Ne pas hésiter à ajouter une ou plusieurs fonctions intermédiaires *)
  let rec aux a1 a2 =
    match a2 with
    |Empty -> a1
    |Node(g,x,d) ->let a1 = add x a1 in
                   let a1 = aux a1 g in
                   aux a1 d
  in
  match a1,a2 with
  |Empty,_ ->a2
  |_,Empty -> a1
  |_,_ -> aux a1 a2

let creer_arb l=
  match l with
  |[] -> Empty
  |t::q -> let a = ref(Node(Empty,t,Empty)) in
           List.iter (fun x ->a:= add x !a) q;
           !a

let intersection (a1 : 'a abr) (a2 : 'a abr) : 'a abr =
  let l =ref [] in
  iter (fun x -> if mem x a1 then l:=x::!l) a2;
  creer_arb !l
