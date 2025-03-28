open Gameengine
open Types
(**********************)
(* AI decision making *)
(**********************)

let pick_move1 deck state =
  let moves = all_moves deck state in
  let rec sample acc i s = match s () with
    | Seq.Nil -> acc
    | Seq.Cons (t,q) ->
       if Random.int i = 0 then
         sample t (i+1) q
       else
         sample acc (i+1) q
  in match moves () with
     | Seq.Nil -> assert false
     | Seq.Cons (t,q) -> sample t 2 q
;;
let rec mem x s =
  match s () with
  |Seq.Nil -> false
  |Seq.Cons(t,q) -> x=t || mem x q
let player (_,_,_,_,p) = p
let rec eval deck state depth alpha beta =
  if depth = 0 then 0 (*heuristique *)
  else
    let moves = all_moves deck state in
    let p = player state in
    eval_seq deck moves (depth-1) p  alpha beta 

and eval_seq deck s depth p alpha beta =
  match p with
  |P1 ->
    (match s () with
    |Seq.Nil  -> alpha
    |Seq.Cons(P1win, _ )-> beta
    |Seq.Cons(P2win,q)-> eval_seq deck q  depth p  alpha beta 
    |Seq.Cons(Playing st,q) -> let bb=  eval deck st depth alpha beta in 
                               if bb >= beta then beta 
                               else eval_seq deck q depth p (max alpha bb) beta 
    )
  |P2 -> (match s () with
    |Seq.Nil  -> beta
    |Seq.Cons(P2win, _ )-> alpha
    |Seq.Cons(P1win,q)-> eval_seq deck q  depth p  alpha beta 
    |Seq.Cons(Playing st,q) ->let bb = (eval deck st depth  alpha beta ) in
                              if bb <= alpha then alpha
                              else eval_seq deck q depth p  alpha ( min bb beta )
    )

let pick_move deck state =
  let moves = all_moves deck state in
  if mem P2win moves then P2win
  else
    let rec choix acc s =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons (t,q) ->
         (*if t = P2win then t
           else choix t q*)
         match t with
         |P2win -> t
         |P1win -> choix acc q
         |Playing st -> if (mem P1win (all_moves deck st)) then choix acc q
                        else Playing st
    in
    match moves () with
    | Seq.Nil -> assert false
    | Seq.Cons (t,q) -> if t=P2win then t
                        else choix t q

let d = 1
let alp = -5
let bet=  5
let pick_move deck state =
  let moves = all_moves deck state in
  if mem P2win moves then P2win
  else
    let rec choix acc score s =
      match s () with
        |Seq.Nil -> acc
        |Seq.Cons (Playing st, q) -> let m = eval deck st d alp bet in
                                     if m < score then choix (Playing st) m  q
                                     else choix acc score s
        |Seq.Cons (_,q) -> choix acc score q 
    in
    match moves () with
    |Seq.Cons (Playing st,q) ->
      choix (Playing st) (eval deck st d alp bet) q
    | _ -> assert false

