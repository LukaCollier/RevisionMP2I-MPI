type 'a langage_regulier=
  | Eps (* Singleton {eps}*)
  | Star of 'a langage_regulier (* Etoile de kleene*)
  | Union of 'a langage_regulier * 'a langage_regulier (*Concaténation*)
  | Cat of 'a langage_regulier * 'a langage_regulier (*Concaténation*)
  | Sing of 'a (*Singleton avec mot de  lettre*)
  |Empty

type cardinal =Finite  |Infinite

let rec is_empty=
  function
  |Eps -> false
  |Sing(a) -> false
  |Empty -> true
  |Cat(l1,l2)-> is_empty l1 || is_empty l2
  |Union(l1,l2) ->  is_empty l1 && is_empty l2
  |Star(a) -> false
(* L1 uL2 ={ eps} -> L1=L2={eps} || L1 ={eps} L2=Empty || L2={eps} L1=Empty*)
(* L1L2={eps} -> L1=L2={eps} *)
let rec is_eps = function
  |Eps -> true
  |Union(l1,l2)->let a =is_eps l1 in
                 let b=is_eps l2 in
                 let c =is_empty l1 in
                 let d =is_empty l2 in
                 a && b || a && d || b && c
  |Sing(a) -> false
  |Star(l) -> is_eps l
  |Cat(l1,l2) -> is_eps l1 && is_eps l2
  |Empty -> false

let rec has_eps = function
  |Eps -> true
  |Union(l1,l2)->has_eps l1 || has_eps l2
  |Sing(a) ->false
  |Star(l) -> true
  |Cat(l1,l2) -> has_eps l1 && has_eps l2
  |Empty -> false


let rec card_lr =
  function
    |Eps -> 0
    |Sing(a) -> 1
    |Union(l1,l2) -> card_lr l1 + card_lr l2
    |Cat(l1,l2) ->  (card_lr l1 * card_lr l2)
    |Star(a) -> 1
    |Empty -> -1
let rec card =function
  |Eps |Sing _ |Empty -> Finite
  |Star l -> if is_eps l || is_empty l
             then Finite
             else Infinite
  |Union (l1,l2) -> if card l1=Infinite || card l2 =Infinite then Infinite
                    else Finite
  |Cat(l1,l2)-> if is_empty(Cat(l1,l2)) then Finite
                else if card l1 = Finite && card l2 =Finite
                then Finite
                else Infinite

let rec mirror =function
  |Empty -> Empty
  |Eps -> Eps
  |Union(l1,l2) -> Union(mirror l2 ,mirror l1)
  |Cat(l1,l2) -> Cat(mirror l2 , mirror l1)
  |Sing a -> Sing a
  |Star a -> Star a












let rec logique a b=
  match a,b with
    |true,true -> if a=true && b=true then true
                  else true
    |true,false -> if a=true && b=false then true
                  else true
    |false,true -> if b=true && a=false then true
                  else true
    |_-> not(logique true true)
