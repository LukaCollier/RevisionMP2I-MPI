(*type 'a liste = [] | (::) of 'a * 'a list *)

let list_finite = [1 ;2;3]
let rec (list_ones:int list) = 1 :: list_ones 

type 'a stream = Nil | Cons of 'a * ( unit -> 'a stream)

let rec ones = Cons (1, fun () -> ones)

let fibo =
  let rec fibo_with_internal_state a b =
    Cons ( a,fun () ->fibo_with_internal_state b (a+b))
  in
  fibo_with_internal_state 0 1


let integers =
  let rec integers_int_state a =
    Cons (a , fun () -> integers_int_state (a+1) )
  in
  integers_int_state 0

let range a b =
  let rec range_from n =
    if n<b then Cons ( n, fun () -> range_from (n+1))
    else Nil
  in
  range_from a

let hd (u: 'a stream) =
  match u with
  |Nil -> failwith "hd"
  |Cons (a,_) -> a

let tl u =
  match u with
  |Nil -> failwith "tl"
  |Cons(_,b) -> b () (* ne pas oublier () *)

let rec of_list (l: 'a list) =
  match l with
  |[] -> Nil
  |t::q -> Cons( t, fun () -> of_list q)


let rec iter (f : 'a -> unit) (t:int) (u:'a stream) =
  try if t>0 then ((hd u); iter f (t-1) (tl u))
  with Failure _ -> ()

let rec map (f :'a -> 'b) (u :'a stream) : 'b stream =
  match u with
  |Nil -> Nil
  |Cons (a,t) -> Cons (f a , fun () ->map f (t ()))


let rec zip (w: 'a stream array) : 'a array stream =
  try
    let heads = Array.map hd w in
    let tails = Array.map tl w in
    Cons(heads,fun () -> zip tails)
  with
  |Failure _  -> Nil

let rec intertwin (u: 'a stream) (v :'a stream): 'a stream =
  match u with
  |Nil -> Nil
  |Cons (a ,q) -> Cons (a, fun () -> intertwin v (q()) )

let rec product_wrong u1 u2 =
  match u1,u2 with
  | _, Nil | Nil, _ -> Nil
  | Cons(hd1, tl_begetter1), Cons(hd2, tl_begetter2) ->
     let part2 = map (fun y -> (hd1, y)) (tl_begetter2 ()) in
     Cons((hd1, hd2), fun () ->  let part1 = product_wrong (tl_begetter1 ()) u2 in intertwin part1 part2)


let stringhash (t:string) = Hashtbl.hash t

let rec ffs n =
  if n=0 then 63
  else if n mod 2 = 1 then 1
  else 1+ffs (n/2)
