let insertinl l x =
    let rec aux l x =
        match l with
        |[] -> [x]
        |t::q -> if t>x then (x)::(t)::q
        else t::(aux q x)
    in
    aux l x