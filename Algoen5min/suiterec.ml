(*exemple fib*)
let fib n =
  let hash= Hashtbl.create n in
  Hashtbl.add hash 0 0; Hashtbl.add hash 1 1;
  let rec aux n =
    try
      Hashtbl.find hash n
    with
    |_ -> 
        let n1 = aux (n-1) in
        let n2 = aux (n-2) in 
        Hashtbl.add hash  n (n1+n2);Hashtbl.find hash n

  in
  aux n