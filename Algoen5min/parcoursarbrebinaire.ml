type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree ;;

let parcourlargeur t =
  let rec aux q = 
    if Queue.is_empty q then ()
    else let t = Queue.pop q in
      match t with
      |Leaf (a) -> print_int a; aux q
      |Node (a,b,c) -> Queue.push b q; Queue.push c q; print_int a ; aux q
  in
  let q = Queue.create () in  
  Queue.push t q;
  aux q
;;


let parcourlargeur_v2 t =
  let q = Queue.create () in  
  Queue.push t q;
  
  let rec aux acc =
    if Queue.is_empty q then List.rev acc  
    else
      let t = Queue.pop q in
      match t with
      | Leaf a -> aux (a :: acc)
      | Node (a, b, c) -> 
          Queue.push b q;
          Queue.push c q;
          aux (a :: acc)
  in
  aux [];;

let parcoursprofondeur t =
  let p = Stack.create () in
  Stack.push t p;
  let rec aux acc =
    if Stack.is_empty p then List.rev acc
    else
      let t = Stack.pop p in 
      match t with
      | Leaf (a )-> aux (a :: acc)
      | Node (a, b, c) -> 
          Stack.push b p;
          Stack.push c p;
          aux (a :: acc)
  in
  aux [];;

let rec prefixe t =
  match t with
  |Leaf a -> print_int a
  |Node(a,b,c) -> print_int a; prefixe b; prefixe c
;;
let rec infixe t =
  match t with
  |Leaf a -> print_int a
  |Node(a,b,c) -> prefixe b;print_int a; prefixe c
;;
let rec postfixe t =
  match t with
  |Leaf a -> print_int a
  |Node(a,b,c) -> prefixe b; prefixe c;print_int a
;;
  