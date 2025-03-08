type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree ;;

let rec hauteur t =
  match t with
  |Leaf(a) -> 0
  |Node(_,a,b) -> 1 + max (hauteur a)( hauteur b)
;;

let rec nb_node t =
  match t with
  |Leaf(a) -> 1
  |Node(_,a,b) -> 1 + (nb_node a) + (nb_node b)
                                    
type 'a tree_g = Vide | Node of 'a * ('a tree_g) list;;

let rec hauteur_g t =
  match t with
  | Vide -> 0
  | Node(_, l) -> 
      let max_h = List.fold_left max 0 (List.map hauteur_g l) in
      max_h + 1 ;;

let rec nb_node_g t =
  match t with 
  |Vide -> 0
  |Node (_,l) -> 1+List.fold_left (fun acc q -> acc + nb_node_g q) 0 l
