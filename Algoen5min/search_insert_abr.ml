type 'a tree = Vide | Node of 'a * 'a tree * 'a tree ;;

let rec mem t m =
  match t with
  |Vide -> false
  |Node(a,b,c) -> if a = m then true else if m > a then mem c m else mem b m
;;

let rec insert t m =
  match t with
  |Vide-> Node ( m, Vide,Vide)
  |Node(a,g,d) -> if a<m then Node(a,g, insert d m)
      else Node(a,insert g m , d)