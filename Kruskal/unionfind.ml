type elem = content ref

and content =
  | Link of elem (* parent *)
  | Root of rank (* rank *)

and rank =
  int

(* Crée un nouveau singleton *)
let make () : elem = ref (Root 0)

(* Trouve le représentant de l'ensemble de x *)
let rec find (x : elem) : elem =
  match !x with
  | Link y -> let r = find y in x := Link r; r
  | Root _ -> x

(* Teste l'égalité entre deux éléments *)
let eq (x : elem) (y : elem) : bool =
  x == y

(* Effectue l'union de deux ensembles désignés par leurs représentants *)
let link (x : elem) (y : elem) : unit =
  match !x, !y with
  | Root i, Root j ->
     if i > j then
       y := Link x
     else if i < j then
       x := Link y
     else begin
         x := Link y; y := Root (j+1)
       end
  | _ -> assert false

(* Effectue l'union de deux ensembles désignés par un élément quelconque *)
let union (x : elem) (y : elem) : unit =
  link (find x) (find y)
