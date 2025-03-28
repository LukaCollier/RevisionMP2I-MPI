type operator = Or | And | Imp | Eq
type formula =
  | Var of int
  | Neg of formula
  | Bin of formula * operator * formula

type cnf = int list list
let rec var_max = function
    |Var n -> n
    |Neg f -> var_max f
    |Bin(f1,_,f2) -> let n1=var_max f1 in
                     let n2=var_max f2 in
                     max n1 n2

let rec clause op n i j: cnf=
  match op with
   |And ->[[-n;i];[-n;j];[-i;-j;n]]
   |Or ->[[-i;n];[-j;n];[i;j;-n]]
   |Imp -> clause Or n (-i) j
   |Eq -> clause Imp n i j @ clause Imp n j i
let tseitin (f:formula): cnf =
  let rec tseitin_aux (n:int) : formula -> int * cnf =
    (*n est le numéro de la prochaine variable non utilisée *)
    function
    | Var(a) -> (a,[])
    | Neg(fml) ->
       let (x,clauses)= tseitin_aux n fml in
       let y=max n (x+1) in
       (y,[[x;y];[-x;-y]] @ clauses)
    | Bin(f1,op,f2) -> let (x1,clauses1)=tseitin_aux n f1 in
                       let y= max n (x1+1) in
                       let (x2,clauses2)=tseitin_aux y f2 in
                       let z= max y (x2+1) in
                       (z,(clause op z x1 x2)@clauses1@clauses2)
  in
  let (n,clauses) =tseitin_aux (var_max f +1) f in
  [n]:: clauses

let test= Bin(Bin(Var(1),Or,Var(2)),And,Neg(Var(3)))
