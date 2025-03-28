type auto_eps = {
    tr : int Abr.abr array array;
    eps_tr : int Abr.abr array;
    init : int Abr.abr;
    fin : int Abr.abr;
  }
(* tr.(i).(j) is the set of possibles transitions from state i reading letter j *)
(* eps_tr.(i) is the set of possibles epsilon transitions from state i *)

type auto = {
    tr : int Abr.abr array array;
    init : int Abr.abr;
    fin : int Abr.abr;
  }

let exemple : auto_eps = { (* From Q18 *)
    tr = [|
           [| (* Reading a from state 1 *)
             Abr.union (Abr.leaf 1) (Abr.leaf 0);
             (* Reading b from state 1 *)
             Abr.leaf 2 |];
           [| Abr.leaf 3; Abr.leaf 2 |];
           [| Abr.leaf 4; Abr.Empty|];
           [| Abr.leaf 5; Abr.leaf 4 |];
           [| Abr.union (Abr.leaf 4) (Abr.leaf 5); Abr.leaf 1 |];
           [| Abr.Empty; Abr.leaf 5 |]
         |];
    eps_tr=[|
              Abr.Empty;
              Abr.union (Abr.leaf 2) (Abr.leaf 3);
              Abr.Empty;
              Abr.leaf 4;
              Abr.leaf 5;
              Abr.Empty
            |];
    init = Abr.leaf 0;
    fin = Abr.leaf 5
  }

let eps_accessible (auto : auto_eps) (q : int) : int Abr.abr =
  (* les annotations de types évitent des problèmes dus aux noms égaux des champs *)
  let n=Array.length auto.tr in
  let vtab =Array.make n false in
  let rec parcours l=
    match l with
    |[] -> vtab
    |t::q ->
      if not vtab.(i) then
        begin
          vtab.(i) <- true;
          let abr= auto.eps_tr.(i) in
          Arb.iter
let remove_eps (auto : auto_eps) : auto =
  assert false

let reconnait (w : int list) (auto : auto) : bool =
  assert false
