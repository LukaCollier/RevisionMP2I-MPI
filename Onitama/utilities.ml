(************************************)
(* Fonctions generiques utilitaires *)
(************************************)

let sample list = match list with
  | [] -> raise Not_found
  | t::q ->
     let rec cur acc reste i = function
       | [] -> (acc, reste)
       | t::q ->
          if Random.int i = 0 then
            cur t (acc::reste) (i+1) q
          else
            cur acc (t::reste) (i+1) q
     in cur t [] 2 q;;

let rec filter_out x = function
  | [] -> []
  | t::q ->
     if t = x then
       q
     else
       t::filter_out x q;;

let rec depile l = function
  | [] -> l
  | t::q -> depile (t::l) q;;
