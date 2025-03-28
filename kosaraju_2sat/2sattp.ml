type cnf = int * (int * int) list
(* (n, clauses) | neg (i) = 2 * n - 1 - i *)
type graph = int list array

let example : cnf = (3,[(0,1);(1,2);(4,0);(3,5)])

let examplebis : cnf = (3,[(0,1);(1,2);(4,0);(3,5);(4,5)])

let target = (100,[(85,144);(41,182);(100,39);(20,104);(170,121);(85,17);(78,149);(12,15);(6,1);(69,28);(193,89);(23,14);(144,123);(3,96);(170,100);(12,49);(11,155);(197,157);(60,97);(163,183);(199,120);(170,159);(43,84);(146,20);(118,154);(158,188);(51,24);(69,109);(86,176);(122,38);(152,65);(83,19);(140,182);(35,70);(191,136);(124,168);(156,22);(128,35);(33,36);(71,153);(95,69);(92,157);(146,24);(133,78);(189,88);(158,95);(1,193);(113,56);(13,129);(17,0);(171,186);(148,24);(11,36);(32,71);(49,65);(50,131);(158,169);(83,12);(96,101);(61,9);(129,121);(97,181);(176,183);(131,30);(39,154);(14,124);(167,189);(87,168);(26,99);(76,121);(195,193);(63,168);(73,162);(77,36);(23,98);(11,140);(176,138);(170,120);(37,155);(190,88);(7,10);(43,51);(95,4);(61,93);(132,157);(92,153);(131,82);(53,83);(165,7);(45,160);(117,166);(54,151);(45,0);(113,166);(60,167);(118,31);(142,183);(11,129);(68,87);(106,147);(170,7)])

let print (n,c) = Printf.printf "(%d,[%s])" n (List.fold_left (fun acc (x,y) -> acc ^ "(" ^ string_of_int x ^ ","^string_of_int y^");") "" c)

let cnf_gen (n:int) (m:int) : cnf =
  (* generates a random formula with m clauses over n variables *)
  let rec gen_clause m =
    if m = 0 then []
    else let x, y = Random.int (2*n), Random.int (2*n) in
         if x = y || x + y = 2*n-1 then gen_clause m else
           (x,y)::gen_clause (m-1)
  in (n,gen_clause m)

(* Code à compléter *)
let rec mem g i =
  match g with
  |[]-> false
  |t::q -> if t = i then true
           else mem q i

let add (g : graph) (i : int) (j : int) =
  (* adds edge (i,j) to graph g *)
  if mem g.(i) j then
    ()
  else
    g.(i) <- (j::g.(i))

let implication_graph (n, clauses : cnf) : graph =
  let g =Array.make (2*n) [] in
  let neg i = 2*n - i - 1 in
  List.iter (fun (t,q) -> add g t (neg q) ; add g (neg t) q) clauses;
  g


(*let transpose (g : graph) n : graph =
  let g1 = Array.make n [] in
  let ajtv u v =
    let voisins = g.(u) in
    g1.(u) <- (v::voisins)
  in
  let tmp = ref 0 in
  Array.iter (fun t -> (List.iter (fun v1 -> ajtv (!tmp) v1) t; incr tmp )) g;
  g1*) 
let transpose (g : graph) n : graph =
  let g1 = Array.make n [] in
  for i=0 to n-1 do
    List.iter (fun t -> add g1 t i) g.(i)
  done;
  g1

let rec makel n l =
    if n < 0 then l
    else makel (n-1) (n::l)

let topological_sort (g : graph) : int list =
  let n = Array.length g in
  let vus = Array.make n false in
  let l = makel (n-1) [] in
  let rec parcours acc s =
    if vus.(s) then acc
    else
      begin
        vus.(s) <- true;
        s:: parcours_list acc g.(s)
      end ;
  and parcours_list acc l =
    match l with
    |[] -> acc
    | t::q -> parcours_list (parcours acc t) q
  in
  parcours_list [] l
(*let kosaraju  (g : graph) : int array = (* failed *)
  let n = Array.length g in
  let g1 = transpose g n in
  let vus = Array.make n false in
  let topo = topological_sort g in
  let rec dfs s i =
    if vus.(i) then s
    else
      begin
        vus.(i) <- true;
        i:: List.fold_left dfs s (g1.(i));
      end
  in
  let parcours_root acc r =
    match dfs [] r with
    | [] -> acc
    | l -> l::acc
  in
  List.fold_left parcours_root [] topo (*erreur *)
*)

let kosaraju  (g : graph) : int array =
  let n = Array.length g in
  let g1= transpose g n in
  let vus = Array.make n false in
  let topo = topological_sort g in
  let scc = Array.make n false in
  let cpt = ref 0 in
  let rec parcours c s =
    if vus.(s) then c
    else
      begin
        vus.(s) <- true;
        
let sat (n, clauses as f: cnf) : bool =
  assert false (* TODO *)
