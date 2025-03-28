type state = int list array

let cle (s: state) = Array.to_list s

type 'a tas ={ mutable len : int; data : 'a array;nb : int}
let couleur = 10
let tube = 12
let hauteur = 4
let pre i = (i-1)/2
let fg i= i*2 +1
let fd i = i*2 +2
let swap tab i j =
  let tmp = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <-tmp

let get h i=
  h.data.(i)

let rec bubble t i=
  if i > 0 && (t.data.(i)) < (t.data.(pre i)) then
    swap t.data i (pre i) ; bubble t (pre i)


let rec sift_down t i =
  let j' = fg i in
  let j''= fd i in
  let j = if j' < t.len && get t j' < get t i then j' else i in
  let j = if j'' < t.len && get t j'' < get t i then j'' else j in
  if  j <> i then
    begin
      swap t.data j i;
      sift_down t j;
    end


let create n x :'a tas  =
  {len = 0 ; data =Array.make n x; nb = n}

let add  t (x:'a) =
  assert( t.len < t.nb);
  t.data.(t.len) <- x;
  bubble t t.len;
  t.len <- t.len +1

let pop t =
  assert(t.len >0);
  t.len <- t.len -1;
  swap t.data 0 t.len;
  sift_down t 0;
  t.data.(t.len)

let heuristique (s:state)=
  let rec phases l=
    match l with
      |[] -> 0
      |a::b:: c when a=b -> phases (b::c)
      |t::q -> 1+phases q
  in
  let cpt = ref (-couleur) in
  for i = 0 to  tube -1 do
    cpt := !cpt + phases s.(i)
  done;
  !cpt


let random_state () =
  let sample = Array.init (hauteur * couleur) (fun i -> i/hauteur) in
  for i =1 to (hauteur * couleur -1) do
    swap sample i (Random.int (i+1))
  done;
  let state = Array.make tube [] in
  for i=0 to couleur -1 do
    for j=0 to hauteur -1 do
      state.(i) <- sample.(hauteur*i+j)::state.(i)
    done
  done;
  state

let compatible t1 t2 = (* verifie pour pouvoir renverser de t1 vers t2 *)
  match t1,t2 with
    |[],_-> false
    |_,[] -> true
    |_,[a;b;c;d] -> false
    |a1::b1 , a2::b2 -> a1=a2

let rec verse t1 t2=
  match t1,t2 with
  |[],_ | _, _::_::_::_::[] -> t1,t2
  |a1::b1, a2::b2 -> if a1 =a2 then verse b1 (a1::t2)
                     else t1,t2
  |a::b, [] -> verse b [a]

let voisins (s:state) =
  let voisins =  ref [] in
  for i=0 to tube -1 do
    for j= 0 to tube -1 do
      if i <>j && compatible s.(i) s.(j) then
        begin
          let v = Array.copy s in
          let (ti,tj) = verse s.(i) s.(j) in
          v.(i) <-ti;
          v.(j) <- tj;
          voisins:= v:: !voisins
        end
    done
  done;
  !voisins

exception Found of int

let astar (s:state) =
  let dist = Hashtbl.create 100 in
  let f =create 10000 (0,s,0) in
  add f (0,s,0);
  try
    while f.len <> 0 do
      let (_,u,d)= pop f in
      if heuristique u = 0 then raise (Found d);
      Hashtbl.add dist u d;
      let traite v = if not (Hashtbl.mem dist v) then add f  (d+1+heuristique v, v,d+1) in
      List.iter traite (voisins u);
    done;
    failwith "pas de solution"
  with Found n -> n 
