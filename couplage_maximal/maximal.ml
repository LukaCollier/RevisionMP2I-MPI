let p=0.3
let rand_bool ()= Random.float 1. <p
let shuffle l= List.sort (fun x y -> Random.int 3 - 1) l
(*creation du graph aléatoire *)
let graph_alea n =
  let edges= ref [] in
  for i=0 to n do
    for j=i+1 to n-1 do
      if rand_bool() then edges:= (i,j)::!edges
    done;
  done;
  !edges

let exemple=graph_alea 50
let ex= shuffle exemple
let couplage_maximal g n=
  let deja_vu=Array.make n false in
  let rec acc g l=
    match g with
      | [] -> l
      | (a,b)::q -> if deja_vu.(a) || deja_vu.(b) then acc q l
                    else begin
                        deja_vu.(a)<-true;
                        deja_vu.(b)<-true;
                        acc q ((a,b)::l)
                      end
  in
  acc g []

let b () =
  let g=graph_alea 500 in
  for i=0 to 100 do
    let gs=shuffle g in
    let l=couplage_maximal gs 500 in
    print_int(List.length l);
    print_newline();
  done;
  ()

