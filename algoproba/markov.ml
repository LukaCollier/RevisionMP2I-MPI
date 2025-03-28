let g=[|
    [1;2;3;4];
    [0;2;3];
    [0;1];
    [0;1];
    [0];
   |]

let m = Array.init 5 (fun i -> let di = float_of_int (List.length g.(i)) in Array.init 5 (fun j -> if List.mem j g.(i) then 1./. di else 0.))

let rand_vois i = 
  let j = Random.int(List.length g.(i)) in
  List.nth g.(i) j

let rec roule n s =
  if n = 0 then s
  else roule (n-1) (rand_vois s)

let simule=
  let res =Array.make 5 0 in
  for i= 1 to 10000 do
    let j = roule 1000 0 in
    res.(j) <- res.(j)+1
  done;
  res
