let nbfils= 8
let ticket= Array.make nbfils (-1)
let choix= Array.make nbfils false
let cpt = ref 0
(*
let prochain_ticket () =
  let i = ref 0 in
  for j=1 to nbfils-1 do
    if ticket.(j)>ticket.(!i) then i:= j
  done;
  if ticket.(!i) = -1 then 0
  else 1+ (!i)
*)
let prochain_ticket () =
  let t = ref min_int in
  for i = 0 to nbfils-1 do
    if ticket.(i) +1 > !t then
      t:= ticket.(i) +1
  done;
  !t
let attend i j =
  while choix.(j) do () done;
  while (ticket.(j),j)< (ticket.(i),i) do () done


let f i =
  for j = 1 to 100 do
    choix.(i) <-true;
    ticket.(i) <- prochain_ticket ();
    choix.(i) <- false;
    for s = 0 to nbfils -1  do
      attend i s
    done;
(*section critique *)
    incr cpt;
    print_int i;
    print_string " : ";
    if !cpt mod 15 = 0 then print_string "FizzBuzz"
    else if !cpt mod 3 = 0 then print_string "Fizz"
    else if !cpt mod 5 = 0 then print_string "Buzz"
    else print_int @@ !cpt;
    print_newline ();
(*fin section critique *)
    ticket.(i)<- max_int
  done

let threads = Array.init nbfils (Thread.create f)

let () =Array.iter Thread.join threads
