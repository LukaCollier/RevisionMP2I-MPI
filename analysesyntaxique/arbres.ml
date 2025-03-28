type tree = Empty | Node of tree * string * tree


exception Syntax_error
let charlist_of_string str =
  let n =String.length str in
  let rec build i  =
    if i = n then []
    else
      str.[i]::build (i+1)
  in
  build 0

let rec parseS l =
  match l with
  | 'E'::'M'::'P'::'T'::'Y':: q -> Empty,q
  | 'N' ::'o'::'d'::'e'::'(':: q ->
     let a1,q = parseS q in
     begin
       match q with
       | ',' :: '\"':: q -> let str, q = parseA q in
                            begin
                              match q with
                                | ',' :: q -> let a2, q = parseS q in
                                              begin
                                                match  q with
                                                  |')' :: q -> Node(a1,str,a2),q
                                                  | _ -> raise Syntax_error
                                              end
                              | _ -> raise Syntax_error
                            end
       | _ -> raise Syntax_error
     end
  | _ -> raise Syntax_error
and parseA l =
  match l with
    |'\"' :: q -> "",l
    | t::q -> let str1,q = parseC l in
              let str2,q = parseA q in
              str1^ str2 , q
    |[] -> raise Syntax_error
and parseC l =
  match l with
  |[] |'\"' :: _ -> raise Syntax_error
  | t ::q -> String.make 1 t , q



let analyze l=
 let (ast,q) = parseS l in
 if q <> [] then raise Syntax_error
 else ast


let test = "Node (Node Node (Empty, \"banane\", Empty), \"cookie\", Empty)"
