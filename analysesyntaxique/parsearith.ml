type expr = N of int | Var | Op of expr * binop * expr
and binop = Add | Mult | Sub | Div

let charlist_of_string str =
  let n =String.length str in
  let rec aux i =
    if i = n then []
    else str.[i] :: aux (i+1)
  in
  aux 0

exception Syntax_error of char
(*let rec parseE = (*fonctionne pas sad *)
  function
  | c::q when c = 'x' || c ='(' || ('0' <= c && c <= '9') ->
     let (e,q) = parseT (c::q) in
     let (f,q) =parseE' q in
     begin
       match f with
       |Some (op,e') -> Op(e,op,e'),q
       |None -> e,q
     end
  |c::q -> raise (Syntax_error c )
  |[] -> raise (Syntax_error '$')
and parseE' =
  function
  |'+'::q -> let (e,q) =parseT ('+'::q) in
             Some (Add,e),q
  |'-'::q -> let (e,q) =parseT ('-'::q) in
             Some (Sub,e),q
  |l -> None,l
  (*|c::q -> raise (Syntax_error c )*)
and parseT =
  function
  | c::q when c = 'x' || c ='(' || ('0' <= c && c <= '9') ->
     let (e,q) = parseF (c::q) in
     let (f,q) =parseE' q in
     begin
       match f with
       |Some (op,e') -> Op(e,op,e'),q
       |None -> e,q
     end
  |c::q -> raise (Syntax_error c )
  |[] -> raise (Syntax_error '$')
and parseT' =
  function
  |'*'::q -> let (e,q) =parseT ('*'::q) in
             Some (Mult,e),q
  |'/'::q -> let (e,q) =parseT ('/'::q) in
             Some (Div,e),q
  |l -> None,l
  (*|c::q -> raise (Syntax_error c )*)
and parseF =
  function
  | c::q when '0'<= c && c <= '9' ->
     parseN (c::q)
  | 'x':: q -> Var, q
  | '(' ::q -> let(e,q) = parseE q in
               begin
                 match q with
                 | ')' :: q -> e,q
                 |c::q -> raise (Syntax_error c )
                 |[] -> raise (Syntax_error '$')
               end
  |c::q -> raise (Syntax_error c )
  |[] -> raise (Syntax_error '$')
and parseN =
  function
  | '0' ::q ->N 0,q
  | c::q when '0'<= c && c <= '9' ->
    parseL (int_of_char c  - int_of_char '0') q
  |c::q -> raise (Syntax_error c )
  |[] -> raise (Syntax_error '$')
and parseL =
  function
  | c::q when '0' <= c && c <= '9' ->
     parseL (acc *10 + (int_of_char c) - (int_of_char '0')) q
  |l -> acc,l


let expr_of_string str=
  let e,l = parseE (charlist_of_string str) in
  match l with
  |[] -> e
  | c::q -> raise (Syntaxe_error c)


let string_of_binop = function
  |Add -> "+"
  |Sub -> "-"
  |Mult -> "*"
  |Div -> "/"


let string_of_expr= function
  |Var -> "x"
  |N n -> string_of_int n
  |Op (e1,op,e2) -> "(" ^ string_of_expr e1 ^ string_of_binop op ^ string_of_expr e2 ^ ")"


let calc op a b =
  match op with
  |Add -> a +b
  |Sub -> a - b
  |Mult -> a *b
  |Div -> a/b

let rec simplify =
  function
  |Op(a,op,b) ->
    begin
      match simplify a, simplify b with
      |N n, N m -> N (calc op n m)
      |e1,e2 -> Op(e1,op,e2)
    end
  |e -> e
*)

(*version corrigé*)
let rec parseE  = function
  | c :: q when c = 'x' || c = '(' || ('0' <= c && c <= '9')
    -> let (e, q) = parseT (c :: q) in
       let (f, q) = parseE' q in
       begin
         match f with
         | Some (op, e') -> Op (e, op, e'), q
         | None -> e, q
       end
  | c :: q -> raise (Syntax_error c)
  | [] -> raise (Syntax_error '$')
and     parseE' = function
  | '+' :: q -> let (e, q) = parseE q in
                Some (Add, e), q
  | '-' :: q -> let (e, q) = parseE q in
                Some (Sub, e), q
  | l -> None, l
and     parseT  = function
  | c :: q when c = 'x' || c = '(' || ('0' <= c && c <= '9')
    -> let (e, q) = parseF (c :: q) in
       let (f, q) = parseT' q in
       begin
         match f with
         | Some (op, e') -> Op (e, op, e'), q
         | None -> e, q
       end
  | c :: q -> raise (Syntax_error c)
  | [] -> raise (Syntax_error '$')
and    parseT' = function
  | '*' :: q -> let (e, q) = parseT q in
                Some (Mult, e), q
  | '/' :: q -> let (e, q) = parseT q in
                Some (Div, e), q
  | l -> None, l
and    parseF  = function
  | c :: q when '0' <= c && c <= '9'
    -> parseN (c :: q)
  | 'x' :: q -> Var, q
  | '(' :: q -> let (e, q) = parseE q in
                begin
                  match q with
                  | ')' :: q -> e, q
                  | c :: q -> raise (Syntax_error c)
                  | [] -> raise (Syntax_error '$')
                end
  | c :: q -> raise (Syntax_error c)
  | [] -> raise (Syntax_error '$')
and    parseN  = function
  | '0' :: q -> N 0, q
  | c :: q when '1' <= c && c <= '9'
    -> parseL (int_of_char c - int_of_char '0') q
  | c :: q -> raise (Syntax_error c)
  | [] -> raise (Syntax_error '$')
and    parseL acc = function
  | c :: q when '0' <= c && c <= '9'
    -> parseL (acc * 10 + (int_of_char c) - (int_of_char '0')) q
  | l -> N acc, l

let expr_of_string str =
  let e, l = parseE (charlist_of_string str) in
  match l with
  | [] -> e
  | c :: q -> raise (Syntax_error c)

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"

let rec string_of_expr = function
  | Var -> "x"
  | N n -> string_of_int n
  | Op (e1, op, e2) -> "(" ^ string_of_expr e1 ^ string_of_binop op ^ string_of_expr e2 ^ ")"

let rec postfix_of_expr = function
  | Var -> "x"
  | N n -> string_of_int n
  | Op (e1, op, e2) ->  string_of_binop op ^ " " ^ postfix_of_expr e1 ^ " " ^ postfix_of_expr e2

let calc op a b = match op with
  | Add -> a + b
  | Sub -> a - b
  | Mult -> a * b
  | Div -> a / b

let rec simplify = function
  | Op (a, op, b) -> begin
      match simplify a, simplify b with
      | N n, N m -> N (calc op n m)
      | e1, e2 -> Op (e1, op, e2)
    end
  | e -> e
