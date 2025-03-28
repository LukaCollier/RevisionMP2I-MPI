open Types
open Graphics
open Io
open Utilities
open Gameengine
open Ai

(*******************)
(* Initialisations *)
(*******************)

let () = Random.self_init ()

let () = open_graph ""; resize_window 780 540; draw_grids ()

let deck = Hashtbl.create 16
let () = Hashtbl.replace deck "Tiger" [(0,2);(0,-1)]
let () = Hashtbl.replace deck "Crab" [(-2,0);(0,1);(2,0)]
let () = Hashtbl.replace deck "Monkey" [(-1,-1);(-1,1);(1,1);(1,-1)]
let () = Hashtbl.replace deck "Crane" [(-1,-1);(0,1);(1,-1)]
let () = Hashtbl.replace deck "Dragon" [(-2,1);(-1,-1);(1,-1);(2,1)]
let () = Hashtbl.replace deck "Elephant" [(-1,0);(-1,1);(1,1);(1,0)]
let () = Hashtbl.replace deck "Mantis" [(-1,1);(0,-1);(1,1)]
let () = Hashtbl.replace deck "Boar" [(-1,0);(0,1);(1,0)]
let () = Hashtbl.replace deck "Frog" [(-2,0);(-1,1);(1,-1)]
let () = Hashtbl.replace deck "Goose" [(-1,1);(-1,0);(1,0);(1,-1)]
let () = Hashtbl.replace deck "Horse" [(-1,0);(0,1);(0,-1)]
let () = Hashtbl.replace deck "Eel" [(-1,-1);(-1,1);(1,0)]
let () = Hashtbl.replace deck "Rabbit" [(-1,-1);(1,1);(2,0)]
let () = Hashtbl.replace deck "Rooster" [(-1,0);(-1,-1);(1,0);(1,1)]
let () = Hashtbl.replace deck "Ox" [(0,1);(0,-1);(1,0)]
let () = Hashtbl.replace deck "Cobra" [(-1,0);(1,1);(1,-1)]
let card_list = Hashtbl.fold (fun k x a -> k::a) deck []

let start_grid = {
    p1king = (0,-2);
    p2king = (0,2);
    p1pawns = [(-2,-2);(-1,-2);(1,-2);(2,-2)];
    p2pawns = [(-2,2);(-1,2);(1,2);(2,2)];
  }

let start_state =
  let (card1, rem) = sample card_list in
  let (card2, rem) = sample rem in
  let (card3, rem) = sample rem in
  let (card4, rem) = sample rem in
  let (card5, _) = sample rem in
  (
    start_grid,
    (card1, card2),
    card3,
    (card4, card5),
    P1
  )

let () = display_state deck start_state

(********************)
(* Running the game *)
(********************)

let rec play st = match st with
  | P1win -> clear_graph (); moveto 390 270; draw_string "P1 wins"; print_endline "P1 wins"
  | P2win -> clear_graph (); moveto 390 270; draw_string "P2 wins"; print_endline "P2 wins"
  | Playing s -> let (g,(c11,c12),cmid,(c21,c22),p) = s in
                 match p with
                 | P1 -> play (play_user deck s)
                 | P2 -> play (pick_move deck s);;

play (Playing start_state);;
