open Types
open Utilities
open Io

(*******************************************)
(* List all possible moves from a position *)
(*******************************************)

let opp = function P1 -> P2 | P2 -> P1

let moves deck (x, y) card p friends =
  let l = Hashtbl.find deck card in
  let rec aux = function
    | [] -> Seq.empty
    | (dx, dy)::q ->
       let (xx, yy) = match p with
         | P1 -> (x + dx, y + dy)
         | P2 -> (x - dx, y - dy)
       in
       if xx <= 2 && yy <= 2 && xx >= -2 && yy >= -2 then
         check_friends (xx,yy) q friends
       else
         aux q
    and check_friends coord q = function
      | [] -> Seq.cons coord (aux q)
      | h::t ->
         if h = coord then
           aux q
         else
           check_friends coord q t
  in aux l

let king_moves deck (g, (c11, c12), cmid, (c21, c22), p) =
  let update card_num mov = match p with
    | P1 ->
       if mov = g.p2king || mov = (0,2) then
         P1win
       else
         let new_grid = {
             p1king = mov;
             p2king = g.p2king;
             p1pawns = g.p1pawns;
             p2pawns = filter_out mov g.p2pawns;
           } in
         if card_num = 1 then
           Playing (new_grid, (cmid, c12), c11, (c21, c22), opp p)
         else
           Playing (new_grid, (c11, cmid), c12, (c21, c22), opp p)
    | P2 ->
       if mov = g.p1king || mov = (0,-2) then
         P2win
       else
         let new_grid = {
             p1king = g.p1king;
             p2king = mov;
             p1pawns = filter_out mov g.p1pawns;
             p2pawns = g.p2pawns;
           } in
         if card_num = 1 then
           Playing (new_grid, (c11, c12), c21, (cmid, c22), opp p)
         else
           Playing (new_grid, (c11, c12), c22, (c21, cmid), opp p)
  in match p with
     | P1 -> Seq.append
               (Seq.map (update 1) (moves deck g.p1king c11 p g.p1pawns))
               (Seq.map (update 2) (moves deck g.p1king c12 p g.p1pawns))
     | P2 -> Seq.append
               (Seq.map (update 1) (moves deck g.p2king c21 p g.p2pawns))
               (Seq.map (update 2) (moves deck g.p2king c22 p g.p2pawns))

let pawn_moves deck (g, (c11, c12), cmid, (c21, c22), p) =
  let update card_num before after mov = match p with
    | P1 ->
       if mov = g.p2king then
         P1win
       else
         let new_grid = {
             p1king = g.p1king;
             p2king = g.p2king;
             p1pawns = depile (mov::after) before;
             p2pawns = filter_out mov g.p2pawns;
           } in
         if card_num = 1 then
           Playing (new_grid, (cmid, c12), c11, (c21, c22), opp p)
         else
           Playing (new_grid, (c11, cmid), c12, (c21, c22), opp p)
    | P2 ->
       if mov = g.p1king then
         P2win
       else
         let new_grid = {
             p1king = g.p1king;
             p2king = g.p2king;
             p1pawns = filter_out mov g.p1pawns;
             p2pawns = depile (mov::after) before;
           } in
         if card_num = 1 then
           Playing (new_grid, (c11, c12), c21, (cmid, c22), opp p)
         else
           Playing (new_grid, (c11, c12), c22, (c21, cmid), opp p)
  in let rec move_pawns before = function
       | [] -> Seq.empty
       | t::q -> let current_pawn_moves = match p with
                   | P1 ->
                      Seq.append
                        (Seq.map (update 1 before q) (moves deck t c11 p (g.p1king::g.p1pawns)))
                        (Seq.map (update 2 before q) (moves deck t c12 p (g.p1king::g.p1pawns)))
                   | P2 ->
                      Seq.append
                        (Seq.map (update 1 before q) (moves deck t c21 p (g.p2king::g.p2pawns)))
                        (Seq.map (update 2 before q) (moves deck t c22 p (g.p2king::g.p2pawns)))
                 in Seq.append
                      current_pawn_moves
                      (move_pawns (t::before) q)
     in match p with
        | P1 -> move_pawns [] g.p1pawns
        | P2 -> move_pawns [] g.p2pawns

let all_moves deck state =
  let move_list = Seq.append (king_moves deck state) (pawn_moves deck state) in
  if move_list () = Seq.Nil then
    let (g, (c11, c12), cmid, (c21, c22), p) = state in
    match p with
    | P1 -> Seq.append
              (Seq.return (Playing (g,(cmid,c12),c11,(c21,c22),opp p)))
              (Seq.return (Playing (g,(c11,cmid),c12,(c21,c22),opp p)))
    | P2 -> Seq.append
              (Seq.return (Playing (g,(c11,c12),c21,(cmid,c22),opp p)))
              (Seq.return (Playing (g,(c11,c12),c22,(c21,cmid),opp p)))
  else
    move_list

(********************)
(* Input user moves *)
(********************)

let play_user deck state =
  try
    display_state deck state;
    let (g, (c11, c12), cmid, (c21, c22), p)  = state in
    let left = click_on_left_card () in
    let card = if left then c11 else c12 in
    pick_card deck card left;
    let (i,j) = click_board () in
    if g.p1king = (i,j) then
      begin
        pick_piece King i j;
        let (newi,newj) = click_board () in
        if not (List.mem (newi-i,newj-j) (Hashtbl.find deck card))
           || List.mem(newi,newj) g.p1pawns
           || newi < -2 || newi > 2
           || newj < -2 || newj > 2
        then raise Retry;
        if (newi,newj) = g.p2king || (newi,newj) = (0,2) then
          P1win
        else
          let newgrid = {
              p1king = (newi, newj);
              p2king = g.p2king;
              p1pawns = g.p1pawns;
              p2pawns = filter_out (newi, newj) g.p2pawns;
            } in
          if left then Playing (newgrid, (cmid, c12), c11, (c21, c22), P2)
          else Playing (newgrid, (c11, cmid), c12, (c21, c22), P2)
      end
    else
      begin
        let rec find x acc = function
          | [] -> None
          | t::q -> if t = x then Some (acc, t, q) else find x (t::acc) q
        in match find (i,j) [] g.p1pawns with
           | None -> raise Retry
           | Some (before, pawn, after) ->
              pick_piece Pawn i j;
              let (newi,newj) = click_board () in
              if not (List.mem (newi-i,newj-j) (Hashtbl.find deck card))
                 || List.mem(newi,newj) g.p1pawns
                 || (newi,newj) = g.p1king
                 || newi < -2 || newi > 2
                 || newj < -2 || newj > 2
              then raise Retry;
              if (newi,newj) = g.p2king then
                P1win
              else
                let newgrid = {
                    p1king = g.p1king;
                    p2king = g.p2king;
                    p1pawns = depile ((newi, newj)::after) before;
                    p2pawns = filter_out (newi, newj) g.p2pawns;
                  } in
                if left then Playing (newgrid, (cmid, c12), c11, (c21, c22), P2)
                else Playing (newgrid, (c11, cmid), c12, (c21, c22), P2)
      end
  with Retry -> display_state deck state; Playing state
