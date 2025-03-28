(************************)
(* Definitions de types *)
(************************)

type player = P1 | P2
type piece = King | Pawn
type coord = int * int
type grid = {
    p1king : coord;
    p2king : coord;
    p1pawns : coord list;
    p2pawns : coord list;
  }
type card = string
type state = P1win | P2win | Playing of (grid * (card * card) * card * (card * card) * player)
type direction = Up | Down

exception Retry
