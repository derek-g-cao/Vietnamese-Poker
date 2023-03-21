type player = {
  hand : Card.t list;
  opted_in : bool;
  out_of_cards : bool;
}

exception Empty
exception Malformed of string

type move =
  | Play of Card.t list
  | Pass

let player_hand player = player.hand
let in_round player = player.opted_in
let has_won player = player.out_of_cards
let parse_move str = raise (Malformed str)
