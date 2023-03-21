type deck = Card.t list

let clean_deck =
  Card.create_rank_set Card.Ace
  @ Card.create_rank_set Card.King
  @ Card.create_rank_set Card.Queen
  @ Card.create_rank_set Card.Jack
  @ Card.create_rank_set Card.Ten
  @ Card.create_rank_set Card.Nine
  @ Card.create_rank_set Card.Eight
  @ Card.create_rank_set Card.Seven
  @ Card.create_rank_set Card.Six
  @ Card.create_rank_set Card.Five
  @ Card.create_rank_set Card.Four
  @ Card.create_rank_set Card.Three
  @ Card.create_rank_set Card.Two

let shuffle deck =
  let nd = List.map (fun c -> (Random.bits (), c)) deck in
  let sond = List.sort compare nd in
  List.map snd sond

let rec deal_helper deck player players dealt =
  match deck with
  | [] -> dealt
  | h :: t ->
      Array.set dealt player (h :: dealt.(player));
      if player < players - 1 then deal_helper t (player + 1) players dealt
      else deal_helper t 0 players dealt

let deal deck players = deal_helper deck 0 players (Array.make players deck)
