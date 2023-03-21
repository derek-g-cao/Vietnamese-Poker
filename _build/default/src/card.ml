type suit =
  | Spade
  | Clubs
  | Hearts
  | Diamonds

type rank =
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two

type t = {
  suit : suit;
  rank : rank;
}

let card_suit card : suit = card.suit
let card_rank card : rank = card.rank
let create_card rank suit = { suit; rank }

let create_rank_set rank =
  [
    create_card rank Spade;
    create_card rank Clubs;
    create_card rank Hearts;
    create_card rank Diamonds;
  ]
