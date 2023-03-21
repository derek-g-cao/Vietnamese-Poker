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

exception Invalid of string

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

let suit_string suit =
  match suit with
  | Spade -> "S"
  | Clubs -> "C"
  | Hearts -> "H"
  | Diamonds -> "D"

let rank_string rank =
  match rank with
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

let card_string card = rank_string card.rank ^ suit_string card.suit

let string_to_suit str =
  match str with
  | "S" -> Spade
  | "C" -> Clubs
  | "H" -> Hearts
  | "D" -> Diamonds
  | _ -> raise (Invalid str)

let string_to_rank str =
  match str with
  | "2" -> Two
  | "3" -> Three
  | "4" -> Four
  | "5" -> Five
  | "6" -> Six
  | "7" -> Seven
  | "8" -> Eight
  | "9" -> Nine
  | "10" -> Ten
  | "J" -> Jack
  | "Q" -> Queen
  | "K" -> King
  | "A" -> Ace
  | _ -> raise (Invalid str)

let string_to_card str =
  {
    suit = string_to_suit (Char.escaped str.[1]);
    rank = string_to_rank (Char.escaped str.[0]);
  }
