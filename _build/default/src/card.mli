type t
(** The abstract type of values representing cards. *)

(** The type [suit] represents the suit of a card *)
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

val card_suit : t -> suit
(** [card_suit a] is the identifier of the card suit for card [a] *)

val card_rank : t -> rank
(** [card_rank a] is the identifier of the card rank for card [a] *)

val create_card : rank -> suit -> t
(** [create_card a b] creates a card with rank [a] and suit [b] *)

val create_rank_set : rank -> t list
(** [create_rank_set a] creates a list of 4 cards with rank [a] and different
    suits*)
