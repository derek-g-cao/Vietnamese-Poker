(** Representation of cards.

    This module represents the data for cards in the game, including suit and
    rank. It handles creating cards of certain suits and ranks, and also
    converting the card data into things that can be used outside this module. *)

(**********************************************************************)

type t
(** The abstract type of values representing cards. *)

(** The type [suit] represents the suit of a card *)
type suit =
  | Spade
  | Clubs
  | Hearts
  | Diamonds

(** The type [rank] represents the rank of a card *)
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

exception Invalid of string
(** Raised when an invalid suit or rank for a card is given *)

val create_card : rank -> suit -> t
(** [create_card a b] creates a card with rank [a] and suit [b] *)

val create_rank_set : rank -> t list
(** [create_rank_set a] creates a list of 4 cards with rank [a] and different
    suits *)

val suit_string : suit -> string
(** [suit_string a] takes in a suit [a] and gives a shortened string
    representation of it *)

val rank_string : rank -> string
(** [rank_string a] takes in a suit [a] and gives a shortened string
    representation of it *)

val card_string : t -> string
(** [card_string a] takes in a card [a] and gives a shortened string
    representation of it *)

val string_to_card : string -> t
(** [string_to_card a] takes in a string [a] (shortened string representation)
    and turns it into a card representation *)
