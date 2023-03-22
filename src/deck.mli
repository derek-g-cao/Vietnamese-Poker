type deck
(** The type representing a deck of cards *)

val clean_deck : deck
(** Represents an unshuffled standard deck of 52 cards *)

val get_cards : deck -> Card.t list
(** [get_cards a] takes in a deck [a] and returns the cards that are in the deck *)

val shuffle : deck -> deck
(** [shuffle a] takes in a deck [a] and returns a shuffled version of it *)

val make_deck: Card.t list->deck

(** [make_deck lst] takes in a list of cards [lst] and returns a deck with those cards in the same order*)

val deal : deck -> int -> int -> Card.t list array
(** [deal a b] takes in a deck [a] and deals its cards to [b] players by
    creating [b] lists of cards *)
