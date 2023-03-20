type t
(** The abstract type of values representing cards. *)

val card_suit : t -> string
(** [card_suit a] is the identifier of the card suit for card [a] *)

val card_rank : t -> int
(** [card_rank a] is the identifier of the card rank for card [a] *)
