type t =
  | Empty
  | Single of Card.t
  | Pair of Card.t
  | Triple of Card.t
  | Quads of Card.t
  | Straight of Card.t * int
  | DoubleStraight of Card.t * int

exception InvalidCombo

val rank_order : Card.rank -> int
(** [rank_order r] gives an int value corresponding to a rank's strength in the
    current set of rules. Ranks with higher int values have higher strength *)

val suit_order : Card.suit -> int
(** [suit_order s] gives an int value corresponding to [s]'s strenght in the
    current set of rules. Suits with higher int values have higher strenght. *)

val compare : Card.t -> Card.t -> int
(** [compare c1 c2] compares two cards [c1] and [c2] first by rank then by suit.
    If c1 > c2 compare will return a positive integer, if c1 < c2 compare will
    return a negative integer, and if they are equal compare will return zero *)

val make_combo : Card.t list -> t
(** [make_combo c] takes a list of cards [c] and turns it into its corresponding
    combo. If [c] does not represent a valid combo an InvalidCombo exception is
    thrown. *)

val valid_play : t -> t -> bool
(** [valid_play c1 c2] takes combo [c2] that the player is attempting to play
    and returns a boolean value true if it is a valid play considering combo
    [c1] is currently on the board *)

val to_string : t -> string
(** [to_string c] takes in combo [c] and converts it to a string *)
