type player
(** The type representing a player in a game *)

(** The type [move] represents a move that a player can make during a game *)
type move =
  | Play of Card.t list
  | Pass

exception Empty
(** Raised when an empty player move is parsed *)

exception Malformed of string
(** Raised when a player move is malformed *)

val player_hand : player -> Card.t list
(** [player_hand a] takes in a player [a] and returns that player's hand *)

val in_round : player -> bool
(** [in_round a] takes in a player [a] and returns if that player is playing in
    the current round of play *)

val has_won : player -> bool
(** [has_won a] takes in a player [a] and returns if that player has already
    played all their cards *)

val parse_move : string -> move
(** [parse_move a] takes in a player's input string as [a] and turns it into a
    valid move *)
