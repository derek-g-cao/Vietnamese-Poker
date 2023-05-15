(** Representation of a player playing the game.

    This module represents the players, including their current hand and whether
    or not they have passed the current round. It handles the logic for player
    hands, as well as bringing them in and out of rounds. *)

(**********************************************************************)

type player
(** The type representing a player in a game *)

(** The type [move] represents a move that a player can make during a game *)
type move =
  | Play of Card.t list
  | Pass
  | Show
  | Count
  | Quit
  | Combo

exception Invalid
(** Raised when a player move is invalid *)

val player_hand : player -> Card.t list
(** [player_hand a] takes in a player [a] and returns that player's hand *)

val in_round : player -> bool
(** [in_round a] takes in a player [a] and returns if that player is playing in
    the current round of play *)

val has_won : player -> bool
(** [has_won a] takes in a player [a] and returns if that player has already
    played all their cards *)

val pass : player -> player
(** [pass p] takes in player [p] and opts them out of the round because they
    passed *)

val return_to_round : player -> player
(** [return_to_round p] takes in player [p] and opts them back in because a new
    round started *)

val make_player : Card.t list -> bool -> bool -> player
(** [make_player lst a b] takes in cards [lst] and booleans [a] and [b] to
    create a player with those cards and booleans *)

val show_hand : Card.t list -> string
(** [show_hand lst] takes in cards and returns a string representation of those
    cards in order *)

val parse_move : string -> move
(** [parse_move a] takes in a player's input string as [a] and turns it into a
    valid move *)

val remove_cards : player -> Card.t list -> player
(** [remove_cards p c] takes the player [p] and removes the cards in the list
    [c], indicating that those cards have been played and are no longer in the
    player's hand *)

val contain_cards : player -> Card.t list -> bool
(** [contain_cards p c] takes in player [p] and returns true if their hand
    contains cards [c] and false otherwise *)
