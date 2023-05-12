type player = {
  hand : Card.t list;
  opted_in : bool;
  out_of_cards : bool;
}

exception Invalid

type move =
  | Play of Card.t list
  | Pass
  | Show
  | Count
  | Quit

let player_hand player = player.hand
let in_round player = player.opted_in
let has_won player = player.out_of_cards
let make_player lst a b = { hand = lst; opted_in = a; out_of_cards = b }
let pass p = { hand = p.hand; opted_in = false; out_of_cards = p.out_of_cards }

let rec show_hand_helper (lst : Card.t list) : string =
  match lst with
  | [ h ] -> Card.card_string h
  | h :: t -> Card.card_string h ^ ", " ^ show_hand_helper t
  | [] -> "empty"

let show_hand lst = show_hand_helper lst

let rec process_cards after_play =
  match after_play with
  | [] -> []
  | h :: t ->
      if h = "" then process_cards t
      else Card.string_to_card h :: process_cards t

let rec command_type player_input =
  match player_input with
  | [] -> raise Invalid
  | h :: t ->
      if h = "" then command_type t
      else if h = "play" then Play (process_cards t)
      else if h = "pass" then Pass
      else if h = "show" then Show
      else if h = "count" then Count
      else if h = "quit" then Quit
      else raise Invalid

let check_valid move =
  match move with
  | Pass -> Pass
  | Show -> Show
  | Count -> Count
  | Quit -> Quit
  | Play s -> (
      match s with
      | [] -> raise Invalid
      | _ -> Play s)

let parse_move str = String.split_on_char ' ' str |> command_type |> check_valid

let rec remove hand card =
  match hand with
  | [] -> []
  | h :: t -> if h = card then t else h :: remove t card

let rec remove_cards p c =
  match c with
  | [] -> p
  | h :: t ->
      remove_cards
        {
          hand = remove p.hand h;
          opted_in = p.opted_in;
          out_of_cards = p.out_of_cards;
        }
        t
