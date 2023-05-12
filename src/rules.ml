type t =
  | Empty
  | Single of Card.t
  | Pair of Card.t
  | Triple of Card.t
  | Quads of Card.t
  | Straight of Card.t * int
  | DoubleStraight of Card.t * int

exception InvalidCombo

let rank_order r =
  match r with
  | Card.Two -> 13
  | Card.Ace -> 12
  | Card.King -> 11
  | Card.Queen -> 10
  | Card.Jack -> 9
  | Card.Ten -> 8
  | Card.Nine -> 7
  | Card.Eight -> 6
  | Card.Seven -> 5
  | Card.Six -> 4
  | Card.Five -> 3
  | Card.Four -> 2
  | Card.Three -> 1

let suit_order s =
  match s with
  | Card.Hearts -> 4
  | Card.Diamonds -> 3
  | Card.Clubs -> 2
  | Card.Spade -> 1

let compare c1 c2 =
  if rank_order (Card.card_rank c1) > rank_order (Card.card_rank c2) then 1
  else if rank_order (Card.card_rank c1) < rank_order (Card.card_rank c2) then
    -1
  else if suit_order (Card.card_suit c1) > suit_order (Card.card_suit c2) then 1
  else if suit_order (Card.card_suit c1) > suit_order (Card.card_suit c2) then
    -1
  else 0

let check_pair c =
  match c with
  | [] -> failwith "pattern matching"
  | f :: s :: _ ->
      if Card.card_rank f = Card.card_rank s then
        if compare f s > 0 then Pair f else Pair s
      else raise InvalidCombo
  | [ _ ] -> failwith "pattern matching"

let check_triple c =
  match c with
  | [] -> failwith "pattern matching"
  | f :: s :: t :: _ ->
      if
        Card.card_rank f = Card.card_rank s
        && Card.card_rank s = Card.card_rank t
      then Some (Triple t)
      else None
  | [ _; _ ] -> failwith "pattern matching"
  | _ -> failwith "pattern matching"

let check_quads c =
  match c with
  | [] -> failwith "pattern matching"
  | [ a; b; c; d ] ->
      if
        Card.card_rank a = Card.card_rank b
        && Card.card_rank b = Card.card_rank c
        && Card.card_rank c = Card.card_rank d
      then Some (Quads d)
      else None
  | _ :: _ -> failwith "pattern matching"

let rec check_straight c acc =
  match c with
  | [] -> failwith "pattern matching"
  | [ h ] -> Some (Straight (h, acc + 1))
  | f :: s :: t ->
      if rank_order (Card.card_rank s) - rank_order (Card.card_rank f) = 1 then
        check_straight (s :: t) (acc + 1)
      else None

let make_combo c =
  let l = List.length c in
  if l = 1 then Single (List.nth c 0)
  else if l = 2 then check_pair c
  else if l = 3 then
    match check_triple c with
    | None -> (
        match check_straight c 0 with
        | None -> raise InvalidCombo
        | Some s -> s)
    | Some s -> s
  else if l = 4 then
    match check_quads c with
    | None -> (
        match check_straight c 0 with
        | None -> raise InvalidCombo
        | Some s -> s)
    | Some s -> s
  else raise InvalidCombo

let valid_play c1 c2 =
  match (c1, c2) with
  | Single a, Single b -> compare b a > 0
  | Single _, _ -> false
  | Pair a, Pair b -> compare b a > 0
  | Pair _, _ -> false
  | Triple a, Triple b -> compare b a > 0
  | Triple _, _ -> false
  | Quads a, Quads b -> compare b a > 0
  | Quads _, _ -> false
  | Straight (a, b), Straight (c, d) ->
      if b <> d then false else compare c a > 0
  | Straight _, _ -> false
  | DoubleStraight (a, b), DoubleStraight (c, d) ->
      if b <> d then false else compare c a > 0
  | DoubleStraight _, _ -> false
  | Empty, _ -> true

let to_string c =
  match c with
  | Empty -> "Nothing"
  | Single a -> "Single " ^ Card.card_string a
  | Pair a -> "Double with higher card " ^ Card.card_string a
  | Triple a -> "Triple with higher card " ^ Card.card_string a
  | Quads a -> "Quads with rank " ^ Card.rank_string (Card.card_rank a)
  | Straight (a, b) ->
      "Straight with length " ^ string_of_int b ^ "with high card"
      ^ Card.card_string a
  | DoubleStraight (a, b) ->
      "Double straight with length " ^ string_of_int b ^ "with high card "
      ^ Card.card_string a
