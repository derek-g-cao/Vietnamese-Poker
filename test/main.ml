open OUnit2
open Game
open Card
open Deck

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent by checking if the elements in both lists are the same *)
let cmp_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  uniq1 = uniq2

let card_suit_test (name : string) card (expected_output : suit) : test =
  name >:: fun _ -> assert_equal expected_output (card_suit card)

let card_rank_test (name : string) card (expected_output : rank) : test =
  name >:: fun _ -> assert_equal expected_output (card_rank card)

let shuffle_test (name : string) deck (expected_output : Card.t list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_lists expected_output (get_cards (shuffle deck))

let card_tests =
  [
    card_suit_test "Ace of Spades suit" (create_card Ace Spade) Spade;
    card_rank_test "Ace of Spades rank" (create_card Ace Spade) Ace;
  ]

let deck_tests =
  [
    shuffle_test "Shuffling a deck compared to normal" (shuffle clean_deck)
      (get_cards clean_deck);
  ]

let suite = "test suite for A2" >::: List.flatten [ card_tests; deck_tests ]
let _ = run_test_tt_main suite
