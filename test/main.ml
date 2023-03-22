open OUnit2
open Game
open Card
open Deck

(** [cmp_lists lst1 lst2] compares two lists to see whether they are equivalent
    by checking if the elements in both lists are the same *)
let cmp_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  uniq1 = uniq2

let card_suit_test (name : string) card (expected_output : suit) : test =
  name >:: fun _ -> assert_equal expected_output (card_suit card)

let card_rank_test (name : string) card (expected_output : rank) : test =
  name >:: fun _ -> assert_equal expected_output (card_rank card)

let card_string_test (name : string) card (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (card_string card) ~printer:String.escaped

let string_to_card_test (name : string) str (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (card_string (string_to_card str))
    ~printer:String.escaped

let invalid_card_test (name : string) str (expected_invalid : string) : test =
  name >:: fun _ ->
  assert_raises (Invalid expected_invalid) (fun () ->
      card_string (string_to_card str))

let shuffle_test (name : string) deck (expected_output : Card.t list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_lists expected_output (get_cards (shuffle deck))

let card_tests =
  [
    card_suit_test "Ace of Spades suit" (create_card Ace Spade) Spade;
    card_rank_test "Ace of Spades rank" (create_card Ace Spade) Ace;
    card_string_test "Ace of Spades string" (create_card Ace Spade) "AS";
    card_string_test "Two of Hearts string" (create_card Two Hearts) "2H";
    card_string_test "Ten of Hearts string" (create_card Ten Hearts) "10H";
    string_to_card_test "AS to Card" "AS" "AS";
    string_to_card_test "QH to Card" "QH" "QH";
    string_to_card_test "9H to Card" "9H" "9H";
    string_to_card_test "10C to Card" "10C" "10C";
    invalid_card_test "11C is Invalid" "11C" "11";
    invalid_card_test "AZ is Invalid" "AZ" "Z";
    invalid_card_test "HH is Invalid" "HH" "H";
    invalid_card_test "INVALID is Invalid" "INVALID" "INVALID";
    invalid_card_test "empty card is Invalid" "" "";
    invalid_card_test "11Z is Invalid" "11Z" "11";
    invalid_card_test "Z11 is Invalid" "Z11" "Z1";
    invalid_card_test "Z1 is Invalid" "Z1" "Z";
    (*soon valid :*)
    invalid_card_test " AS is Invalid" " AS" " A";
    invalid_card_test "  AS is Invalid" "  AS" "  AS";
  ]

let deck_tests =
  [
    shuffle_test "Shuffling a deck compared to normal" (shuffle clean_deck)
      (get_cards clean_deck);
  ]

let suite =
  "test suite for viet poker" >::: List.flatten [ card_tests; deck_tests ]

let _ = run_test_tt_main suite
