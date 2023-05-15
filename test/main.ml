(** Main.ml tests most functions directly and some functions indirectly.
    Functions like shuffle were tested directly, but functions like create_card
    were not. This is because directly-tested functions used the
    indirectly-tested functions. We created cards when creating decks to test
    on. Should create_card not work, many of our tests would not have passed. We
    tested different combinations of cards extensively. Card, Deck, Player,
    Rules, and State were tested. Glass Box and Black Box testing were both
    used, depending on what function was being tested. For example, functions
    with pattern matching were tested with glass box testing to make sure we
    covered every branch. Black Box was used for more easy-to-see functions,
    like card_string. A combination of this type of testing makes it so that we
    review a lot of lines in the system, making for more comprehensive testing. *)

open OUnit2
open Game
open Card
open Deck
open Rules
open Player
open State

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

let suit_string_test name s output : test =
  name >:: fun _ -> assert_equal output (suit_string s)

let rank_string_test name s output : test =
  name >:: fun _ -> assert_equal output (rank_string s)

let invalid_card_test (name : string) str (expected_invalid : string) : test =
  name >:: fun _ ->
  assert_raises (Invalid expected_invalid) (fun () ->
      card_string (string_to_card str))

let card_tests =
  [
    suit_string_test "Spade string" Spade "S";
    suit_string_test "hearts string" Hearts "H";
    suit_string_test "clubs string" Clubs "C";
    suit_string_test "diamonds string" Diamonds "D";
    rank_string_test "10 rank string" Ten "10";
    rank_string_test "4 rank string" Four "4";
    rank_string_test "3 rank string" Three "3";
    rank_string_test "Queen rank string" Queen "Q";
    rank_string_test "Ace rank string" Ace "A";
    rank_string_test "Jack rank string" King "K";
    rank_string_test "10 rank string" Two "2";
    rank_string_test "4 rank string" Nine "9";
    rank_string_test "3 rank string" Three "3";
    rank_string_test "Queen rank string" Seven "7";
    rank_string_test "Ace rank string" Eight "8";
    rank_string_test "Jack rank string" Jack "J";
    rank_string_test "Ace rank string" Ten "10";
    rank_string_test "Jack rank string" Five "5";
    card_suit_test "Ace of Spades suit" (create_card Ace Spade) Spade;
    card_rank_test "Ace of Spades rank" (create_card Ace Spade) Ace;
    card_suit_test "Ace of Spades suit" (create_card Four Clubs) Clubs;
    card_rank_test "Ace of Spades rank" (create_card Jack Clubs) Jack;
    card_string_test "Ace of Spades string" (create_card Ace Spade) "AS";
    card_string_test "Two of Hearts string" (create_card Two Hearts) "2H";
    card_string_test "Ten of Hearts string" (create_card Ten Hearts) "10H";
    card_string_test "Ace of Spades string" (create_card Three Spade) "3S";
    card_string_test "Two of Hearts string" (create_card King Hearts) "KH";
    card_string_test "Ten of Hearts string" (create_card Three Hearts) "3H";
    card_string_test "Ace of Spades string" (create_card Jack Spade) "JS";
    card_string_test "Two of Hearts string" (create_card Jack Hearts) "JH";
    card_string_test "Ten of Hearts string" (create_card Queen Hearts) "QH";
    string_to_card_test "AS to Card" "AS" "AS";
    string_to_card_test "QH to Card" "QH" "QH";
    string_to_card_test "9H to Card" "9H" "9H";
    string_to_card_test "10C to Card" "10C" "10C";
    string_to_card_test "AS to Card" "AH" "AH";
    string_to_card_test "QH to Card" "QS" "QS";
    string_to_card_test "9H to Card" "10H" "10H";
    string_to_card_test "10C to Card" "10C" "10C";
    string_to_card_test "AS to Card" "JS" "JS";
    string_to_card_test "QH to Card" "QH" "QH";
    string_to_card_test "9H to Card" "KH" "KH";
    string_to_card_test "10C to Card" "3C" "3C";
    invalid_card_test "11C is Invalid" "11C" "11";
    invalid_card_test "AZ is Invalid" "AZ" "Z";
    invalid_card_test "HH is Invalid" "HH" "H";
    invalid_card_test "INVALID is Invalid" "INVALID" "INVALID";
    invalid_card_test "empty card is Invalid" "" "";
    invalid_card_test "11Z is Invalid" "11Z" "11";
    invalid_card_test "Z11 is Invalid" "Z11" "Z1";
    invalid_card_test "Z1 is Invalid" "Z1" "Z";
    invalid_card_test " AS is Invalid" " AS" " A";
    invalid_card_test "  AS is Invalid" "  AS" "  AS";
  ]

let shuffle_test (name : string) deck (expected_output : Card.t list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_lists expected_output (get_cards (shuffle deck))

let deal_length_test (name : string) deck players : test =
  name >:: fun _ ->
  let dealt = deal deck players in
  let arr_length = Array.length dealt in
  assert_equal players arr_length

let deal_contents_test (name : string) deck players (expected_output : int) :
    test =
  name >:: fun _ ->
  let dealt = deal deck players in
  let all_dealt =
    match dealt with
    | [| a1; a2; a3 |] -> List.length a1 + List.length a2 + List.length a3
    | [| a1; a2; a3; a4 |] ->
        List.length a1 + List.length a2 + List.length a3 + List.length a4
    | _ -> 0
  in
  assert_equal expected_output all_dealt ~printer:string_of_int

let deck1List =
  [
    create_card Three Spade;
    create_card Ace Clubs;
    create_card Seven Clubs;
    create_card Nine Spade;
    create_card Four Hearts;
    create_card Jack Hearts;
    create_card King Diamonds;
    create_card Queen Hearts;
  ]

let deck1 = make_deck deck1List
let empty_deck_list = []
let empty_deck = make_deck empty_deck_list

let deck_tests =
  [
    shuffle_test "Shuffling a deck compared to normal" (shuffle clean_deck)
      (get_cards clean_deck);
    shuffle_test "shuffle custom deck1" (shuffle deck1) (get_cards deck1);
    shuffle_test "shuffle empty deck" (shuffle empty_deck)
      (get_cards empty_deck);
    deal_length_test "deal deck to 2" (shuffle clean_deck) 2;
    deal_length_test "deal deck to 3" (shuffle clean_deck) 3;
    deal_length_test "deal deck to 4" (shuffle clean_deck) 4;
    deal_length_test "deal deck to 5" (shuffle clean_deck) 5;
    deal_length_test "deal empty to 2" (shuffle empty_deck) 2;
    deal_length_test "deal empty to 5" (shuffle empty_deck) 5;
    deal_contents_test "deal deck to 3 contents" (shuffle clean_deck) 3 52;
    deal_contents_test "deal deck to 4 contents" (shuffle clean_deck) 4 52;
    deal_contents_test "deal deck1 to 3 contents" (shuffle deck1) 3 8;
    deal_contents_test "deal deck1 to 4 contents" (shuffle deck1) 4 8;
    deal_contents_test "deal empty to 3 contents" empty_deck 3 0;
    deal_contents_test "deal empty to 4 contents" empty_deck 4 0;
  ]

let opted_test name player output =
  name >:: fun _ -> assert_equal output (Player.in_round player)

let won_test name player output =
  name >:: fun _ -> assert_equal output (Player.has_won player)

let parse_test name move output =
  name >:: fun _ -> assert_equal output (Player.parse_move move)

let show_test name player output =
  name >:: fun _ ->
  assert_equal output (player |> Player.player_hand |> Player.show_hand)

let contain_cards_test name player cards output =
  name >:: fun _ -> assert_equal output (Player.contain_cards player cards)

let a =
  Player.make_player
    [
      create_card Ace Spade;
      create_card Ace Clubs;
      create_card Two Hearts;
      create_card Two Spade;
      create_card Four Diamonds;
      create_card Five Clubs;
      create_card Five Spade;
      create_card Five Hearts;
      create_card Five Diamonds;
      create_card King Spade;
      create_card Queen Hearts;
    ]
    true false

let b = Player.pass a
let c = Player.return_to_round b
let dremoved = [ create_card Ace Spade ]
let d = Player.remove_cards c dremoved
let dstring = "AC, 2H, 2S, 4D, 5C, 5S, 5H, 5D, KS, QH"
let estring = "AC, 2H, 5C, 5S, 5H, 5D, KS, QH"
let fstring = "2H, 5D, KS, QH"
let gstring = "2H"
let eremoved = [ create_card Two Spade; create_card Four Diamonds ]
let e = Player.remove_cards d eremoved

let fremoved =
  [
    create_card Ace Clubs;
    create_card Five Clubs;
    create_card Five Spade;
    create_card Five Hearts;
  ]

let f = Player.remove_cards e fremoved

let gremoved =
  [
    create_card Five Diamonds; create_card King Spade; create_card Queen Hearts;
  ]

let g = Player.remove_cards f gremoved
let hremoved = [ create_card Two Hearts ]
let h = Player.remove_cards g hremoved

let non_original_cards =
  [
    create_card Seven Hearts;
    create_card Eight Clubs;
    create_card Nine Diamonds;
    create_card Four Clubs;
  ]

let player_tests =
  [
    opted_test "default opted in" a true;
    won_test "a not won" a false;
    opted_test " b not opted in" b false;
    won_test "b not won" b false;
    opted_test " c opted back in" c true;
    won_test "c not won" c false;
    parse_test "pass" "pass " Pass;
    parse_test "count" "count" Count;
    parse_test "Combo" "combo" Combo;
    parse_test "show" "show " Show;
    parse_test "play" "play 10H" (Play [ Card.create_card Ten Hearts ]);
    parse_test "play" "play 10H 2D"
      (Play [ Card.create_card Ten Hearts; Card.create_card Two Diamonds ]);
    contain_cards_test "still has ace spades" c dremoved true;
    contain_cards_test "no more ace spades" d dremoved false;
    contain_cards_test "no unoriginal cards d" d non_original_cards false;
    contain_cards_test "no unoriginal cards e" e non_original_cards false;
    contain_cards_test "no unoriginal cards f" f non_original_cards false;
    contain_cards_test "no unoriginal cards g" g non_original_cards false;
    contain_cards_test "no unoriginal cards h" h non_original_cards false;
    show_test "d show" d dstring;
    won_test "d not won" d false;
    contain_cards_test "e cards" d eremoved true;
    contain_cards_test "e cards" e eremoved false;
    won_test "e not won" e false;
    show_test "e show" e estring;
    contain_cards_test "still has ace spades" e fremoved true;
    contain_cards_test "no more ace spades" f fremoved false;
    won_test "f not won" f false;
    show_test "f show" f fstring;
    contain_cards_test "e cards" f gremoved true;
    won_test "g not won" g false;
    show_test "g show" g gstring;
    contain_cards_test "e cards" g gremoved false;
    contain_cards_test "e cards" g hremoved true;
    won_test "h has won" h true;
    contain_cards_test "e cards" h hremoved false;
  ]

let highstraight5 =
  Rules.make_combo
    [
      Card.create_card Nine Spade;
      Card.create_card Ten Spade;
      Card.create_card Jack Spade;
      Card.create_card Queen Spade;
      Card.create_card King Spade;
    ]

let lowstraight5 =
  Rules.make_combo
    [
      Card.create_card Eight Spade;
      Card.create_card Nine Spade;
      Card.create_card Ten Spade;
      Card.create_card Jack Spade;
      Card.create_card Queen Spade;
    ]

let straight4 =
  Rules.make_combo
    [
      Card.create_card Nine Spade;
      Card.create_card Ten Spade;
      Card.create_card Jack Spade;
      Card.create_card Queen Spade;
    ]

let straight42 =
  Rules.make_combo
    [
      Card.create_card Nine Spade;
      Card.create_card Ten Spade;
      Card.create_card Jack Spade;
      Card.create_card Queen Hearts;
    ]

let double =
  Rules.make_combo [ Card.create_card Nine Spade; Card.create_card Nine Clubs ]

let triple1 =
  Rules.make_combo
    [
      Card.create_card Four Hearts;
      Card.create_card Four Clubs;
      Card.create_card Four Diamonds;
    ]

let triple2 =
  Rules.make_combo
    [
      Card.create_card Five Hearts;
      Card.create_card Five Clubs;
      Card.create_card Five Diamonds;
    ]

let quad1 =
  Rules.make_combo
    [
      Card.create_card Ace Hearts;
      Card.create_card Ace Clubs;
      Card.create_card Ace Diamonds;
      Card.create_card Ace Spade;
    ]

let quad2 =
  Rules.make_combo
    [
      Card.create_card Five Hearts;
      Card.create_card Five Clubs;
      Card.create_card Five Diamonds;
      Card.create_card Five Spade;
    ]

let single = Rules.make_combo [ Card.create_card Nine Spade ]

let valid_play_test name prevc nextc output =
  name >:: fun _ -> assert_equal output (valid_play prevc nextc)

let rules_tests =
  [
    valid_play_test "single on double" double single false;
    valid_play_test "double on single" single double false;
    valid_play_test "double on straight5" lowstraight5 double false;
    valid_play_test "straight4 on straight4" straight4 straight4 false;
    valid_play_test "straight4 on highstraight5" highstraight5 straight4 false;
    valid_play_test "highstraight5 on lowstraight5" lowstraight5 highstraight5
      true;
    valid_play_test "straight4 on singles" single straight4 false;
    valid_play_test "quad2 is lower than quad1" quad1 quad2 false;
    valid_play_test "quad2 is lower than quad1" quad2 quad1 true;
    valid_play_test "triple1 is lower than triple 2" triple2 triple1 false;
    valid_play_test "triple1 is lower than triple 2" triple1 triple2 true;
    valid_play_test "1 straight4 is lower than straight42" straight4 straight42
      true;
    valid_play_test "2 straight4 is lower than straight42" straight42 straight4
      false;
  ]

let player14 =
  make_player
    [
      create_card Six Hearts;
      create_card Ten Spade;
      create_card Six Diamonds;
      create_card Jack Clubs;
      create_card Eight Clubs;
      create_card Queen Clubs;
      create_card Ace Diamonds;
      create_card Four Spade;
      create_card Nine Spade;
      create_card Queen Diamonds;
      create_card Five Hearts;
      create_card Four Clubs;
      create_card Eight Hearts;
    ]
    true false

let player24 =
  make_player
    [
      create_card Seven Hearts;
      create_card Four Hearts;
      create_card Two Diamonds;
      create_card Three Spade;
      create_card Jack Diamonds;
      create_card Seven Spade;
      create_card Nine Clubs;
      create_card Five Clubs;
      create_card Three Clubs;
      create_card Ace Clubs;
      create_card Queen Hearts;
      create_card Two Spade;
      create_card Five Diamonds;
    ]
    true false

let player34 =
  make_player
    [
      create_card Ten Clubs;
      create_card Three Diamonds;
      create_card Jack Hearts;
      create_card Ten Diamonds;
      create_card Five Spade;
      create_card Six Spade;
      create_card King Clubs;
      create_card Ace Hearts;
      create_card Six Clubs;
      create_card Jack Spade;
      create_card Four Diamonds;
      create_card Three Hearts;
      create_card King Spade;
    ]
    true false

let player44 =
  make_player
    [
      create_card Two Clubs;
      create_card Seven Clubs;
      create_card Nine Diamonds;
      create_card Ten Hearts;
      create_card Eight Spade;
      create_card Two Hearts;
      create_card Eight Diamonds;
      create_card King Hearts;
      create_card Queen Spade;
      create_card King Diamonds;
      create_card Seven Diamonds;
      create_card Ace Spade;
      create_card Nine Hearts;
    ]
    true false

let player4Array = [| player14; player24; player34; player44 |]
let state14 = start player4Array 1
let state_tests = []

let suite =
  "test suite for viet poker"
  >::: List.flatten
         [ card_tests; deck_tests; player_tests; state_tests; rules_tests ]

let _ = run_test_tt_main suite
