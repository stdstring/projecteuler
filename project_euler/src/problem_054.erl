%% In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:
%%
%% 1) High Card: Highest value card.
%% 2) One Pair: Two cards of the same value.
%% 3) Two Pairs: Two different pairs.
%% 4) Three of a Kind: Three cards of the same value.
%% 5) Straight: All cards are consecutive values.
%% 6) Flush: All cards of the same suit.
%% 7) Full House: Three of a kind and a pair.
%% 8) Four of a Kind: Four cards of the same value.
%% 9) Straight Flush: All cards are consecutive values of same suit.
%% 10) Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
%%
%% The cards are valued in the order: 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
%% If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below).
%% But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below);
%% if the highest cards tie then the next highest cards are compared, and so on.
%%
%% Consider the following five hands dealt to two players:
%% Hand : Player 1 : Player 2 : Winner
%% 1 : 5H 5C 6S 7S KD (Pair of Fives) : 2C 3S 8S 8D TD (Pair of Eights) : Player 2
%% 2 : 5D 8C 9S JS AC (Highest card Ace) : 2C 5C 7D 8S QH (Highest card Queen) : Player 1
%% 3 : 2D 9C AS AH AC (Three Aces) : 3D 6D 7D TD QD (Flush with Diamonds) : Player 2
%% 4 : 4D 6S 9H QH QC (Pair of Queens, Highest card Nine) : 3D 6D 7H QD QS (Pair of Queens, Highest card Seven) : Player 1
%% 5 : 2H 2D 4C 4D 4S (Full House with Three Fours) : 3C 3D 3S 9S 9D (Full House with Three Threes) : Player 1
%%
%% The file, problem_054.dat, contains one-thousand random hands dealt to two players.
%% Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards.
%% You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.
%% How many hands does Player 1 win?

-module(problem_054).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(TEN_CHAR, $T).
-define(JACK_CHAR, $J).
-define(QUEEN_CHAR, $Q).
-define(KING_CHAR, $K).
-define(ACE_CHAR, $A).

-define(TEN, 10).
-define(JACK, 11).
-define(QUEEN, 12).
-define(KING, 13).
-define(ACE, 14).

-define(HIGH_CARD, 1).
-define(ONE_PAIR, 2).
-define(TWO_PAIRS, 3).
-define(THREE_KIND, 4).
-define(STRAIGHT, 5).
-define(FLUSH, 6).
-define(FULL_HOUSE, 7).
-define(FOUR_KIND, 8).
-define(STRAIGHT_FLUSH, 9).
-define(ROYAL_FLUSH, 10).

get_check_data() ->
    [{"problem_054.dat", 376}].

prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_strings(filename:join(ModuleSourceDir, Filename)).

solve(HandsTable) ->
    CollectFun = fun(HandsRow, Count) ->
        Hands = prepare_hands(HandsRow),
        case check_hands(Hands) of
            first -> Count + 1;
            _Other -> Count
        end
    end,
    lists:foldl(CollectFun, 0, HandsTable).

-spec prepare_hands(HandsSource :: string()) -> {Hand1 :: [{Value :: pos_integer(), Suit :: char()}], Hand2 :: [{Value :: pos_integer(), Suit :: char()}]}.
prepare_hands(HandsSource) ->
    HandsData = string:tokens(HandsSource, " "),
    {Hand1, Hand2} = parse_hands(HandsData),
    {sort_cards(Hand1), sort_cards(Hand2)}.

-spec parse_hands(HandsData :: [string()]) -> {Hand1 :: [{Value :: pos_integer(), Suit :: char()}], Hand2 :: [{Value :: pos_integer(), Suit :: char()}]}.
parse_hands([Player1Card1, Player1Card2, Player1Card3, Player1Card4, Player1Card5, Player2Card1, Player2Card2, Player2Card3, Player2Card4, Player2Card5]) ->
    {[parse_card(Player1Card1), parse_card(Player1Card2), parse_card(Player1Card3),parse_card(Player1Card4), parse_card(Player1Card5)],
     [parse_card(Player2Card1), parse_card(Player2Card2), parse_card(Player2Card3),parse_card(Player2Card4), parse_card(Player2Card5)]}.

-spec parse_card(Card :: string()) -> {Value :: pos_integer(), Suit :: char()}.
parse_card([?TEN_CHAR, Suit]) -> {?TEN, Suit};
parse_card([?JACK_CHAR, Suit]) -> {?JACK, Suit};
parse_card([?QUEEN_CHAR, Suit]) -> {?QUEEN, Suit};
parse_card([?KING_CHAR, Suit]) -> {?KING, Suit};
parse_card([?ACE_CHAR, Suit]) -> {?ACE, Suit};
parse_card([Value, Suit]) -> {Value - $0, Suit}.

-spec sort_cards(Hand :: [{Value :: pos_integer(), Suit :: char()}]) -> [{Value :: pos_integer(), Suit :: char()}].
sort_cards(Hand) ->
    %% sorting in back order
    lists:sort(fun({Value1, _Suit1}, {Value2, _Suit2}) -> Value1 > Value2 end, Hand).

-spec calc_hand_rank([{Value :: pos_integer(), Suit :: char()}]) -> {Rank :: 1..10, MaxValue :: non_neg_integer(), MinValue :: non_neg_integer()}.
%% ROYAL FLUSH
calc_hand_rank([{?ACE, Suit}, {?KING, Suit}, {?QUEEN, Suit}, {?JACK, Suit}, {?TEN, Suit}]) -> {?ROYAL_FLUSH, 0, 0};
%% STRAIGHT FLUSH
calc_hand_rank([{Value1, Suit}, {Value2, Suit}, {Value3, Suit}, {Value4, Suit}, {Value5, Suit}])
    when Value2 == Value1 - 1, Value3 == Value1 - 2, Value4 == Value1 - 3, Value5 == Value1 - 4 -> {?STRAIGHT_FLUSH, Value1, Value5};
calc_hand_rank([{Value1, Suit}, {Value2, Suit}, {Value3, Suit}, {Value4, Suit}, {Value5, Suit}])
    when Value4 == Value5 + 1, Value3 == Value5 + 2, Value2 == Value5 + 3, Value1 == Value5 + 4 -> {?STRAIGHT_FLUSH, Value1, Value5};
%% FOUR OF A KIND
calc_hand_rank([{Value, _Suit1}, {Value, _Suit2}, {Value, _Suit3}, {Value, _Suit4}, {_Value5, _Suit5}]) -> {?FOUR_KIND, Value, 0};
calc_hand_rank([{_Value1, _Suit1}, {Value, _Suit2}, {Value, _Suit3}, {Value, _Suit4}, {Value, _Suit5}]) -> {?FOUR_KIND, Value, 0};
%% FULL HOUSE
calc_hand_rank([{Value1, _Suit1}, {Value1, _Suit2}, {Value3, _Suit3}, {Value3, _Suit4}, {Value3, _Suit5}]) -> {?FULL_HOUSE, Value1, Value3};
calc_hand_rank([{Value1, _Suit1}, {Value1, _Suit2}, {Value1, _Suit3}, {Value3, _Suit4}, {Value3, _Suit5}]) -> {?FULL_HOUSE, Value1, Value3};
%% FLUSH
calc_hand_rank([{_Value1, Suit}, {_Value2, Suit}, {_Value3, Suit}, {_Value4, Suit}, {_Value5, Suit}]) -> {?FLUSH, 0, 0};
%% STRAIGHT
calc_hand_rank([{Value1, _Suit1}, {Value2, _Suit2}, {Value3, _Suit3}, {Value4, _Suit4}, {Value5, _Suit5}])
    when Value2 == Value1 - 1, Value3 == Value1 - 2, Value4 == Value1 - 3, Value5 == Value1 - 4 -> {?STRAIGHT, Value1, Value5};
calc_hand_rank([{Value1, _Suit1}, {Value2, _Suit2}, {Value3, _Suit3}, {Value4, _Suit4}, {Value5, _Suit5}])
    when Value4 == Value5 + 1, Value3 == Value5 + 2, Value2 == Value5 + 3, Value1 == Value5 + 4 -> {?STRAIGHT, Value1, Value5};
%% THREE OF A KIND
calc_hand_rank([{Value, _Suit1}, {Value, _Suit2}, {Value, _Suit3}, {_Value4, _Suit4}, {_Value5, _Suit5}]) -> {?THREE_KIND, Value, 0};
calc_hand_rank([{_Value1, _Suit1}, {Value, _Suit2}, {Value, _Suit3}, {Value, _Suit4}, {_Value5, _Suit5}]) -> {?THREE_KIND, Value, 0};
calc_hand_rank([{_Value1, _Suit1}, {_Value2, _Suit2}, {Value, _Suit3}, {Value, _Suit4}, {Value, _Suit5}]) -> {?THREE_KIND, Value, 0};
%% TWO PAIRS
calc_hand_rank([{Value1, _Suit1}, {Value1, _Suit2}, {Value3, _Suit3}, {Value3, _Suit4}, {_Value5, _Suit5}]) -> {?TWO_PAIRS, Value1, Value3};
calc_hand_rank([{Value1, _Suit1}, {Value1, _Suit2}, {_Value3, _Suit3}, {Value4, _Suit4}, {Value4, _Suit5}]) -> {?TWO_PAIRS, Value1, Value4};
calc_hand_rank([{_Value1, _Suit1}, {Value2, _Suit2}, {Value2, _Suit3}, {Value4, _Suit4}, {Value4, _Suit5}]) -> {?TWO_PAIRS, Value2, Value4};
%% ONE PAIR
calc_hand_rank([{Value, _Suit1}, {Value, _Suit2}, {_Value3, _Suit3}, {_Value4, _Suit4}, {_Value5, _Suit5}]) -> {?ONE_PAIR, Value, 0};
calc_hand_rank([{_Value1, _Suit1}, {Value, _Suit2}, {Value, _Suit3}, {_Value4, _Suit4}, {_Value5, _Suit5}]) -> {?ONE_PAIR, Value, 0};
calc_hand_rank([{_Value1, _Suit1}, {_Value2, _Suit2}, {Value, _Suit3}, {Value, _Suit4}, {_Value5, _Suit5}]) -> {?ONE_PAIR, Value, 0};
calc_hand_rank([{_Value1, _Suit1}, {_Value2, _Suit2}, {_Value3, _Suit3}, {Value, _Suit4}, {Value, _Suit5}]) -> {?ONE_PAIR, Value, 0};
%% HIGH CARD
calc_hand_rank([_Card1, _Card2, _Card3, _Card4, _Card5]) -> {?HIGH_CARD, 0, 0}.

-spec check_hands({Hand1 :: [{Value :: pos_integer(), Suit :: char()}], Hand2 :: [{Value :: pos_integer(), Suit :: char()}]}) -> 'undefined' | 'first' | 'second'.
check_hands({Hand1, Hand2}) ->
    Rank1 = calc_hand_rank(Hand1),
    Rank2 = calc_hand_rank(Hand2),
    if
        Rank1 > Rank2 -> first;
        Rank1 < Rank2 -> second;
        Rank1 == Rank2 -> check_hands_cards(Hand1, Hand2)
    end.

-spec check_hands_cards(Card1 :: {Value :: pos_integer(), Suit :: char()}, Card2 :: {Value :: pos_integer(), Suit :: char()}) -> 'undefined' | 'first' | 'second'.
check_hands_cards([], []) -> undefined;
check_hands_cards([{Value1, _Suit1} | _Rest1], [{Value2, _Suit2} | _Rest2]) when Value1 > Value2 -> first;
check_hands_cards([{Value1, _Suit1} | _Rest1], [{Value2, _Suit2} | _Rest2]) when Value1 < Value2 -> second;
check_hands_cards([{Value, _Suit1} | Rest1], [{Value, _Suit2} | Rest2]) -> check_hands_cards(Rest1, Rest2).