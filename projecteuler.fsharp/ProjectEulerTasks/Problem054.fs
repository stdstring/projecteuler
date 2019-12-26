﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System

// In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:
//
// 1) High Card: Highest value card.
// 2) One Pair: Two cards of the same value.
// 3) Two Pairs: Two different pairs.
// 4) Three of a Kind: Three cards of the same value.
// 5) Straight: All cards are consecutive values.
// 6) Flush: All cards of the same suit.
// 7) Full House: Three of a kind and a pair.
// 8) Four of a Kind: Four cards of the same value.
// 9) Straight Flush: All cards are consecutive values of same suit.
// 10) Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
//
// The cards are valued in the order: 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
// If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below).
// But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.
//
// Consider the following five hands dealt to two players:
// Hand : Player 1 : Player 2 : Winner
// 1 : 5H 5C 6S 7S KD (Pair of Fives) : 2C 3S 8S 8D TD (Pair of Eights) : Player 2
// 2 : 5D 8C 9S JS AC (Highest card Ace) : 2C 5C 7D 8S QH (Highest card Queen) : Player 1
// 3 : 2D 9C AS AH AC (Three Aces) : 3D 6D 7D TD QD (Flush with Diamonds) : Player 2
// 4 : 4D 6S 9H QH QC (Pair of Queens, Highest card Nine) : 3D 6D 7H QD QS (Pair of Queens, Highest card Seven) : Player 1
// 5 : 2H 2D 4C 4D 4S (Full House with Three Fours) : 3C 3D 3S 9S 9D (Full House with Three Threes) : Player 1
//
// The file, problem_054.dat, contains one-thousand random hands dealt to two players.
// Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards.
// You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.
// How many hands does Player 1 win?

module Problem054Impl =

    [<Literal>]
    let JackValue = 11

    [<Literal>]
    let QueenValue = 12

    [<Literal>]
    let KingValue = 13

    [<Literal>]
    let AceValue = 14

    [<Literal>]
    let HighCardRank = 1

    [<Literal>]
    let OnePairRank = 2

    [<Literal>]
    let TwoPairsRank = 3

    [<Literal>]
    let ThreeKindRank = 4

    [<Literal>]
    let StraightRank = 5

    [<Literal>]
    let FlushRank = 6

    [<Literal>]
    let FullHouseRank = 7

    [<Literal>]
    let FourKindRank = 8

    [<Literal>]
    let StraightFlushRank = 9

    [<Literal>]
    let RoyalFlushRank = 10

    type Card = {Value: int; Suit: char}

    type Hands = {Player1: List<Card>; Player2: List<Card>}

    type HandRank =
    | HighCard of card: Card
    | OnePair of card: Card
    | TwoPairs of card1: Card * card2: Card
    | ThreeKind of card: Card
    | Straight of highCard: Card
    | Flush
    | FullHouse of card1: Card * card2: Card
    | FourKind of card: Card
    | StraightFlush of highCard: Card
    | RoyalFlush

        member public this.GetValue() =
            match this with
            | HighCard _ -> HighCardRank
            | OnePair _ -> OnePairRank
            | TwoPairs _ -> TwoPairsRank
            | ThreeKind _ -> ThreeKindRank
            | Straight _ -> StraightRank
            | Flush -> FlushRank
            | FullHouse _ -> FullHouseRank
            | FourKind _ -> FourKindRank
            | StraightFlush _ -> StraightFlushRank
            | RoyalFlush -> RoyalFlushRank

    type HandsWinner =
    | Player1
    | Player2
    | Draw

open Problem054Impl

[<TestFixture>]
type Problem054() =

    let playerCardsCount = 5

    let parseHands (handsData: string) =
        let parseCard = function
            | 'A' :: suit :: [] -> {Card.Value = AceValue; Card.Suit = suit}
            | 'K' :: suit :: [] -> {Card.Value = KingValue; Card.Suit = suit}
            | 'Q' :: suit :: [] -> {Card.Value = QueenValue; Card.Suit = suit}
            | 'J' :: suit :: [] -> {Card.Value = JackValue; Card.Suit = suit}
            | 'T' :: suit :: [] -> {Card.Value = 10; Card.Suit = suit}
            | value :: suit :: [] -> {Card.Value = Char.GetNumericValue(value) |> int; Card.Suit = suit}
            | _ -> failwith "Unknown format of data"
        let cardsData = handsData.Split(' ')
        let player1Cards = cardsData |> Seq.take playerCardsCount |> Seq.map (fun cardData -> cardData |> Seq.toList |> parseCard) |> Seq.sortByDescending (fun card -> card.Value) |> Seq.toList
        let player2Cards = cardsData |> Seq.skip playerCardsCount |> Seq.map (fun cardData -> cardData |> Seq.toList |> parseCard) |> Seq.sortByDescending (fun card -> card.Value) |> Seq.toList
        {Hands.Player1 = player1Cards; Hands.Player2 = player2Cards}

    let (|RoyalFlush|_|) = function
        | [{Card.Value = AceValue; Card.Suit = suit1};
           {Card.Value = KingValue; Card.Suit = suit2};
           {Card.Value = QueenValue; Card.Suit = suit3};
           {Card.Value = JackValue; Card.Suit = suit4};
           {Card.Value = 10; Card.Suit = suit5}] when (suit1 = suit2) && (suit2 = suit3) && (suit3 = suit4) && (suit4 = suit5) -> Some ()
        | _ -> None

    let (|StraightFlush|_|) = function
        | [{Card.Value = value1; Card.Suit = suit1};
           {Card.Value = value2; Card.Suit = suit2};
           {Card.Value = value3; Card.Suit = suit3};
           {Card.Value = value4; Card.Suit = suit4};
           {Card.Value = value5; Card.Suit = suit5}]
          when (suit1 = suit2) && (suit2 = suit3) && (suit3 = suit4) && (suit4 = suit5) && (value1 - value2 = 1) && (value2 - value3 = 1) && (value3 - value4 = 1) && (value4 - value5 = 1) -> Some {Card.Value = value1; Card.Suit = suit1}
        | _ -> None

    let (|FourKind|_|) = function
        | [{Card.Value = value1; Card.Suit = suit}; {Card.Value = value2}; {Card.Value = value3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 = value2) && (value2 = value3) && (value3 = value4) && (value4 <> value5) -> Some {Card.Value = value1; Card.Suit = suit}
        | [{Card.Value = value1}; {Card.Value = value2; Card.Suit = suit}; {Card.Value = value3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 <> value2) && (value2 = value3) && (value3 = value4) && (value4 = value5) -> Some {Card.Value = value2; Card.Suit = suit}
        | _ -> None

    let (|FullHouse|_|) = function
        | [{Card.Value = value1; Card.Suit = suit1}; {Card.Value = value2}; {Card.Value = value3}; {Card.Value = value4; Card.Suit = suit4}; {Card.Value = value5}]
          when (value1 = value2) && (value2 = value3) && (value3 <> value4) && (value4 = value5) -> Some ({Card.Value = value1; Card.Suit = suit1}, {Card.Value = value4; Card.Suit = suit4})
        | [{Card.Value = value1; Card.Suit = suit1}; {Card.Value = value2}; {Card.Value = value3; Card.Suit = suit3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 = value2) && (value2 <> value3) && (value3 = value4) && (value4 = value5) -> Some ({Card.Value = value1; Card.Suit = suit1}, {Card.Value = value3; Card.Suit = suit3})
        | _ -> None

    let (|Flush|_|) = function
        | [{Card.Suit = suit1}; {Card.Suit = suit2}; {Card.Suit = suit3}; {Card.Suit = suit4}; {Card.Suit = suit5}]
          when (suit1 = suit2) && (suit2 = suit3) && (suit3 = suit4) && (suit4 = suit5) -> Some ()
        | _ ->  None

    let (|Straight|_|) = function
        | [{Card.Value = value1; Card.Suit = suit1}; {Card.Value = value2}; {Card.Value = value3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 - value2 = 1) && (value2 - value3 = 1) && (value3 - value4 = 1) && (value4 - value5 = 1) -> Some {Card.Value = value1; Card.Suit = suit1}
        | _ -> None

    let (|ThreeKind|_|) = function
        | [{Card.Value = value1; Card.Suit = suit1}; {Card.Value = value2}; {Card.Value = value3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 = value2) && (value2 = value3) && (value3 <> value4) && (value4 <> value5) -> Some {Card.Value = value1; Card.Suit = suit1}
        | [{Card.Value = value1}; {Card.Value = value2; Card.Suit = suit2}; {Card.Value = value3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 <> value2) && (value2 = value3) && (value3 = value4) && (value4 <> value5) -> Some {Card.Value = value2; Card.Suit = suit2}
        | [{Card.Value = value1}; {Card.Value = value2}; {Card.Value = value3; Card.Suit = suit3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 <> value2) && (value2 <> value3) && (value3 = value4) && (value4 = value5) -> Some {Card.Value = value3; Card.Suit = suit3}
        | _ -> None

    let (|TwoPairs|_|) = function
        | [{Card.Value = value1; Card.Suit = suit1}; {Card.Value = value2}; {Card.Value = value3; Card.Suit = suit3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 = value2) && (value2 <> value3) && (value3 = value4) && (value4 <> value5) -> Some ({Card.Value = value1; Card.Suit = suit1}, {Card.Value = value3; Card.Suit = suit3})
        | [{Card.Value = value1; Card.Suit = suit1}; {Card.Value = value2}; {Card.Value = value3}; {Card.Value = value4; Card.Suit = suit4}; {Card.Value = value5}]
          when (value1 = value2) && (value2 <> value3) && (value3 <> value4) && (value4 = value5) -> Some ({Card.Value = value1; Card.Suit = suit1}, {Card.Value = value4; Card.Suit = suit4})
        | [{Card.Value = value1}; {Card.Value = value2; Card.Suit = suit2}; {Card.Value = value3}; {Card.Value = value4; Card.Suit = suit4}; {Card.Value = value5}]
          when (value1 <> value2) && (value2 = value3) && (value3 <> value4) && (value4 = value5) -> Some ({Card.Value = value2; Card.Suit = suit2}, {Card.Value = value4; Card.Suit = suit4})
        | _ -> None

    let (|OnePair|_|) = function
        | [{Card.Value = value1; Card.Suit = suit1}; {Card.Value = value2}; {Card.Value = value3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 = value2) && (value2 <> value3) && (value3 <> value4) && (value4 <> value5) -> Some {Card.Value = value1; Card.Suit = suit1}
        | [{Card.Value = value1}; {Card.Value = value2; Card.Suit = suit2}; {Card.Value = value3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 <> value2) && (value2 = value3) && (value3 <> value4) && (value4 <> value5) -> Some {Card.Value = value2; Card.Suit = suit2}
        | [{Card.Value = value1}; {Card.Value = value2}; {Card.Value = value3; Card.Suit = suit3}; {Card.Value = value4}; {Card.Value = value5}]
          when (value1 <> value2) && (value2 <> value3) && (value3 = value4) && (value4 <> value5) -> Some {Card.Value = value3; Card.Suit = suit3}
        | [{Card.Value = value1}; {Card.Value = value2}; {Card.Value = value3}; {Card.Value = value4; Card.Suit = suit4}; {Card.Value = value5}]
          when (value1 <> value2) && (value2 <> value3) && (value3 <> value4) && (value4 = value5) -> Some {Card.Value = value4; Card.Suit = suit4}
        | _ -> None

    let calcHandRank = function
        | RoyalFlush -> HandRank.RoyalFlush
        | StraightFlush highCard -> HandRank.StraightFlush highCard
        | FourKind card -> HandRank.FourKind card
        | FullHouse (card1, card2) -> HandRank.FullHouse (card1, card2)
        | Flush -> HandRank.Flush
        | Straight highCard -> HandRank.Straight highCard
        | ThreeKind card -> HandRank.ThreeKind card
        | TwoPairs (card1, card2) -> HandRank.TwoPairs (card1, card2)
        | OnePair card -> HandRank.OnePair card
        | {Card.Value= value1; Card.Suit = suit1} :: _ -> HandRank.HighCard {Card.Value = value1; Card.Suit = suit1}
        | _ -> failwith "Unexpected branch of match expression"

    let rec compareCards (player1: List<Card>) (player2: List<Card>) =
        match player1, player2 with
        | [], [] -> HandsWinner.Draw
        | player1Card :: _, player2Card :: _ when player1Card.Value > player2Card.Value -> HandsWinner.Player1
        | player1Card :: _, player2Card :: _ when player1Card.Value < player2Card.Value -> HandsWinner.Player2
        | _ :: player1CardsRest, _ :: player2CardsRest -> compareCards player1CardsRest player2CardsRest
        | _ -> failwith "Unexpected branch of match expression"

    let compareHands {Hands.Player1 = player1Cards; Hands.Player2 = player2Cards} =
        let player1HandRank = player1Cards |> calcHandRank
        let player2HandRank = player2Cards |> calcHandRank
        match player1HandRank, player2HandRank with
        | _ when player1HandRank.GetValue() > player2HandRank.GetValue() -> HandsWinner.Player1
        | _ when player1HandRank.GetValue() < player2HandRank.GetValue() -> HandsWinner.Player2
        | HandRank.RoyalFlush, HandRank.RoyalFlush -> HandsWinner.Draw
        | HandRank.StraightFlush highCard1, HandRank.StraightFlush highCard2 when highCard1.Value > highCard2.Value -> HandsWinner.Player1
        | HandRank.StraightFlush highCard1, HandRank.StraightFlush highCard2 when highCard1.Value < highCard2.Value -> HandsWinner.Player2
        | HandRank.StraightFlush highCard1, HandRank.StraightFlush highCard2 when highCard1.Value = highCard2.Value -> HandsWinner.Draw
        | HandRank.FourKind card1, HandRank.FourKind card2 when card1.Value > card2.Value -> HandsWinner.Player1
        | HandRank.FourKind card1, HandRank.FourKind card2 when card1.Value < card2.Value -> HandsWinner.Player2
        | HandRank.FullHouse (threeCard1, _), HandRank.FullHouse (threeCard2, _) when threeCard1.Value > threeCard2.Value -> HandsWinner.Player1
        | HandRank.FullHouse (threeCard1, _), HandRank.FullHouse (threeCard2, _) when threeCard1.Value < threeCard2.Value -> HandsWinner.Player2
        | HandRank.FullHouse (threeCard1, pairCard1), HandRank.FullHouse (threeCard2, pairCard2) when (threeCard1.Value = threeCard2.Value) && (pairCard1.Value > pairCard2.Value) -> HandsWinner.Player1
        | HandRank.FullHouse (threeCard1, pairCard1), HandRank.FullHouse (threeCard2, pairCard2) when (threeCard1.Value = threeCard2.Value) && (pairCard1.Value < pairCard2.Value) -> HandsWinner.Player2
        | HandRank.FullHouse (threeCard1, pairCard1), HandRank.FullHouse (threeCard2, pairCard2) when (threeCard1.Value = threeCard2.Value) && (pairCard1.Value = pairCard2.Value) -> HandsWinner.Draw
        | HandRank.Straight highCard1, HandRank.Straight highCard2 when highCard1.Value > highCard2.Value -> HandsWinner.Player1
        | HandRank.Straight highCard1, HandRank.Straight highCard2 when highCard1.Value < highCard2.Value -> HandsWinner.Player2
        | HandRank.Straight highCard1, HandRank.Straight highCard2 when highCard1.Value = highCard2.Value -> HandsWinner.Draw
        | HandRank.ThreeKind card1, HandRank.ThreeKind card2 when card1.Value > card2.Value -> HandsWinner.Player1
        | HandRank.ThreeKind card1, HandRank.ThreeKind card2 when card1.Value < card2.Value -> HandsWinner.Player2
        | HandRank.TwoPairs (pair1Card1, _), HandRank.TwoPairs (pair1Card2, _) when pair1Card1.Value > pair1Card2.Value -> HandsWinner.Player1
        | HandRank.TwoPairs (pair1Card1, _), HandRank.TwoPairs (pair1Card2, _) when pair1Card1.Value < pair1Card2.Value -> HandsWinner.Player2
        | HandRank.TwoPairs (pair1Card1, pair2Card1), HandRank.TwoPairs (pair1Card2, pair2Card2) when (pair1Card1.Value = pair1Card2.Value) && (pair2Card1.Value > pair2Card2.Value) -> HandsWinner.Player1
        | HandRank.TwoPairs (pair1Card1, pair2Card1), HandRank.TwoPairs (pair1Card2, pair2Card2) when (pair1Card1.Value = pair1Card2.Value) && (pair2Card1.Value < pair2Card2.Value) -> HandsWinner.Player2
        | HandRank.OnePair card1, HandRank.OnePair card2 when card1.Value > card2.Value -> HandsWinner.Player1
        | HandRank.OnePair card1, HandRank.OnePair card2 when card1.Value < card2.Value -> HandsWinner.Player2
        | _ -> compareCards player1Cards player2Cards

    let solveImpl (dataFilename: string) =
        File.ReadAllLines(Path.Combine("Data", dataFilename)) |> Seq.map parseHands |> Seq.map compareHands |> Seq.filter (fun winner -> winner = HandsWinner.Player1) |> Seq.length

    [<TestCase("problem_054.dat", 376, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)