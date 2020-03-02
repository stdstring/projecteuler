namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System
open System.Collections.Generic

// In the game, Monopoly, the standard board is set up in the following way:

// GO   A1   CC1   A2   T1   R1   B1   CH1   B2   B3   JAIL
// H2                                                    C1
// T2                                                    U1
// H1                                                    C2
// CH3                                                   C3
// R4                                                    R2
// G3                                                    D1
// CC3                                                  CC2
// G2                                                    D2
// G1                                                    D3
// G2J   F3   U2   F2   F1   R3   E3   E2   CH2   E1     FP

// A player starts on the GO square and adds the scores on two 6-sided dice to determine the number of squares they advance in a clockwise direction.
// Without any further rules we would expect to visit each square with equal probability: 2.5%.
// However, landing on G2J (Go To Jail), CC (community chest), and CH (chance) changes this distribution.
// In addition to G2J, and one card from each of CC and CH, that orders the player to go directly to jail, if a player rolls three consecutive doubles, they do not advance the result of their 3rd roll. Instead they proceed directly to jail.
// At the beginning of the game, the CC and CH cards are shuffled.
// When a player lands on CC or CH they take a card from the top of the respective pile and, after following the instructions, it is returned to the bottom of the pile.
// There are sixteen cards in each pile, but for the purpose of this problem we are only concerned with cards that order a movement; any instruction not concerned with movement will be ignored and the player will remain on the CC/CH square.
// Community Chest (2/16 cards):
// Advance to GO
// Go to JAIL
// Chance (10/16 cards):
// Advance to GO
// Go to JAIL
// Go to C1
// Go to E3
// Go to H2
// Go to R1
// Go to next R (railway company)
// Go to next R
// Go to next U (utility company)
// Go back 3 squares.
// The heart of this problem concerns the likelihood of visiting a particular square. That is, the probability of finishing at that square after a roll.
// For this reason it should be clear that, with the exception of G2J for which the probability of finishing on it is zero,
// the CH squares will have the lowest probabilities, as 5/8 request a movement to another square, and it is the final square that the player finishes at on each roll that we are interested in.
// We shall make no distinction between "Just Visiting" and being sent to JAIL, and we shall also ignore the rule about requiring a double to "get out of jail", assuming that they pay to get out on their next turn.
// By starting at GO and numbering the squares sequentially from 00 to 39 we can concatenate these two-digit numbers to produce strings that correspond with sets of squares.
// Statistically it can be shown that the three most popular squares, in order, are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square 00.
// So these three most popular squares can be listed with the six-digit modal string: 102400.
// If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal string.

module Problem084Impl =
    type SimulationState = {NextRBoard: int[];
                            NextUBoard: int[];
                            Back3Board: int[];
                            CommunityChestCards: string[];
                            ChanceCards: string[];
                            BoardValues: int[];
                            mutable RandomGenerator: Random;
                            mutable DicesDoublesCount: int;
                            mutable CommunityChestIndex: int;
                            mutable ChanceIndex: int;
                            mutable CurrentCell: int;}

open Problem084Impl

[<TestFixture>]
type Problem084() =

    [<Literal>]
    let SimulationStepCount = 10000000

    [<Literal>]
    let RandomGeneratorReplacementStep = 12357

    [<Literal>]
    let CardsShuffleCount = 200

    let board = [|"GO"; "A1"; "CC1"; "A2"; "T1"; "R1"; "B1"; "CH1"; "B2"; "B3"; "JAIL"; "C1"; "U1"; "C2"; "C3"; "R2"; "D1"; "CC2"; "D2"; "D3";
                  "FP"; "E1"; "CH2"; "E2"; "E3"; "R3"; "F1"; "F2"; "U2"; "F3"; "G2J"; "G1"; "G2"; "CC3"; "G3"; "R4"; "CH3"; "H1"; "T2"; "H2"|]

    let getCellByValue (cellValue: string) = cellValue |> (board :> IList<string>).IndexOf

    let goCell = "GO" |> getCellByValue

    let jailCell = "JAIL" |> getCellByValue

    let g2jCell = "G2J" |> getCellByValue

    let cc1Cell = "CC1" |> getCellByValue

    let cc2Cell = "CC2" |> getCellByValue

    let cc3Cell = "CC3" |> getCellByValue

    let ch1Cell = "CH1" |> getCellByValue

    let ch2Cell = "CH2" |> getCellByValue

    let ch3Cell = "CH3" |> getCellByValue

    let c1Cell = "C1" |> getCellByValue

    let e3Cell = "E3" |> getCellByValue

    let h2Cell = "H2" |> getCellByValue

    let r1Cell = "R1" |> getCellByValue

    let createNextRBoard () =
        let nextRBoard = Array.create board.Length -1
        // CH1 - R2, CH2 - R3, CH3 - R1
        nextRBoard.[ch1Cell] <- ("R2" |> getCellByValue)
        nextRBoard.[ch2Cell] <- ("R3" |> getCellByValue)
        nextRBoard.[ch3Cell] <- r1Cell
        nextRBoard

    let createNextUBoard () =
        let nextUBoard = Array.create board.Length -1
        // CH1 - U1, CH2 -U2, CH3 - U1
        nextUBoard.[ch1Cell] <- ("U1" |> getCellByValue)
        nextUBoard.[ch2Cell] <- ("U2" |> getCellByValue)
        nextUBoard.[ch3Cell] <- ("U1" |> getCellByValue)
        nextUBoard

    let createBack3Board () =
        let back3Board = Array.create board.Length -1
        // CH1 - T1, CH2 - D3, CH3 -CC3
        back3Board.[ch1Cell] <- ("T1" |> getCellByValue)
        back3Board.[ch2Cell] <- ("D3" |> getCellByValue)
        back3Board.[ch3Cell] <- cc3Cell
        back3Board

    let shuffleCards (shuffleCount: int) (cards: string[]) =
        let swap (storage: string[]) (indexA: int) (indexB: int) =
            let value = storage.[indexA]
            storage.[indexA] <- storage.[indexB]
            storage.[indexB] <- value
        let randomGenerator = new System.Random();
        for _ in seq {1 .. shuffleCount} do
            let indexA = randomGenerator.Next(cards.Length)
            let indexB = randomGenerator.Next(cards.Length)
            swap cards indexA indexB

    let createCommunityChestCards () =
        let communityChestCards = [|"GO"; "JAIL"; "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"|]
        communityChestCards |> shuffleCards CardsShuffleCount
        communityChestCards

    let createChanceCards () =
        let chanceCards = [|"GO"; "JAIL"; "C1"; "E3"; "H2"; "R1"; "NextR"; "NextR"; "NextU"; "Back3"; "-"; "-"; "-"; "-"; "-"; "-"|]
        chanceCards |> shuffleCards CardsShuffleCount
        chanceCards

    let processCommunityChestCard (state: SimulationState) =
        match state.CommunityChestCards.[state.CommunityChestIndex] with
        | "GO" -> state.CurrentCell <- goCell
        | "JAIL" -> state.CurrentCell <- jailCell
        | _ -> ()
        state.CommunityChestIndex <- (state.CommunityChestIndex + 1) % state.CommunityChestCards.Length

    let processChanceCard (state: SimulationState) =
        match state.ChanceCards.[state.ChanceIndex] with
        | "GO" ->
            state.CurrentCell <- goCell
        | "JAIL" ->
            state.CurrentCell <- jailCell
        | "C1" ->
            state.CurrentCell <- c1Cell
        | "E3" ->
            state.CurrentCell <- e3Cell
        | "H2" ->
            state.CurrentCell <- h2Cell
        | "R1" ->
            state.CurrentCell <- r1Cell
        | "NextR" ->
            state.CurrentCell <- state.NextRBoard.[state.CurrentCell]
        | "NextU" ->
            state.CurrentCell <- state.NextUBoard.[state.CurrentCell]
        | "Back3" ->
            state.CurrentCell <- state.Back3Board.[state.CurrentCell]
            if state.CurrentCell = cc3Cell then
                state |> processCommunityChestCard
        | _ ->
            ()
        state.ChanceIndex <- (state.ChanceIndex + 1) % state.ChanceCards.Length

    // solution based on simulation gives incorrect result periodically (it is obviously)
    let solveImpl (diceSideCount: int) =
        let state = {SimulationState.NextRBoard = createNextRBoard ();
                     SimulationState.NextUBoard = createNextUBoard ();
                     SimulationState.Back3Board = createBack3Board ();
                     SimulationState.CommunityChestCards = createCommunityChestCards ();
                     SimulationState.ChanceCards = createChanceCards ();
                     SimulationState.BoardValues = Array.zeroCreate board.Length;
                     SimulationState.RandomGenerator = new Random();
                     SimulationState.DicesDoublesCount = 0;
                     SimulationState.CommunityChestIndex = 0;
                     SimulationState.ChanceIndex = 0;
                     SimulationState.CurrentCell = "GO" |> getCellByValue}
        for step in {1 .. SimulationStepCount} do
            if step % RandomGeneratorReplacementStep = 0 then
                state.RandomGenerator <- new Random()
            // generate dices throw
            let dice1 = state.RandomGenerator.Next(1, diceSideCount + 1)
            let dice2 = state.RandomGenerator.Next(1, diceSideCount + 1)
            state.DicesDoublesCount <- if dice1 = dice2 then state.DicesDoublesCount + 1 else 0
            match state.DicesDoublesCount with
            | 3  ->
                state.CurrentCell <- jailCell
                state.DicesDoublesCount <- 0
            | _ ->
                state.CurrentCell <- (state.CurrentCell + dice1 + dice2) % board.Length
                match state.CurrentCell with
                | _ when state.CurrentCell = g2jCell ->
                    state.CurrentCell <- jailCell
                | _ when state.CurrentCell = cc1Cell || state.CurrentCell = cc2Cell || state.CurrentCell = cc3Cell ->
                    state |> processCommunityChestCard
                | _ when state.CurrentCell = ch1Cell || state.CurrentCell = ch2Cell || state.CurrentCell = ch3Cell ->
                    state |> processChanceCard
                | _ ->
                    ()
            state.BoardValues.[state.CurrentCell] <- state.BoardValues.[state.CurrentCell] + 1
        // result
        let indicies = [0 .. board.Length - 1] |> List.sortByDescending (fun index -> state.BoardValues.[index])
        System.String.Format("{0:00}{1:00}{2:00}", indicies.[0], indicies.[1], indicies.[2])

    // TODO (std_string) : add solution via Markov chain
    [<TestCase(4, "101524", TimeThresholds.HardTimeLimit)>]
    [<TestCase(6, "102400", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(diceSideCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, diceSideCount)
