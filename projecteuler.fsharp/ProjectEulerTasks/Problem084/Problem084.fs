namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System
open System.Collections.Generic

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
