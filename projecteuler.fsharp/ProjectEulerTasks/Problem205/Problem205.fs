namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Globalization

[<TestFixture>]
type Problem205() =

    let firstDice = [|1; 2; 3; 4|]

    let secondDice = [|1; 2; 3; 4; 5; 6|]

    let generateRollState (dice: int[]) (diceCount: int) (stateIndex: int64) =
        let diceSize = dice.Length |> int64
        let numbers, _ = seq { 1 .. diceCount } |> Seq.fold (fun (numbers, stateIndex) _ -> let numberIndex = stateIndex % diceSize |> int in dice.[numberIndex] :: numbers, stateIndex / diceSize) ([], stateIndex)
        numbers |> Seq.sum

    let generateRollAllStates (dice: int[]) (diceCount: int) =
        let stateCount = pown (dice.Length |> int64) diceCount
        seq { 0L .. stateCount - 1L } |> Seq.map (fun stateIndex -> stateIndex |> generateRollState dice diceCount) |> Seq.toList

    let prepareRollData (dice: int[]) (diceCount: int) (states: int list) =
        let minValue = diceCount * dice.[0]
        let maxValue = diceCount * dice.[dice.Length - 1]
        let statesArray = Array.create (maxValue - minValue + 1) 0
        states |> Seq.iter (fun value -> let index = value - minValue in statesArray.[index]<-statesArray.[index] + 1)
        seq { 1 .. statesArray.Length - 1 } |> Seq.fold (fun (storage: int[]) (index: int) -> storage.[index]<-storage.[index] + storage.[index - 1]; storage) statesArray

    let calculateFirstWinCount (firstDiceCount: int) (secondDiceCount: int) =
        let secondStates = generateRollAllStates secondDice secondDiceCount
        let secondData = prepareRollData secondDice secondDiceCount secondStates
        let firstStateCount = pown (firstDice.Length |> int64) firstDiceCount
        let minValue = secondDiceCount * secondDice.[0]
        seq { 0L .. firstStateCount - 1L } |>
        Seq.map (fun stateIndex ->stateIndex |> generateRollState firstDice firstDiceCount) |>
        Seq.filter (fun value -> value > minValue) |>
        Seq.map(fun value -> secondData.[value - minValue - 1] |> int64) |>
        Seq.sum

    let solveImpl (firstDiceCount: int) (secondDiceCount: int) =
        // Description:
        // Let we have a set of N identical dice (with K faces), then State - the result of some roll of all these dices.
        // Let L - the set of all possible states for a set of N identical dice.
        // For each element (state) of L, we calculate the sum of the values of the roll of all dice and
        // sort the states in the set L according to the value of the sum in ascending order - get an ordered set L'.
        // It is easy to show that the values of sum for all states are in the range [N, N * K] without any holes.
        // Then, for any value, we can easily find out how many states have a sum less than this value (using an ordered set of L').
        let result = calculateFirstWinCount firstDiceCount secondDiceCount
        let totalCount = (pown (firstDice.Length |> int64) firstDiceCount) * (pown (secondDice.Length |> int64) secondDiceCount)
        System.String.Format(CultureInfo.InvariantCulture, "{0:0.0000000}", (result |> float) / (totalCount |> float))

    [<TestCase(1, 1, "0.2500000", TimeThresholds.HardTimeLimit)>]
    [<TestCase(3, 2, "0.5000000", TimeThresholds.HardTimeLimit)>]
    [<TestCase(9, 6, "0.5731441", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(firstDiceCount: int, secondDiceCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, firstDiceCount, secondDiceCount)