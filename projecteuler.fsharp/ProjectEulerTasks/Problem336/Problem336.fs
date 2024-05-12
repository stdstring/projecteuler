namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem336() =

    let generateNextArrangement (nextItem: int) (nextItemIndex: int) (currentArrangement: int[]) =
        let parts = [|currentArrangement[nextItemIndex..] |> Array.rev; [|nextItem|]; currentArrangement[..nextItemIndex - 1]|]
        parts |> Array.concat

    let generateNextArrangements (nextItem: int) (currentArrangement: int[]) =
        let currentArrangementSize = currentArrangement.Length
        seq{1..currentArrangementSize - 1} |> Seq.map (fun nextItemIndex -> currentArrangement |> generateNextArrangement nextItem nextItemIndex)
                                           |> Seq.toArray

    let generateNextArrangementsList (nextItem: int) (currentArrangements: int[][]) =
        currentArrangements |> Array.map (fun currentArrangement -> currentArrangement |> generateNextArrangements nextItem)
                            |> Array.concat

    let convertToString (startCombination: string) (currentArrangement: int[]) =
        currentArrangement |> Seq.map (fun index -> startCombination.[index]) |> System.String.Concat

    let solveImpl (startCombination: string) (lexicographicNumber: int) =
        // Description:
        // To get a worst case scenario with N carriages, we must ensure that after the first carriage is got into the right place,
        // the remaining carriages must be in one of the worst possible configurations for N-1.
        // So, for each possible length I keep a list of worst arrangements
        // and use this to generate the list the worst arrangements for one more carriage as follows.
        // for example:
        // xyzAqpr = xyz | Apqr -> xyz | rqpA = xyzrqpA -> Apqrzyx
        // So for every worst case solution of the form pqrzyx for 6, we generate a new worst case solution of xyzAqpr for 7.
        // And of course for a length N solution we have N-1 possible insertion points for our new front carriage.
        let initArrangementsData = [|[|startCombination.Length - 1; startCombination.Length - 2|]|]
        let foldFun = fun (currentArrangements: int[][]) (nextItem: int) -> currentArrangements |> generateNextArrangementsList nextItem
        let resultArrangementsData = {startCombination.Length - 3 .. -1 .. 0} |> Seq.fold foldFun initArrangementsData
        let resultArrangements = resultArrangementsData |> Seq.map (fun currentArrangement -> currentArrangement |> convertToString startCombination)
                                                        |> Seq.sort
                                                        |> Seq.toArray
        resultArrangements.[lexicographicNumber - 1]

    [<TestCase("ABCD", 2, "DBAC", TimeThresholds.HardTimeLimit)>]
    [<TestCase("ABCDEF", 10, "DFAECB", TimeThresholds.HardTimeLimit)>]
    [<TestCase("ABCDEFGHIJK", 2011, "CAGBIHEFJDK", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(startCombination: string, lexicographicNumber: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, startCombination, lexicographicNumber)