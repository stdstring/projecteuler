namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic
open System.IO

// Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:
// 1) S(B) != S(C); that is, sums of subsets cannot be equal.
// 2) If B contains more elements than C then S(B) > S(C).
// For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because 65 + 87 + 88 = 75 + 81 + 84,
// whereas {157, 150, 164, 119, 79, 159, 161, 139, 158} satisfies both rules for all possible subset pair combinations and S(A) = 1286.
// Using data (situated in "problem_105.dat") from text file with one-hundred sets containing seven to twelve elements (the two examples given above are the first two sets in the file),
// identify all the special sum sets, A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).

[<TestFixture>]
type Problem105() =

    let rec checkRule2Impl (numbers: int[]) (count: int) =
        match count with
        | _ when count >= numbers.Length -> true
        | _ ->
            let minCurrentPortionSum = numbers |> Seq.take count |> Seq.sum
            let maxPrevPortionSum = numbers |> Seq.rev |> Seq.take (count - 1) |> Seq.sum
            match maxPrevPortionSum < minCurrentPortionSum with
            | false -> false
            | true -> (count + 1) |> checkRule2Impl numbers

    let checkRule2 (numbers: int[]) =
        checkRule2Impl numbers 2

    let rec checkRule1Impl (numbers: int[]) (currentValue: int) (size: int) (index: int) (subsets: ISet<int>[]) =
        match index with
        | _ when index = numbers.Length -> true
        | _ ->
            let nextValue = currentValue + numbers.[index]
            let nextSize = size + 1
            match nextValue |> subsets.[nextSize].Add with
            | false -> false
            | true ->
                match checkRule1Impl numbers nextValue nextSize (index + 1) subsets with
                | false -> false
                | true -> checkRule1Impl numbers currentValue size (index + 1) subsets

    let checkRule1 (numbers: int[]) =
        let subsets = numbers.Length + 1 |> Array.zeroCreate
        for index in 1 .. numbers.Length do
            subsets.[index] <- (new HashSet<int>() :> ISet<int>)
        checkRule1Impl numbers 0 0 0 subsets

    let checkRules (numbers: int[]) = (numbers |> checkRule1) && (numbers |> checkRule2)

    let createSet (line: string) = line.Split(',') |> Array.map(fun value -> value |> int) |> Array.sort

    let solveImpl (dataFilename: string) =
        let lines = File.ReadAllLines(Path.Combine("Data", dataFilename))
        lines |> Seq.map (fun line -> line |> createSet) |> Seq.filter (fun numbers -> numbers |> checkRules) |> Seq.map (fun numbers -> numbers |> Seq.sum) |> Seq.sum

    let solveMultitasksImpl (dataFilename: string) =
        let lines = File.ReadAllLines(Path.Combine("Data", dataFilename))
        lines |> Seq.map (fun line -> async { let numbers = line |> createSet in return if numbers |> checkRules then numbers |> Seq.sum else 0 }) |> Async.Parallel |> Async.RunSynchronously |> Seq.sum

    [<TestCase("problem_105.dat", 73702, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)

    [<TestCase("problem_105.dat", 73702, TimeThresholds.HardTimeLimit)>]
    member public this.SolveWithMultitasks(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveMultitasksImpl, dataFilename)