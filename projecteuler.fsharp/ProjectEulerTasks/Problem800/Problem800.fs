namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem800() =

    // TODO (std_string) : think about moving into common libs
    let rec findRangeRight (f: int -> double) (value: double) (right: int) =
        match (right |> f) < value with
        | true -> 2 * right |> findRangeRight f value
        | false -> right

    // TODO (std_string) : think about moving into common libs
    let rec solveInequalityImpl (f: int -> double) (value: double) (left: int) (right: int) =
        match right - left with
        | 0 -> left
        | 1 -> left
        | _ ->
            let middle = (left + right) / 2
            match (middle |> f) with
            | middleValue when middleValue = value -> middle
            | middleValue when middleValue < value -> solveInequalityImpl f value middle right
            | middleValue when middleValue > value -> solveInequalityImpl f value left middle
            | _ -> failwith "Unexpected control branch"

    // TODO (std_string) : think about moving into common libs
    let solveInequality (f: int -> double) (value: double) (left: int) =
        left + 1 |> findRangeRight f value |> solveInequalityImpl f value left

    let logValue (p: int) (q: int) =
        (q |> double) * (p |> double |> System.Math.Log2) + (p |> double) * (q |> double |> System.Math.Log2)

    let rec calcHybridIntegers (borderValue: double) (sieve: int[]) (leftIndex: int) (rightIndex: int) (hybridIntegersCount: int) =
        let rec rewindRightIndexBack (index: int) =
            match logValue sieve.[leftIndex] sieve.[index] > borderValue with
            | true -> index - 1 |> rewindRightIndexBack
            | false -> index
        match leftIndex with
        | _ when leftIndex >= rightIndex -> hybridIntegersCount
        | _ when logValue sieve.[leftIndex] sieve.[leftIndex + 1] > borderValue -> hybridIntegersCount
        | _ ->
            let correctRightIndex = rightIndex |> rewindRightIndexBack
            hybridIntegersCount + (correctRightIndex - leftIndex) |> calcHybridIntegers borderValue sieve (leftIndex + 1) correctRightIndex

    let solveImpl (baseValue: int) (exponentValue: int) =
        let borderValue = (exponentValue |> double) * (baseValue |> System.Math.Log2)
        let maxNumber =  solveInequality (fun x -> logValue 2 x) borderValue 2
        let sieve = (maxNumber |> EratosSieve.Create).ToSeq() |> Seq.toArray
        calcHybridIntegers borderValue sieve 0 (sieve.Length - 1) 0

    [<TestCase(800, 1, 2, TimeThresholds.HardTimeLimit)>]
    [<TestCase(800, 800, 10790, TimeThresholds.HardTimeLimit)>]
    [<TestCase(800800, 800800, 1412403576, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(baseValue: int, exponentValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, baseValue, exponentValue)