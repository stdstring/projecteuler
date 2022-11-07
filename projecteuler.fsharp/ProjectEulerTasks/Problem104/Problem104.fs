namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem104Impl =
    type FibonacciData = {Prev: bigint; Current: bigint; TermNumber: int; TopDivisor: bigint}

open Problem104Impl

[<TestFixture>]
type Problem104() =

    [<Literal>]
    let LastKnownTermNumber = 2749
    [<Literal>]
    let MinPandigital = 123456789
    [<Literal>]
    let MaxPandigital = 987654321

    let alphabet = [1; 2; 3; 4; 5; 6; 7; 8; 9]

    let border = 1000000000I

    let getPermutationAsNumber (lexicographicalNumber: bigint) =
        Permutations.GetPermutation(lexicographicalNumber, alphabet) |> Seq.fold (fun number digit -> number * 10 + digit) 0

    let createPandigitalStorage () =
        let pandigitalStorage = Array.create (MaxPandigital - MinPandigital + 1) false
        let alphabet = [1; 2; 3; 4; 5; 6; 7; 8; 9]
        let lexicographicalNumberSup = alphabet |> Permutations.GetLexicographicalNumberSup
        seq {0I .. lexicographicalNumberSup - 1I} |> Seq.map getPermutationAsNumber |> Seq.iter (fun number -> pandigitalStorage.[number - MinPandigital] <- true)
        pandigitalStorage

    let isPandigital (pandigitalStorage: bool[]) (number: int) =
        match number with
        | value when value < MinPandigital -> false
        | value when value > MaxPandigital -> false
        | value -> pandigitalStorage.[value - MinPandigital]

    let calcQuotientAndTopDivisor (nextFibonacci: bigint) (topDivisor: bigint) =
        let quotient = nextFibonacci / topDivisor
        match quotient with
        | value when value > border -> quotient / 10I, topDivisor * 10I
        | _ -> quotient, topDivisor

    let rec processKnownFibonacciNumbers (data: FibonacciData) =
        match data.TermNumber with
        | LastKnownTermNumber -> data
        | _ ->
            let nextFibonacci = data.Current + data.Prev
            let _, topDivisor = data.TopDivisor |> calcQuotientAndTopDivisor nextFibonacci
            {FibonacciData.Prev = data.Current; FibonacciData.Current = nextFibonacci; FibonacciData.TermNumber = data.TermNumber + 1; FibonacciData.TopDivisor = topDivisor} |> processKnownFibonacciNumbers

    let rec findSuitableFibonacciNumber (pandigitalStorage: bool[]) (data: FibonacciData) =
        let nextFibonacci = data.Current + data.Prev
        let quotient, topDivisor = data.TopDivisor |> calcQuotientAndTopDivisor nextFibonacci
        let nextData = {FibonacciData.Prev = data.Current; FibonacciData.Current = nextFibonacci; FibonacciData.TermNumber = data.TermNumber + 1; FibonacciData.TopDivisor = topDivisor}
        match quotient |> int |> isPandigital pandigitalStorage with
        | false -> nextData |> findSuitableFibonacciNumber pandigitalStorage
        | true ->
            match nextFibonacci % border |> int |> isPandigital pandigitalStorage with
            | false -> nextData |> findSuitableFibonacciNumber pandigitalStorage
            | true -> nextData

    let solveImpl () =
        let pandigitalStorage = createPandigitalStorage ()
        let foundData = {FibonacciData.Prev = 0I; FibonacciData.Current = 1I; FibonacciData.TermNumber = 1; FibonacciData.TopDivisor = 1I} |> processKnownFibonacciNumbers |> findSuitableFibonacciNumber pandigitalStorage
        foundData.TermNumber

    [<TestCase(329468, TimeThresholds.SoftTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)