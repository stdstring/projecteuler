namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem055() =

    [<Literal>]
    let MinNumber = 1

    [<Literal>]
    let MaxIteration = 50

    let processNumber (number: int) =
        let rec processChain (iteration: int) (current: bigint) (reversedCurrent: bigint) =
            match iteration with
            | _ when iteration > MaxIteration -> false
            | _ ->
                let next = current + reversedCurrent
                let digits = next |> NumbersDigits.GetDigits
                let reversedDigits = digits |> List.rev
                let reversedNext = reversedDigits |> NumbersDigits.GetNumber
                match digits = reversedDigits with
                | true -> true
                | false -> processChain (iteration + 1) next reversedNext
        processChain 0 (number |> bigint) (number |> NumbersDigits.ReverseNumber |> bigint)


    let solveImpl (maxNumber: int) =
        seq {MinNumber .. maxNumber} |> Seq.filter (fun number -> number |> processNumber |> not) |> Seq.length

    [<TestCase(10000, 249, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)