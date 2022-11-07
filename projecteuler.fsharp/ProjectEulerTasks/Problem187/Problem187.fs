namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem187() =

    let solveImpl (maxNumber: int) =
        let primesSup  = maxNumber / 2
        let primes = EratosSieve.Create(primesSup).ToSeq() |> Seq.toArray
        let rec countNumbers (index1: int) (index2: int) (count: int) =
            match index1 with
            | _ when index1 = primes.Length -> count
            | _ when index2 = primes.Length -> countNumbers (index1 + 1) (index1 + 1) count
            | _ when primes.[index1] * primes.[index1] > maxNumber -> count
            | _ when primes.[index1] * primes.[index2] > maxNumber -> countNumbers (index1 + 1) (index1 + 1) count
            | _ -> countNumbers index1 (index2 + 1) (count + 1)
        countNumbers 0 0 0

    [<TestCase(29, 10, TimeThresholds.HardTimeLimit)>]
    [<TestCase(99999999, 17427258, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)