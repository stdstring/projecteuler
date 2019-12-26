namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime.
// For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
// Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

module Problem060Impl =

    type SolutionImpl(size: int, sieve: EratosSieve) =

        [<Literal>]
        let SearchMax = 10000

        let calcFactor (number: int) =
            let rec calcFactorImpl (factor: int) =
                match factor with
                | _ when factor > number -> factor
                | _ -> 10 * factor |> calcFactorImpl
            10 |> calcFactorImpl

        let checkPrimePair (prime1: int) (prime2: int) =
            let prime1Factor = prime1 |> calcFactor
            let prime2Factor = prime2 |> calcFactor
            (prime1 * prime2Factor + prime2 |> sieve.IsPrime) && (prime2 * prime1Factor + prime1 |> sieve.IsPrime)

        let checkPrime (primes: int list) (prime: int) =
            primes |> List.exists (fun number -> number |> checkPrimePair prime |> not) |> not

        let mergeResults (existingResult: int option) (newResult: int) =
            match existingResult with
            | Some value when value < newResult -> existingResult
            | _ -> newResult |> Some

        let rec findNextPrime (selectedPrimes: int list) (primes: int list) =
            match primes with
            | [] -> []
            | prime :: _ when prime |> checkPrime selectedPrimes -> primes
            | _ :: primesRest -> primesRest |> findNextPrime selectedPrimes

        let rec processPrime (primes: int list) (selectedPrimes: int list) (result: int option) =
            match primes, selectedPrimes with
            | _ when selectedPrimes.Length = size -> selectedPrimes |> List.sum |> mergeResults result
            | [], _ -> result
            | _ ->
                match primes |> findNextPrime selectedPrimes with
                | [] -> result
                | nextPrime :: primesRest ->
                    let newResult = result |> processPrime primesRest (nextPrime :: selectedPrimes)
                    match selectedPrimes with
                    | _ when selectedPrimes.Length = size - 1 -> newResult
                    | _ -> newResult |> processPrime primesRest selectedPrimes

        member public this.Solve() =
            let primes = sieve.ToSeq() |> Seq.skip 1 |> Seq.takeWhile (fun prime -> prime <= SearchMax) |> Seq.toList
            (None |> processPrime primes []).Value

open Problem060Impl

[<TestFixture>]
type Problem060() =

    [<Literal>]
    let SieveMax = 100000000

    let solveImpl (size: int) =
        let sieve = EratosSieve.Create(SieveMax)
        let solver = new SolutionImpl(size, sieve)
        solver.Solve()

    [<TestCase(4, 792, TimeThresholds.HardTimeLimit)>]
    [<TestCase(5, 26033, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(size: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, size)
