namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The number 3797 has an interesting property.
// Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7.
// Similarly we can work from right to left: 3797, 379, 37, and 3.
// Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
// NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

[<TestFixture>]
type Problem037() =

    let expectedCount = 11

    let generateNextNumber (digits: List<int>) =
        // for suitable number d0d1...dN :
        // d0 in [2, 3, 5, 7] due to source number is reduced to d0
        // d1... in [1, 3, 7, 9] due to source number is reduced to prime number
        // dN in [3, 7] due to source number is prime and is reduced to dN
        let rec generateNextNumber (source: List<int>) (dest: List<int>) =
            match source with
            // dN
            | 3 :: digits when dest = [] -> 7 :: digits
            | 7 :: digits when dest = [] -> generateNextNumber digits [3]
            // d0
            | [2] -> dest @ [3]
            | [3] -> dest @ [5]
            | [5] -> dest @ [7]
            | [7] -> dest @ [1; 2]
            // d1 ...
            | 1 :: digits -> dest @ [3] @ digits
            | 3 :: digits -> dest @ [7] @ digits
            | 7 :: digits -> dest @ [9] @ digits
            | 9 :: digits -> generateNextNumber digits (dest @ [1])
            // all other
            | _ -> failwith "Unexpected branch of match expression"
        [] |> generateNextNumber (digits |> List.rev) |> List.rev

    let checkNumberLeft (source: int) (digitsCount: int) =
        seq {1 .. digitsCount - 1} |> Seq.map (fun power -> source / (pown 10 power)) |> Seq.exists (fun number -> number |> NumbersDividers.IsPrime |> not) |> not

    let checkNumberRight (source: int) (digitsCount: int) =
        seq {digitsCount - 1 .. -1 .. 1} |> Seq.map (fun power -> source % (pown 10 power)) |> Seq.exists (fun number -> number |> NumbersDividers.IsPrime |> not) |> not

    let checkNumber (digits: List<int>) =
        let number = digits |> NumbersDigits.GetNumber |> int
        NumbersDividers.IsPrime(number) && (checkNumberLeft number digits.Length) && (checkNumberRight number digits.Length)

    let solveImpl () =
        [2; 3] |> Seq.unfold (fun digits -> Some (digits, digits |> generateNextNumber)) |>
                  Seq.filter (fun digits -> digits |> checkNumber) |>
                  Seq.take expectedCount |>
                  Seq.map (fun digits -> digits |> NumbersDigits.GetNumber |> int) |>
                  Seq.sum

    [<TestCase(748317, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)
