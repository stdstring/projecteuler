namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System
open System.Text
open CommonLib

// Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine (see https://projecteuler.net/problem=68).
// Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely.
// For example, the one of the solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.
// It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
// Total   Solution Set
// 9       4,2,3; 5,3,1; 6,1,2
// 9       4,3,2; 6,2,1; 5,1,3
// 10      2,3,5; 4,5,1; 6,1,3
// 10      2,5,3; 6,3,1; 4,1,5
// 11      1,4,6; 3,6,2; 5,2,4
// 11      1,6,4; 5,4,2; 3,2,6
// 12      1,5,6; 2,6,4; 3,4,5
// 12      1,6,5; 3,5,4; 2,4,6
// By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.
// Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings.
// What is the maximum 16-digit string for a "magic" 5-gon ring (see https://projecteuler.net/problem=68)?

[<TestFixture>]
type Problem068() =

    let chooseString (stringSize: int) (alphabet: int list) (permutation: int list) =
        let alphabetRest = alphabet |> List.filter (fun item -> permutation |> List.contains item |> not)
        let itemPairs = List.zip permutation ((permutation |> List.tail) @ [permutation.Head])
        let maxSumPair = itemPairs |> List.maxBy (fun (item1, item2) -> item1 + item2)
        let tripletSum = (alphabetRest |> List.head) + (maxSumPair |> fst) + (maxSumPair |> snd)
        let rec createTriplets (pairs: (int * int) list) (alphabetRest: int list) (result: StringBuilder) =
            match pairs with
            | [] when result.Length = stringSize -> result.ToString() |> Some
            | [] when result.Length <> stringSize -> None
            | (item1, item2) :: pairsRest ->
                let externalNode = tripletSum - item1 - item2
                match alphabetRest |> List.contains externalNode with
                | false -> None
                | true ->
                    result.Append(externalNode).Append(item1).Append(item2) |> createTriplets pairsRest (alphabetRest |> List.filter (fun item -> item <> externalNode))
            | _ -> failwith "Unexpected branch of match expression"
        new StringBuilder() |> createTriplets (ListUtils.ShiftToItem(maxSumPair, itemPairs)) alphabetRest

    let solveImpl (stringSize: int) (ringSize: int) (numbersCount: int) =
        let alphabet = [1 .. numbersCount]
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, ringSize)
        seq {1I .. lexicographicalNumberSup - 1I} |>
        Seq.map (fun lexicographicalNumber -> Permutations.GetPermutation(lexicographicalNumber, ringSize, alphabet)) |>
        Seq.choose (fun permutation -> permutation |> chooseString stringSize alphabet) |>
        Seq.max


    [<TestCase(9, "432621513", TimeThresholds.HardTimeLimit)>]
    member public this.SolveFor3Ring(stringSize: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, stringSize, 3, 6)

    [<TestCase(16, "6531031914842725", TimeThresholds.HardTimeLimit)>]
    member public this.SolveFor5Ring(stringSize: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, stringSize, 5, 10)

