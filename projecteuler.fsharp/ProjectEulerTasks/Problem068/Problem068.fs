namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Text
open CommonLib

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
        Permutations.GeneratePermutations(ringSize, alphabet) |>
        Seq.choose (fun permutation -> permutation |> chooseString stringSize alphabet) |>
        Seq.max


    [<TestCase(9, "432621513", TimeThresholds.HardTimeLimit)>]
    member public this.SolveFor3Ring(stringSize: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, stringSize, 3, 6)

    [<TestCase(16, "6531031914842725", TimeThresholds.HardTimeLimit)>]
    member public this.SolveFor5Ring(stringSize: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, stringSize, 5, 10)

