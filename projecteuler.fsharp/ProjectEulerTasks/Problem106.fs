namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// Let S(A) represent the sum of elements in set A of size n.
// We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:
// 1) S(B) != S(C); that is, sums of subsets cannot be equal.
// 2) If B contains more elements than C then S(B) > S(C).
// For this problem we shall assume that a given set contains n strictly increasing elements and it already satisfies the second rule.
// Surprisingly, out of the 25 possible subset pairs that can be obtained from a set for which n = 4, only 1 of these pairs need to be tested for equality (first rule).
// Similarly, when n = 7, only 70 out of the 966 subset pairs need to be tested.
// For n = 12, how many of the 261625 subset pairs that can be obtained need to be tested for equality?

[<TestFixture>]
type Problem106() =

    // TODO (std_string) : think about move into CommonLib
    let generateCatalanNumbers (n: int) =
        let calcCatalanNumber (current: int) (catalanNumbers: int[]) =
            seq {0 .. current - 1} |> Seq.map (fun index -> catalanNumbers.[index] * catalanNumbers.[current - 1 - index]) |> Seq.sum
        let catalanNumbers = (n + 1) |> Array.zeroCreate
        catalanNumbers.[0] <- 1
        seq {1 .. n} |> Seq.iter (fun current -> let catalanNumber = catalanNumbers |> calcCatalanNumber current in catalanNumbers.[current] <- catalanNumber)
        catalanNumbers

    let solveImpl (setSize: int) =
        // Description:
        // Let a1 < a2 < ... < an - set (which already satisfies the second rule)
        // It's obviously, that we must investigate pairs of subsets with equal size, which > 1, i.e., 2-2, 3-3, ... pairs
        // Let a11 < ... < a1k - numbers of first subset, a21 < ... < a2k - numbers of second subset, which are equal size
        // Easy it can be see the following:
        // If we write on numeric axis open brackets instead of numbers from first subset and closed brackets instead of numbers from second subset
        // (in the positions of numbers), then our task wil be equivalent to task about correctness result bracket sequence
        // If we have the correct bracket sequence, then we don't need to check first rule for pairs of source subsets
        // If we have the incorrect bracket sequence, then we have to check first rule for pairs of source subsets
        // Let C(n) - Catalan number (https://en.wikipedia.org/wiki/Catalan_number)
        // Catalan number is number of correct bracket sequence consisting of n opening and n closing brackets.
        // Let C(n, k) - binomial coefficient
        // Let n - total set size, m - subset size, then:
        // Total count of bracket sequences consisting of m opening and m closing brackets,
        // which are selected from set with n element will be C(n, m) * C(n - m, m)
        // Total count of correct bracket sequences consisting of m opening and m closing brackets,
        // which are selected from set with n element will be C(m) * C(n, 2 * m)
        // Total count of incorrect bracket sequences consisting of m opening and m closing brackets,
        // which are selected from set with n element will be C(n, m) * C(n - m, m) - C(m) * C(n, 2 * m)
        // And total count of pairs of subsets from m numbers need to be tested will be C(n, m) * C(n - m, m) - C(m) * C(n, 2 * m)
        // PS. 25 possible subset pairs that can be obtained from a set for which n = 4
        //     Let a1, a2, a3, a4 - set
        //     1-1 pairs: a1-a2, a1-a3, a1-a4, a2-a3, a2-a4, a3-a4 (6 items)
        //     1-2 pairs: a1-a2a3, a1-a2a4, a1-a3a4, a2-a1a3, a2-a1a4, a2-a3a4, a3-a1a2, a3-a1a4, a3-a2a4, a4-a1a2, a4-a1a3, a4-a2a3 (12 items)
        //     1-3 pairs: a1-a2a3a4, a2-a1a3a4, a3-a1a2a4, a4-a1a2a3 (4 items)
        //     2-2 pairs: a1a2-a3a4, a1a3-a2a4, a1a4-a2a3 (3 items)
        //     total items = 6 + 12 + 4 + 3 = 25 items
        let maxPairSize = setSize / 2
        let catalanNumbers = maxPairSize |> generateCatalanNumbers
        let totalPairsCount = seq {2 .. maxPairSize}
                              |> Seq.map(fun pairSize -> Numbers.CalcBinomialCoeff(setSize, pairSize) * Numbers.CalcBinomialCoeff(setSize - pairSize, pairSize) / 2I |> int)
                              |> Seq.sum
        let correctBracketSeqCount = seq {2 .. maxPairSize}
                                     |> Seq.map (fun pairSize -> (Numbers.CalcBinomialCoeff(setSize, 2 * pairSize) |> int) * catalanNumbers.[pairSize])
                                     |> Seq.sum
        totalPairsCount - correctBracketSeqCount

    [<TestCase(4, 1, TimeThresholds.HardTimeLimit)>]
    [<TestCase(7, 70, TimeThresholds.HardTimeLimit)>]
    [<TestCase(12, 21384, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(size: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, size)