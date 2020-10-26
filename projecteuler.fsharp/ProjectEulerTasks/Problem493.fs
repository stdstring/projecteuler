namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// 70 coloured balls are placed in an urn, 10 for each of the seven rainbow colours.
// What is the expected number of distinct colours in 20 randomly picked balls?
// Give your answer with nine digits after the decimal point (a.bcdefghij).

[<TestFixture>]
type Problem493() =

    let solveImpl (ballCount: int) (colourCount: int) (pickCount: int) =
        // Description:
        // For our task, probability of the presence of any selected colour is p = 1 - C(60, 20) / C(70, 20), where C(n, k) - binomial coefficient.
        // The expected value of the number of colours present is just the sum of the expected values of the number of occurrences of the individual colours,
        // which are all equal to p (0 with probability 1 - p, 1 with probability p), so the total expectation value is 7 * p.
        let ballsPerColour = ballCount / colourCount
        let totalCases = Numbers.CalcBinomialCoeff(ballCount, pickCount)
        let casesWithoutSelectedColour = Numbers.CalcBinomialCoeff(ballCount - ballsPerColour, pickCount)
        let colourProbability = 1.0 - (casesWithoutSelectedColour |> float) / (totalCases |> float)
        let expectedValue = (colourCount |> float) * colourProbability
        System.String.Format(System.Globalization.CultureInfo.InvariantCulture, "{0:0.000000000}", expectedValue)

    [<TestCase(8, 4, 3, "2.571428571", TimeThresholds.HardTimeLimit)>]
    [<TestCase(16, 4, 5, "3.274725275", TimeThresholds.HardTimeLimit)>]
    [<TestCase(70, 7, 20, "6.818741802", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(ballsCount: int, coloursCount: int, picksCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, ballsCount, coloursCount, picksCount)