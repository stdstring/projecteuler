namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem127() =

    let solveImpl (cTopBorder: int) =
        // GCD(a, b) = GCD(a, c) = GCD(b, c) = 1 => rad(abc) = rad(a) * rad(b) * rad(c)
        // a + b = c => if GCD(a, c) == 1 then GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
        // GCD(a, b) = 1 if GCD(rad(a), rad(b)) = 1 and vice versa
        // rad(c) <= c / 2, rad(b) <= b / 2 (2 * b > c)
        // working solution
        let cMaxValue = cTopBorder - 1
        let sieve = cMaxValue |> EratosSieveWithSmallestPrimeFactors.Create
        let radValues = cTopBorder |> Array.zeroCreate
        radValues[1] <- 1
        for number in seq{2 .. cMaxValue} do
            let smallestPrimeFactor = sieve.[number]
            let prevRadValue = radValues.[number / smallestPrimeFactor]
            radValues.[number] <- prevRadValue * (if NumbersRelation.CalcGCD(prevRadValue, smallestPrimeFactor) = 1 then smallestPrimeFactor else 1)
        let possibleCValues = seq {1 .. cMaxValue} |> Seq.filter (fun number -> 2 * radValues.[number] < number) |> Seq.toArray
        let mutable result = 0
        for bValueIndex in seq {0 .. possibleCValues.Length - 1} do
            let bValue = possibleCValues[bValueIndex]
            let radBValue = radValues[bValue]
            let mutable cValueIndex = bValueIndex + 1
            while (cValueIndex < possibleCValues.Length) && (2 * bValue > possibleCValues[cValueIndex]) do
                let cValue = possibleCValues[cValueIndex]
                let radCValue = radValues[cValue]
                let aValue = cValue - bValue
                let radAValue = radValues[aValue]
                if radCValue <= (cMaxValue / radAValue) then
                    let radACValue = radAValue * radCValue
                    if radBValue <= (cMaxValue / radACValue) then
                        let totalRadValue = radACValue * radBValue
                        if (radACValue < cMaxValue) && (radValues.[radACValue] = radACValue) && (totalRadValue < cValue) then
                            result <- result + cValue
                cValueIndex <- cValueIndex + 1
        result

    [<TestCase(1000, 12523, TimeThresholds.HardTimeLimit)>]
    [<TestCase(120000, 18407904, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(cTopBorder: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, cTopBorder)