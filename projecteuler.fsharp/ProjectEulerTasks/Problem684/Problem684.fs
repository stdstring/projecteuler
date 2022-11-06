namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem684Impl =

    type PowerData = {Power: bigint; Value: bigint}

open Problem684Impl

[<TestFixture>]
type Problem684() =

    let calcTenPow (power: bigint) (modValue: bigint) (powerData: ResizeArray<PowerData>) =
        let rec calcImpl (index: int) (powerRest: bigint) (result: bigint) =
            match powerRest with
            | _ when powerRest = 0I -> result
            | _ when powerRest = 1I -> (10I * result) % modValue
            | _ ->
                match index with
                | _ when powerData.[index].Power > powerRest -> result |> calcImpl (index - 1) powerRest
                | _ when powerData.[index].Power = powerRest -> (result * powerData.[index].Value) % modValue
                | _ -> (result * powerData.[index].Value) % modValue |> calcImpl index (powerRest - powerData.[index].Power)
        while (2I * powerData.[powerData.Count - 1].Power) < power do
            let lastData = powerData.[powerData.Count - 1]
            {PowerData.Power = 2I * lastData.Power; PowerData.Value = (lastData.Value * lastData.Value) % modValue} |> powerData.Add
        calcImpl (powerData.Count - 1) power 1I

    let calcSum (k: bigint) (r: bigint) (modValue: bigint) (tenPower: bigint) =
        match (((r * r + 3I * r + 12I) / 2I) * tenPower - (9I * k + 6I + r)) % modValue with
        | value when value < 0I -> value + modValue
        | value -> value

    let solveImpl (fibonacciTo: int) (modValue: int64) =
        // Description:
        // Anyone can see, that s(1) = 1, s(2) = 2, .. , s(9) = 9, s(10) = 19, s(11) = 29, ..., s(18) = 99, s(19) = 199, s(20) = 299, ... s(27) = 999, s(28) = 1999, ...
        // let n = 1 + 9 * k + r (0 <= r < 9) = 9 * k + r (1 <= r <= 9)
        // s9(k) = s(9 * k + 1) + .. + (9 * k + 9) = (1 + 2 + ... + 9) * 10^k + 9 * (10^k - 1) = 54 * 10^k + 9
        // s9(0) + s9(1) + ... + s9(k) = 54 * (10^0 + 10^1 + ... + 10^k) - 9 * (k + 1) = 6 * 10^(k + 1) - 9 * k - 15
        // 19...9 + r9...9 = [here 9...9 are k + 1 nines] = (1 + .. + r) * 10^(k + 1) + r * (10^(k + 1) - 1) = ((r^2 + 3 * r) / 2) * 10^(k + 1) - r
        // S(9 * k + r) = s9(0) + s9(1) + ... s9(k - 1) + 19...9 + r9...9 = [here 9...9 are k nines] = 6 * 10^k - 9 * (k - 1) - 15 + ((r^2 + 3 * r) / 2) * 10^k - r = ((r^2 + 3 * r + 12) / 2) * 10^k - 9 * k - 6 - r
        let modValue = modValue |> bigint
        let powerData = new ResizeArray<PowerData>([{PowerData.Power = 0I; PowerData.Value = 1I}; {PowerData.Power = 1I; PowerData.Value = 10I}; {PowerData.Power = 2I; PowerData.Value = 100I}])
        let mutable previous = 0I
        let mutable current = 1I
        let mutable sum = 0I
        for _ in 2 .. fibonacciTo do
            let next = current + previous
            let k = next / 9I
            let r = next % 9I
            let tenPower = powerData |> calcTenPow k modValue
            let value = tenPower |> calcSum k r modValue
            sum <- (sum + value) % modValue
            previous <- current
            current <- next
        (sum % modValue) |> int64

    [<TestCase(90, 1000000007L, 922058210L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(fibonacciTo: int, modValue: int64, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, fibonacciTo, modValue)
