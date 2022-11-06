namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem315() =

    [<Literal>]
    let PreparedDataSize = 100

    // digit representation in array: top horizontal, top-left vertical, top-right vertical, middle horizontal, bottom-left vertical, bottom-right vertical, bottom horizontal
    let zeroRepresentation = [|true; true; true; false; true; true; true|]
    let oneRepresentation = [|false; false; true; false; false; true; false|]
    let twoRepresentation = [|true; false; true; true; true; false; true|]
    let threeRepresentation = [|true; false; true; true; false; true; true|]
    let fourRepresentation = [|false; true; true; true; false; true; false|]
    let fiveRepresentation = [|true; true; false; true; false; true; true|]
    let sixRepresentation = [|true; true; false; true; true; true; true|]
    let sevenRepresentation = [|true; true; true; false; false; true; false|]
    let eightRepresentation = [|true; true; true; true; true; true; true|]
    let nineRepresentation = [|true; true; true; true; false; true; true|]

    let digits = [|zeroRepresentation;
                   oneRepresentation;
                   twoRepresentation;
                   threeRepresentation;
                   fourRepresentation;
                   fiveRepresentation;
                   sixRepresentation;
                   sevenRepresentation;
                   eightRepresentation;
                   nineRepresentation|]

    let digitsSizes = [|zeroRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        oneRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        twoRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        threeRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        fourRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        fiveRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        sixRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        sevenRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        eightRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length;
                        nineRepresentation |> Seq.filter (fun segment -> segment) |> Seq.length|]

    let calcSwitchOffOnCount (digitA: int) (digitB: int) =
        let inline xor a b = (a || b) && not (a && b)
        let digitARepresentation = digits.[digitA]
        let digitBRepresentation = digits.[digitB]
        seq {0 .. digitARepresentation.Length - 1} |> Seq.map (fun index -> xor digitARepresentation.[index] digitBRepresentation.[index]) |> Seq.filter (fun segment -> segment) |> Seq.length

    let digitsConvertCount = Array2D.init 10 10 (fun digitA digitB -> calcSwitchOffOnCount digitA digitB)

    let calcSwitchCount (digit: int) = digitsSizes.[digit]

    let generateDirectPreparedData () =
        let preparedData = Array.zeroCreate PreparedDataSize
        for number in 0 .. 9 do
            preparedData.[number] <- 2 * digitsSizes.[number]
        for number in 10 .. 99 do
            let numberDigits = number |> NumbersDigits.GetDigits
            let switchOnOffCount = 2 * (numberDigits |> Seq.map (fun digit -> digitsSizes.[digit]) |> Seq.sum)
            let nextNumber = numberDigits |> Seq.sum
            preparedData.[number] <- preparedData.[nextNumber] + switchOnOffCount
        preparedData

    let generateSmartPreparedData () =
        let preparedData = Array.zeroCreate PreparedDataSize
        for number in 0 .. 9 do
            // only switch off
            preparedData.[number] <- digitsSizes.[number]
        for number in 10 .. 99 do
            let numberDigits = number |> NumbersDigits.GetDigits
            let nextNumber = numberDigits.[0] + numberDigits.[1]
            if nextNumber < 10 then
                // switch off first digit, switch on/off segments in the second digit
                preparedData.[number] <- (digitsSizes.[numberDigits.[0]] + digitsConvertCount.[numberDigits.[1], nextNumber]) + preparedData.[nextNumber]
            else
                let nextDigits = nextNumber |> NumbersDigits.GetDigits
                // switch on/off segments in the first and second digits
                preparedData.[number] <- (digitsConvertCount.[numberDigits.[0], nextDigits.[0]] + digitsConvertCount.[numberDigits.[1], nextDigits.[1]]) + preparedData.[nextNumber]
        preparedData

    let calcDirectSwitchCount (preparedData: int[]) (numberDigits: int list) =
        let nextNumber = numberDigits |> Seq.sum
        let switchOnOffCount = 2 * (numberDigits |> Seq.map (fun digit -> digit |> calcSwitchCount) |> Seq.sum)
        switchOnOffCount + preparedData.[nextNumber]

    let calcSmartSwitchCount (preparedData: int[]) (numberDigits: int list) =
        let nextNumber = numberDigits |> Seq.sum
        let nextNumberDigits = nextNumber |> NumbersDigits.GetDigits
        let switchOnCount = numberDigits |> Seq.map (fun digit -> digit |> calcSwitchCount) |> Seq.sum
        let digitsLengthDiff = numberDigits.Length - nextNumberDigits.Length
        let switchOffCountForPrefix = seq {0 .. digitsLengthDiff - 1} |> Seq.map (fun index -> numberDigits.[index] |> calcSwitchCount) |> Seq.sum
        let switchOffOnCountForSuffix = seq {0 .. nextNumberDigits.Length - 1} |> Seq.map (fun index -> digitsConvertCount.[numberDigits.[digitsLengthDiff + index], nextNumberDigits.[index]]) |> Seq.sum
        switchOnCount + switchOffCountForPrefix + switchOffOnCountForSuffix + preparedData.[nextNumber]

    let calcSwitchCountDiff (directPreparedData: int[]) (smartPreparedData: int[]) (prime: int) =
        let primeDigits = prime |> NumbersDigits.GetDigits
        (primeDigits |> calcDirectSwitchCount directPreparedData) - (primeDigits |> calcSmartSwitchCount smartPreparedData)

    let solveImpl (leftBorder: int) (rightBorder: int) =
        let sieve = rightBorder |> EratosSieve.Create
        let directPreparedData = generateDirectPreparedData ()
        let smartPreparedData = generateSmartPreparedData ()
        sieve.ToSeq() |> Seq.skipWhile (fun prime -> prime < leftBorder) |> Seq.map (fun prime -> prime |> calcSwitchCountDiff directPreparedData smartPreparedData) |> Seq.sum

    [<TestCase(10000000, 20000000, 13625242, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(leftBorder: int, rightBorder: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, leftBorder, rightBorder)
