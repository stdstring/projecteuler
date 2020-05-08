namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// Sam and Max are asked to transform two digital clocks into two "digital root" clocks. A digital root clock is a digital clock that calculates digital roots step by step.
// When a clock is fed a number, it will show it and then it will start the calculation, showing all the intermediate values until it gets to the result.
// For example, if the clock is fed the number 137, it will show: "137" -> "11" -> "2" and then it will go black, waiting for the next number.
// Every digital number consists of some light segments: three horizontal (top, middle, bottom) and four vertical (top-left, top-right, bottom-left, bottom-right).
// Number "1" is made of vertical top-right and bottom-right, number "4" is made by middle horizontal and vertical top-left, top-right and bottom-right. Number "8" lights them all.
// The clocks consume energy only when segments are turned on/off. To turn on a "2" will cost 5 transitions, while a "7" will cost only 4 transitions.
// Sam and Max built two different clocks.
// Sam's clock is fed e.g. number 137: the clock shows "137", then the panel is turned off, then the next number ("11") is turned on, then the panel is turned off again and finally the last number ("2") is turned on and, after some time, off.
// For the example, with number 137, Sam's clock requires:
// "137" : (2 + 5 + 4) × 2 = 22 transitions ("137" on/off).
// "11"  : (2 + 2) × 2 = 8 transitions ("11" on/off).
// "2"   : (5) × 2 = 10 transitions ("2" on/off).
// For a grand total of 40 transitions.
// Max's clock works differently. Instead of turning off the whole panel, it is smart enough to turn off only those segments that won't be needed for the next number.
// For number 137, Max's clock requires:
// "137" : 2 + 5 + 4 = 11 transitions ("137" on), 7 transitions (to turn off the segments that are not needed for number "11").
// "11" : 0 transitions (number "11" is already turned on correctly), 3 transitions (to turn off the first "1" and the bottom part of the second "1"; the top part is common with number "2").
// "2" : 4 transitions (to turn on the remaining segments in order to get a "2"), 5 transitions (to turn off number "2").
// For a grand total of 30 transitions.
// Of course, Max's clock consumes less power than Sam's one.
// The two clocks are fed all the prime numbers between A = 10^7 and B = 2*10^7.
// Find the difference between the total number of transitions needed by Sam's clock and that needed by Max's one

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

    let digits = [|zeroRepresentation; oneRepresentation; twoRepresentation; threeRepresentation; fourRepresentation; fiveRepresentation; sixRepresentation; sevenRepresentation; eightRepresentation; nineRepresentation|]

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
