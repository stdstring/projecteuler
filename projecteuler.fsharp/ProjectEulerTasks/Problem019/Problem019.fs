namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem019Impl =
    type ProcessData = {LastDay: int; SundaysCount: int}

open Problem019Impl

[<TestFixture>]
type Problem019() =

    let usualYear = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]
    let leapYear = [|31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]

    let isLeapYear (year: int) = (year % 4 = 0) && ((year % 100 <> 0) || (year % 400 = 0))

    let processYear (processData: ProcessData) (year: int) =
        let days =  match year |> isLeapYear with | true -> leapYear | false -> usualYear
        let processMonth (data: ProcessData) (monthDays: int) =
            let sundaysCount = data.SundaysCount + match data.LastDay with | 6 -> 1 | _ -> 0
            let lastDay = (data.LastDay + monthDays) % 7
            {ProcessData.LastDay = lastDay; ProcessData.SundaysCount = sundaysCount}
        days |> Seq.fold processMonth processData

    let solveImpl () =
        // 1900 is not the leap year
        // 1 Jan 1900 - Monday => InitDayNumber = 1
        let initDayNumber = 1
        let initLastDay = (initDayNumber + 365) % 7 - 1
        (seq {1901 .. 2000} |> Seq.fold processYear {ProcessData.LastDay = initLastDay; ProcessData.SundaysCount = 0}).SundaysCount

    [<TestCase(171, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)