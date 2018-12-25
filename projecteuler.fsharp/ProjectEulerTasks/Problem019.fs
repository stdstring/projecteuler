namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// You are given the following information, but you may prefer to do some research for yourself.
// 1 Jan 1900 was a Monday. Thirty days has September, April, June and November.
// All the rest have thirty-one, Saving February alone, which has twenty-eight, rain or shine. And on leap years, twenty-nine.
// A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
// How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

type ProcessData = {LastDay: int; SundaysCount: int}

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
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl ())