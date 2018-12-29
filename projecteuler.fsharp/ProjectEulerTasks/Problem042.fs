namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System
open System.Text
open System.Collections.Generic

// The n-th term of the sequence of triangle numbers is given by, t(n) = (1/2) * n * (n + 1); so the first ten triangle numbers are: 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
// By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value.
// For example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word value is a triangle number then we shall call the word a triangle word.
// Using words.txt, a 16K text file containing nearly two-thousand common English words, how many are triangle words?

[<TestFixture>]
type Problem042() =

    let maxTriangleNumber = 200

    let asciiEncoder = Encoding.ASCII

    let start = asciiEncoder.GetBytes("A").[0]

    let calcWordValue (word: string) = word |> asciiEncoder.GetBytes |> Seq.map (fun byteValue -> byteValue - start + 1uy |> int) |> Seq.sum

    let solveImpl (dataFilename: string) =
        let names = File.ReadAllText(Path.Combine("Data", dataFilename)).Split([|','; '"'|], StringSplitOptions.RemoveEmptyEntries)
        let triangleNumbers = seq {1 .. maxTriangleNumber} |> Seq.map (fun number -> number * (number + 1) / 2) |> HashSet<int> :> ISet<int>
        names |> Seq.map calcWordValue |> Seq.filter (fun number -> triangleNumbers.Contains(number)) |> Seq.length

    [<TestCase("problem_042.dat", 162, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)
