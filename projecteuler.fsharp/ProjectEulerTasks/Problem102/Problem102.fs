namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

// TODO (std_string) : probably, move into CommonLib
module Problem102Impl =
    type Point = {X: int; Y: int}

open Problem102Impl

[<TestFixture>]
type Problem102() =

    let calcVectorZProduct (point1: Point) (point2: Point) = point1.Y * point2.X - point1.X * point2.Y

    let checkTriangle = function
        | [point1; point2; point3] ->
            let productZ1 = calcVectorZProduct point1 point2
            let productZ2 = calcVectorZProduct point2 point3
            let productZ3 = calcVectorZProduct point3 point1
            match productZ1, productZ2, productZ3 with
            | _ when (productZ1 > 0) && (productZ2 > 0) && (productZ3 > 0) -> true
            | _ when (productZ1 < 0) && (productZ2 < 0) && (productZ3 < 0) -> true
            | _ -> false
        | _ -> failwith "Unexpected branch of match expression"

    let solveImpl (dataFilename: string) =
        let createTriangle (numbers: int[]) =
            [{Point.X = numbers.[0]; Point.Y = numbers.[1]}; {Point.X = numbers.[2]; Point.Y = numbers.[3]}; {Point.X = numbers.[4]; Point.Y = numbers.[5]}]
        File.ReadAllLines(Path.Combine("Data", dataFilename)) |>
        Seq.map (fun line -> line.Split(',') |> Seq.map int |> Seq.toArray |> createTriangle) |>
        Seq.filter (fun points -> points |> checkTriangle) |>
        Seq.length

    [<TestCase("problem_102.dat", 228, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)