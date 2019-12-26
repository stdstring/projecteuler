namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

// Three distinct points are plotted at random on a Cartesian plane, for which -1000 <= x, y <= 1000, such that a triangle is formed.
// Consider the following two triangles:
// A(-340,495), B(-153,-910), C(835,-947)
// X(-175,41), Y(-421,-714), Z(574,-645)
// It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.
// Using problem_012.dat, a data file containing the co-ordinates of one thousand "random" triangles, find the number of triangles for which the interior contains the origin.
// NOTE: The first two examples in the file represent the triangles in the example given above.

// TODO (std_string) : probably, move into CommonLib
module Problem102Impl =
    type Point = {X: int; Y: int}

open Problem102Impl

[<TestFixture>]
type Problem102() =

    let calcVectorZProduct (point1: Point) (point2: Point) = point1.Y * point2.X - point1.X * point2.Y

    let checkTriangle([point1; point2; point3]: Point list) =
        let productZ1 = calcVectorZProduct point1 point2
        let productZ2 = calcVectorZProduct point2 point3
        let productZ3 = calcVectorZProduct point3 point1
        match productZ1, productZ2, productZ3 with
        | _ when (productZ1 > 0) && (productZ2 > 0) && (productZ3 > 0) -> true
        | _ when (productZ1 < 0) && (productZ2 < 0) && (productZ3 < 0) -> true
        | _ -> false

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