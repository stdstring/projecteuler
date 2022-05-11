namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// A hexagonal tile with number 1 is surrounded by a ring of six hexagonal tiles, starting at "12 o'clock" and numbering the tiles 2 to 7 in an anti-clockwise direction.
// New rings are added in the same fashion, with the next rings being numbered 8 to 19, 20 to 37, 38 to 61, and so on. The diagram here (https://projecteuler.net/problem=128) shows the first three rings.
// By finding the difference between tile n and each of its six neighbours we shall define PD(n) to be the number of those differences which are prime.
// For example, working clockwise around tile 8 the differences are 12, 29, 11, 6, 1, and 13. So PD(8) = 3.
// In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and 10, hence PD(17) = 2.
// It can be shown that the maximum value of PD(n) is 3.
// If all of the tiles for which PD(n) = 3 are listed in ascending order to form a sequence, the 10th tile would be 271.
// Find the 2000th tile in this sequence.

module Problem128Impl =

    type LayerData = {First: int64; Last: int64; SideSize: int64}

    let createInitLayerData () =
        {LayerData.First = 1L; LayerData.Last = 1L; LayerData.SideSize = 0L}

    let generateNextLayerData (data: LayerData) =
        let nextFirst = data.Last + 1L
        let nextSize = data.SideSize + 1L
        let nextLast = data.Last + 6L * nextSize
        {LayerData.First = nextFirst; LayerData.Last = nextLast; LayerData.SideSize = nextSize}

    type CellData = {Cell: int64; PrimeDiffs: int64 list}

open Problem128Impl

[<TestFixture>]
type Problem128() =

    let generateCellNeighbours (cell: int64) (layer: LayerData) =
        let prevFirst = layer.First - 6L * (layer.SideSize - 1L)
        let prevLast = layer.First - 1L
        let nextFirst = layer.Last + 1L
        let nextLast = layer.Last + 6L * (layer.SideSize + 1L)
        match cell with
        | 1L -> [2L; 3L; 4L; 5L; 6L; 7L]
        | 2L -> [1L; 3L; 7L; nextFirst; nextFirst + 1L; nextLast]
        | 7L -> [1L; 2L; 6L; nextFirst; nextLast - 1L; nextLast]
        | _ when cell = layer.First -> [prevFirst; cell + 1L; layer.Last; nextFirst; nextFirst + 1L; nextLast]
        | _ when cell = layer.Last -> [prevFirst; prevLast; layer.First; cell - 1L; nextLast - 1L; nextLast]
        | _ -> failwith "Uninteresting cell value"

    let calcDifferences (cell: int64) (neighbours: int64 list) =
        neighbours |> List.map (fun neighbour -> (cell - neighbour) |> abs)

    let calcPrimeDifferences (sieve: EratosSieve) (differences: int64 list) =
        differences |> List.filter (fun difference -> (difference > 1L) && difference |> sieve.IsPrime)

    let generatePrimeDifferences (cell: int64) (layer: LayerData) (sieve: EratosSieve) =
        {CellData.Cell = cell; CellData.PrimeDiffs = layer |> generateCellNeighbours cell |> calcDifferences cell |> calcPrimeDifferences sieve}

    let solveImpl (number: int) =
        // Note: It can be shown that PD(n) can be equal 3 only for first and last cells of the layer.
        // It's obvious that any cell has 6 neighbours. If we take any other cell, then differences with neighbours in the same layer will be 1.
        // 2 cells from 4 neighbours from neighbour layers have the same parity as the current one; and difference between such cells will be even number, i.e not prime number.
        // So, the maximum value of PD(n) for such cells can be 2only, not 3.
        let sieve = 10000000 |> EratosSieve.Create
        let generator (iteration: int, layer: LayerData) =
            match iteration with
            | 0 -> (sieve |> generatePrimeDifferences layer.First layer, (iteration + 1, layer |> generateNextLayerData)) |> Some
            | _ when iteration % 2 = 1 -> (sieve |> generatePrimeDifferences layer.First layer, (iteration + 1, layer)) |> Some
            | _ -> (sieve |> generatePrimeDifferences layer.Last layer, (iteration + 1, layer |> generateNextLayerData)) |> Some
        (0, createInitLayerData ()) |> Seq.unfold generator |> Seq.filter (fun data -> data.PrimeDiffs.Length = 3) |> Seq.map (fun data -> data.Cell) |> Seq.skip (number - 1) |> Seq.head

    [<TestCase(10, 271L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(2000, 14516824220L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(number: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, number)