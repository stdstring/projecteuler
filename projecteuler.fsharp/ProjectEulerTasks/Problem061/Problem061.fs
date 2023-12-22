namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem061Impl =
    type Storage = {mutable Data: int list[]}

open Problem061Impl

[<TestFixture>]
type Problem061() =

    // all numbers in 1000...9999
    // P(3, 44) = 990, P(3, 45) = 1035, ..., P(3, 140) = 9870, P(3, 141) = 10011
    [<Literal>]
    let TriangleInf = 45
    [<Literal>]
    let TriangleSup = 140
    // P(4, 31) = 961, P(4, 32) = 1024, ..., P(4, 99) = 9801, P(4, 100) = 10000
    [<Literal>]
    let SquareInf = 32
    [<Literal>]
    let SquareSup = 99
    // P(5, 25) = 925, P(5, 26) = 1001, ..., P(5, 81) = 9801, P(5, 82) = 10045
    [<Literal>]
    let PentagonalInf = 26
    [<Literal>]
    let PentagonalSup = 81
    // P(6, 22) = 946, P(6, 23) = 1035, ..., P(6, 70) = 9730, P(6, 71) = 10011
    [<Literal>]
    let HexagonalInf = 23
    [<Literal>]
    let HexagonalSup = 70
    // P(7,20) = 970, P(7,21) = 1071, ..., P(7,63) = 9828, P(7,64) = 10144
    [<Literal>]
    let HeptagonalInf = 21
    [<Literal>]
    let HeptagonalSup = 63
    // P(8,18) = 936, P(8,19) = 1045, ..., P(8,58) = 9976, P(8,59) = 10325
    [<Literal>]
    let OctagonalInf = 19
    [<Literal>]
    let OctagonalSup = 58
    [<Literal>]
    let StorageDelta = 10

    let createNumberStorage (numbers: seq<int>) =
        // we skip 00.. 09
        let storageSize = 89
        let processNumberFun (storage: int list[]) (number: int) =
            let firstPart = number / 100
            let secondPart = number % 100
            if secondPart >= StorageDelta then
                storage.[firstPart - StorageDelta]<-number :: storage.[firstPart - StorageDelta]
            storage
        let data = numbers |> Seq.fold processNumberFun (Array.create storageSize [])
        {Storage.Data = data}

    let splitStorages (storages: Storage list) (position: int) =
        match storages, position with
        | [storage], 1 -> storage, []
        | [storage1; storage2], 1 -> storage1, [storage2]
        | [storage1; storage2], 2 -> storage2, [storage1]
        | [storage1; storage2; storage3], 1 -> storage1, [storage2; storage3]
        | [storage1; storage2; storage3], 2 -> storage2, [storage1; storage3]
        | [storage1; storage2; storage3], 3 -> storage3, [storage1; storage2]
        | [storage1; storage2; storage3; storage4], 1 -> storage1, [storage2; storage3; storage4]
        | [storage1; storage2; storage3; storage4], 2 -> storage2, [storage1; storage3; storage4]
        | [storage1; storage2; storage3; storage4], 3 -> storage3, [storage1; storage2; storage4]
        | [storage1; storage2; storage3; storage4], 4 -> storage4, [storage1; storage2; storage3]
        | [storage1; storage2; storage3; storage4; storage5], 1 -> storage1, [storage2; storage3; storage4; storage5]
        | [storage1; storage2; storage3; storage4; storage5], 2 -> storage2, [storage1; storage3; storage4; storage5]
        | [storage1; storage2; storage3; storage4; storage5], 3 -> storage3, [storage1; storage2; storage4; storage5]
        | [storage1; storage2; storage3; storage4; storage5], 4 -> storage4, [storage1; storage2; storage3; storage5]
        | [storage1; storage2; storage3; storage4; storage5], 5 -> storage5, [storage1; storage2; storage3; storage4]
        | _ -> failwith "Unexpected branch of match expression"

    let rec processNumber (storages: Storage list) (startNumber: int) (numbers: int list) =
        let processStorage (currentStorage: Storage) (storagesRest: Storage list) (key: int) =
            currentStorage.Data.[key] |>
            List.filter (fun number -> number % 100 >= StorageDelta) |>
            List.filter (fun number -> numbers |> List.contains number |> not) |>
            List.map (fun number -> number :: numbers |> processNumber storagesRest startNumber) |>
            List.concat
        let processNextNumbers (positionsCount: int) =
            let key = (numbers |> List.head) % 100
            match key with
            | _ when key < StorageDelta -> []
            | _ ->
                seq { 1 .. positionsCount } |>
                Seq.map (fun position -> position |> splitStorages storages) |>
                Seq.map (fun (currentStorage, storagesRest) -> key - StorageDelta |> processStorage currentStorage storagesRest) |>
                List.concat
        match storages with
        // processedNumber = [n5; n4; n3; n2; n1; n0]
        | [] ->
            match (numbers |> List.head) % 100 with
            | value when value = startNumber / 100 -> [numbers]
            | _ -> []
        // processedNumber = [n4; n3; n2; n1; n0]
        | [_] -> processNextNumbers 1
        // processedNumber = [n3; n2; n1; n0]
        | [_; _] -> processNextNumbers 2
        // processedNumber = [n2; n1; n0]
        | [_; _; _] -> processNextNumbers 3
        // processedNumber = [n1; n0]
        | [_; _; _; _] -> processNextNumbers 4
        // processedNumber = [n0]
        | [_; _; _; _; _] -> processNextNumbers 5
        | _ -> failwith "Unexpected branch of match expression"

    let solveImpl () =
        let triangleStorage = seq { TriangleInf .. TriangleSup } |> Seq.map (fun n -> n * (n + 1) / 2) |> createNumberStorage
        let squareStorage = seq { SquareInf .. SquareSup } |> Seq.map (fun n -> n * n) |> createNumberStorage
        let pentagonalStorage = seq { PentagonalInf .. PentagonalSup } |> Seq.map (fun n -> n * (3 * n - 1) / 2) |> createNumberStorage
        let hexagonalStorage = seq { HexagonalInf .. HexagonalSup } |> Seq.map (fun n -> n * (2 * n - 1)) |> createNumberStorage
        let heptagonalStorage = seq { HeptagonalInf .. HeptagonalSup } |> Seq.map (fun n -> n * (5 * n - 3) / 2) |> createNumberStorage
        let storages = [heptagonalStorage; hexagonalStorage; pentagonalStorage; squareStorage; triangleStorage]
        let octagonalNumbers = seq { OctagonalInf .. OctagonalSup } |> Seq.map (fun n -> n * (3 * n - 2))
        let result = octagonalNumbers |> Seq.filter (fun number -> number % 100 >= StorageDelta) |> Seq.map (fun number -> [number] |> processNumber storages number) |> List.concat
        Assert.That(result.Length, Is.EqualTo(1))
        result |> List.head |> List.sum

    [<TestCase(28684, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)