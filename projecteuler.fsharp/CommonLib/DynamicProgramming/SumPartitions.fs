namespace CommonLib.DynamicProgramming

open Checked
open System

type SumPartitions<'TItem> private (maxNumber: int, partitionCountStorage: 'TItem[]) =

    member public this.GetPartitionCount(number: int) =
        if (number <= 0) || (number > maxNumber) then
            raise (new ArgumentOutOfRangeException("number"))
        partitionCountStorage.[number - 1]

    static member public CreateInt(maxNumber: int, items: int[]) =
        if (maxNumber <= 0) then
            raise (new ArgumentOutOfRangeException("maxNumber"))
        let items = items |> Array.sort
        let storage = Array2D.createBased 0 0 (maxNumber + 1) (maxNumber + 1) 0
        seq {1 .. items.Length} |> Seq.iter (fun kIndex -> storage.[0, kIndex]<-1)
        let processStorageValue (i: int, kIndex: int) =
            match i < items.[kIndex - 1] with
            | true -> storage.[i, kIndex]<-storage.[i, kIndex - 1]
            | false -> storage.[i, kIndex]<-storage.[i, kIndex - 1] + storage.[i - items.[kIndex - 1], kIndex]
        seq {for i in 1 .. maxNumber do for kIndex in 1 .. items.Length do yield (i, kIndex)} |> Seq.iter processStorageValue
        let partitionCountStorage = seq {1 .. maxNumber} |> Seq.map (fun number -> storage.[number, items.Length]) |> Seq.toArray
        new SumPartitions<int>(maxNumber, partitionCountStorage)

    static member public CreateInt64(maxNumber: int, items: int[]) =
        if (maxNumber <= 0) then
            raise (new ArgumentOutOfRangeException("maxNumber"))
        let items = items |> Array.sort
        let storage = Array2D.createBased 0 0 (maxNumber + 1) (maxNumber + 1) 0L
        seq {1 .. items.Length} |> Seq.iter (fun kIndex -> storage.[0, kIndex]<-1L)
        let processStorageValue (i: int, kIndex: int) =
            match i < items.[kIndex - 1] with
            | true -> storage.[i, kIndex]<-storage.[i, kIndex - 1]
            | false -> storage.[i, kIndex]<-storage.[i, kIndex - 1] + storage.[i - items.[kIndex - 1], kIndex]
        seq {for i in 1 .. maxNumber do for kIndex in 1 .. items.Length do yield (i, kIndex)} |> Seq.iter processStorageValue
        let partitionCountStorage = seq {1 .. maxNumber} |> Seq.map (fun number -> storage.[number, items.Length]) |> Seq.toArray
        new SumPartitions<int64>(maxNumber, partitionCountStorage)

    static member public CreateBigInt(maxNumber: int, items: int[]) =
        if (maxNumber <= 0) then
            raise (new ArgumentOutOfRangeException("maxNumber"))
        let items = items |> Array.sort
        let storage = Array2D.createBased 0 0 (maxNumber + 1) (maxNumber + 1) 0I
        seq {1 .. items.Length} |> Seq.iter (fun kIndex -> storage.[0, kIndex]<-1I)
        let processStorageValue (i: int, kIndex: int) =
            match i < items.[kIndex - 1] with
            | true -> storage.[i, kIndex]<-storage.[i, kIndex - 1]
            | false -> storage.[i, kIndex]<-storage.[i, kIndex - 1] + storage.[i - items.[kIndex - 1], kIndex]
        seq {for i in 1 .. maxNumber do for kIndex in 1 .. items.Length do yield (i, kIndex)} |> Seq.iter processStorageValue
        let partitionCountStorage = seq {1 .. maxNumber} |> Seq.map (fun number -> storage.[number, items.Length]) |> Seq.toArray
        new SumPartitions<bigint>(maxNumber, partitionCountStorage)