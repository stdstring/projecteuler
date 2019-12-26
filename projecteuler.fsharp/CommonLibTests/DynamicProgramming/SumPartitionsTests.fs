namespace CommonLibTests.DynamicProgramming

open CommonLib.DynamicProgramming
open NUnit.Framework
open System

[<TestFixture>]
type SumPartitionsTests() =

    [<Test>]
    member public this.CreateInt() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SumPartitions<int>.CreateInt(-1, [|1 .. 10|]) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SumPartitions<int>.CreateInt(0, [|1 .. 10|]) |> ignore) |> ignore
        // natural numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int>.CreateInt(10, [|1 .. 10|]), [|1; 2; 3; 5; 7; 11; 15; 22; 30; 42|])
        // prime numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int>.CreateInt(10, [|2; 3; 5; 7; 11; 13; 17; 19|]), [|0; 1; 1; 1; 2; 2; 3; 3; 4; 5|])
        // even numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int>.CreateInt(10, [|2; 4; 6; 8; 10|]), [|0; 1; 0; 2; 0; 3; 0; 5; 0; 7|])
        // odd numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int>.CreateInt(10, [|1; 3; 5; 7; 9|]), [|1; 1; 2; 2; 3; 4; 5; 6; 8; 10|])
        // coins
        this.CheckPartitions(SumPartitions<int>.CreateInt(10, [|1; 2; 5|]), [|1; 2; 2; 3; 4; 5; 6; 7; 8; 10|])

    [<Test>]
    member public this.CreateInt64() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SumPartitions<int64>.CreateInt64(-1, [|1 .. 10|]) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SumPartitions<int64>.CreateInt64(0, [|1 .. 10|]) |> ignore) |> ignore
        // natural numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateInt64(10, [|1 .. 10|]), [|1L; 2L; 3L; 5L; 7L; 11L; 15L; 22L; 30L; 42L|])
        // prime numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateInt64(10, [|2; 3; 5; 7; 11; 13; 17; 19|]), [|0L; 1L; 1L; 1L; 2L; 2L; 3L; 3L; 4L; 5L|])
        // even numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateInt64(10, [|2; 4; 6; 8; 10|]), [|0L; 1L; 0L; 2L; 0L; 3L; 0L; 5L; 0L; 7L|])
        // odd numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateInt64(10, [|1; 3; 5; 7; 9|]), [|1L; 1L; 2L; 2L; 3L; 4L; 5L; 6L; 8L; 10L|])
        // coins
        this.CheckPartitions(SumPartitions<int64>.CreateInt64(10, [|1; 2; 5|]), [|1L; 2L; 2L; 3L; 4L; 5L; 6L; 7L; 8L; 10L|])

    [<Test>]
    member public this.CreateBigInt() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SumPartitions<bigint>.CreateBigInt(-1, [|1 .. 10|]) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SumPartitions<bigint>.CreateBigInt(0, [|1 .. 10|]) |> ignore) |> ignore
        // natural numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateBigInt(10, [|1 .. 10|]), [|1I; 2I; 3I; 5I; 7I; 11I; 15I; 22I; 30I; 42I|])
        // prime numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateBigInt(10, [|2; 3; 5; 7; 11; 13; 17; 19|]), [|0I; 1I; 1I; 1I; 2I; 2I; 3I; 3I; 4I; 5I|])
        // even numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateBigInt(10, [|2; 4; 6; 8; 10|]), [|0I; 1I; 0I; 2I; 0I; 3I; 0I; 5I; 0I; 7I|])
        // odd numbers in range 1 .. 10
        this.CheckPartitions(SumPartitions<int64>.CreateBigInt(10, [|1; 3; 5; 7; 9|]), [|1I; 1I; 2I; 2I; 3I; 4I; 5I; 6I; 8I; 10I|])
        // coins
        this.CheckPartitions(SumPartitions<int64>.CreateBigInt(10, [|1; 2; 5|]), [|1I; 2I; 2I; 3I; 4I; 5I; 6I; 7I; 8I; 10I|])

    member private this.CheckPartitions(partitions: SumPartitions<int>, expectedPartitions: int[]) =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(0) |> ignore) |> ignore
        expectedPartitions |> Array.iteri (fun index expectedValue -> let number = index + 1 in Assert.AreEqual(expectedValue, number |> partitions.GetPartitionCount))
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(expectedPartitions.Length + 1) |> ignore) |> ignore

    member private this.CheckPartitions(partitions: SumPartitions<int64>, expectedPartitions: int64[]) =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(0) |> ignore) |> ignore
        expectedPartitions |> Array.iteri (fun index expectedValue -> let number = index + 1 in Assert.AreEqual(expectedValue, number |> partitions.GetPartitionCount))
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(expectedPartitions.Length + 1) |> ignore) |> ignore

    member private this.CheckPartitions(partitions: SumPartitions<bigint>, expectedPartitions: bigint[]) =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(0) |> ignore) |> ignore
        expectedPartitions |> Array.iteri (fun index expectedValue -> let number = index + 1 in Assert.AreEqual(expectedValue, number |> partitions.GetPartitionCount))
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> partitions.GetPartitionCount(expectedPartitions.Length + 1) |> ignore) |> ignore