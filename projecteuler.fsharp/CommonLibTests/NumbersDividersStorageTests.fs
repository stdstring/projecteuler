namespace CommonLibTests

open NUnit.Framework
open CommonLib
open System

[<TestFixture>]
type NumbersPrimeDividersStorageTests() =

    [<Test>]
    member public this.Create() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreatePrimeDividersStorage(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreatePrimeDividersStorage(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreatePrimeDividersStorage(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreatePrimeDividersStorage(1000000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> NumbersDividersStorageFactory.CreatePrimeDividersStorage(2) |> ignore)
        Assert.DoesNotThrow(fun() -> NumbersDividersStorageFactory.CreatePrimeDividersStorage(3) |> ignore)
        Assert.DoesNotThrow(fun() -> NumbersDividersStorageFactory.CreatePrimeDividersStorage(10) |> ignore)

    [<Test>]
    member public this.GetDividersSet() =
        let storage = NumbersDividersStorageFactory.CreatePrimeDividersStorage(20)
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetDividersSet(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetDividersSet(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetDividersSet(21) |> ignore) |> ignore
        this.CheckGetDividersSet(storage, 1)
        this.CheckGetDividersSet(storage, 2, 2)
        this.CheckGetDividersSet(storage, 3, 3)
        this.CheckGetDividersSet(storage, 4, 2)
        this.CheckGetDividersSet(storage, 5, 5)
        this.CheckGetDividersSet(storage, 6, 2, 3)
        this.CheckGetDividersSet(storage, 7, 7)
        this.CheckGetDividersSet(storage, 8, 2)
        this.CheckGetDividersSet(storage, 9, 3)
        this.CheckGetDividersSet(storage, 10, 2, 5)
        this.CheckGetDividersSet(storage, 11, 11)
        this.CheckGetDividersSet(storage, 12, 2, 3)
        this.CheckGetDividersSet(storage, 13, 13)
        this.CheckGetDividersSet(storage, 14, 2, 7)
        this.CheckGetDividersSet(storage, 15, 3, 5)
        this.CheckGetDividersSet(storage, 16, 2)
        this.CheckGetDividersSet(storage, 17, 17)
        this.CheckGetDividersSet(storage, 18, 2, 3)
        this.CheckGetDividersSet(storage, 19, 19)
        this.CheckGetDividersSet(storage, 20, 2, 5)

    [<Test>]
    member public this.CalcPhiFunction() =
        let storage = NumbersDividersStorageFactory.CreatePrimeDividersStorage(20)
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.CalcPhiFunction(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.CalcPhiFunction(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.CalcPhiFunction(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.CalcPhiFunction(21) |> ignore) |> ignore
        Assert.AreEqual(1, storage.CalcPhiFunction(2))
        Assert.AreEqual(2, storage.CalcPhiFunction(3))
        Assert.AreEqual(2, storage.CalcPhiFunction(4))
        Assert.AreEqual(4, storage.CalcPhiFunction(5))
        Assert.AreEqual(2, storage.CalcPhiFunction(6))
        Assert.AreEqual(6, storage.CalcPhiFunction(7))
        Assert.AreEqual(4, storage.CalcPhiFunction(8))
        Assert.AreEqual(6, storage.CalcPhiFunction(9))
        Assert.AreEqual(4, storage.CalcPhiFunction(10))
        Assert.AreEqual(10, storage.CalcPhiFunction(11))
        Assert.AreEqual(4, storage.CalcPhiFunction(12))
        Assert.AreEqual(12, storage.CalcPhiFunction(13))
        Assert.AreEqual(6, storage.CalcPhiFunction(14))
        Assert.AreEqual(8, storage.CalcPhiFunction(15))
        Assert.AreEqual(8, storage.CalcPhiFunction(16))
        Assert.AreEqual(16, storage.CalcPhiFunction(17))
        Assert.AreEqual(6, storage.CalcPhiFunction(18))
        Assert.AreEqual(18, storage.CalcPhiFunction(19))
        Assert.AreEqual(8, storage.CalcPhiFunction(20))

    member private this.CheckGetDividersSet(storage: NumbersPrimeDividersStorage, number: int, [<ParamArray>] primeDividers: int[]) =
        Assert.AreEqual(primeDividers |> Seq.toList |> List.sort, storage.GetDividersSet(number) |> Seq.toList |> List.sort)

[<TestFixture>]
type NumbersDividersStorageTests() =

    [<Test>]
    member public this.Create() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreateDividersStorage(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreateDividersStorage(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreateDividersStorage(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividersStorageFactory.CreateDividersStorage(1000000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> NumbersDividersStorageFactory.CreateDividersStorage(2) |> ignore)
        Assert.DoesNotThrow(fun() -> NumbersDividersStorageFactory.CreateDividersStorage(3) |> ignore)
        Assert.DoesNotThrow(fun() -> NumbersDividersStorageFactory.CreateDividersStorage(10) |> ignore)

    [<Test>]
    member public this.GetDividersSet() =
        let storage = NumbersDividersStorageFactory.CreateDividersStorage(20)
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetDividersSet(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetDividersSet(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetDividersSet(21) |> ignore) |> ignore
        this.CheckGetDividersSet(storage, 1, 1)
        this.CheckGetDividersSet(storage, 2, 1, 2)
        this.CheckGetDividersSet(storage, 3, 1, 3)
        this.CheckGetDividersSet(storage, 4, 1, 2, 4)
        this.CheckGetDividersSet(storage, 5, 1, 5)
        this.CheckGetDividersSet(storage, 6, 1, 2, 3, 6)
        this.CheckGetDividersSet(storage, 7, 1, 7)
        this.CheckGetDividersSet(storage, 8, 1, 2, 4, 8)
        this.CheckGetDividersSet(storage, 9, 1, 3, 9)
        this.CheckGetDividersSet(storage, 10, 1, 2, 5, 10)
        this.CheckGetDividersSet(storage, 11, 1, 11)
        this.CheckGetDividersSet(storage, 12, 1, 2, 3, 4, 6, 12)
        this.CheckGetDividersSet(storage, 13, 1, 13)
        this.CheckGetDividersSet(storage, 14, 1, 2, 7, 14)
        this.CheckGetDividersSet(storage, 15, 1, 3, 5, 15)
        this.CheckGetDividersSet(storage, 16, 1, 2, 4, 8, 16)
        this.CheckGetDividersSet(storage, 17, 1, 17)
        this.CheckGetDividersSet(storage, 18, 1, 2, 3, 6, 9, 18)
        this.CheckGetDividersSet(storage, 19, 1, 19)
        this.CheckGetDividersSet(storage, 20, 1, 2, 4, 5, 10, 20)

    member private this.CheckGetDividersSet(storage: NumbersDividersStorage, number: int, [<ParamArray>] primeDividers: int[]) =
        Assert.AreEqual(primeDividers |> Seq.toList |> List.sort, storage.GetDividersSet(number) |> Seq.toList |> List.sort)