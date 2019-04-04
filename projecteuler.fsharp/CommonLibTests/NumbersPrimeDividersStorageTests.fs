namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib

[<TestFixture>]
type NumbersPrimeDividersStorageTests() =

    [<Test>]
    member public this.Create() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersPrimeDividersStorage.Create(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersPrimeDividersStorage.Create(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersPrimeDividersStorage.Create(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersPrimeDividersStorage.Create(1000000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> NumbersPrimeDividersStorage.Create(2) |> ignore)
        Assert.DoesNotThrow(fun() -> NumbersPrimeDividersStorage.Create(3) |> ignore)
        Assert.DoesNotThrow(fun() -> NumbersPrimeDividersStorage.Create(10) |> ignore)

    [<Test>]
    member public this.GetPrimeDividersSet() =
        let storage = NumbersPrimeDividersStorage.Create(20)
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetPrimeDividersSet(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetPrimeDividersSet(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetPrimeDividersSet(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> storage.GetPrimeDividersSet(21) |> ignore) |> ignore
        this.CheckGetPrimeDividersSet(storage, 2, 2)
        this.CheckGetPrimeDividersSet(storage, 3, 3)
        this.CheckGetPrimeDividersSet(storage, 4, 2)
        this.CheckGetPrimeDividersSet(storage, 5, 5)
        this.CheckGetPrimeDividersSet(storage, 6, 2, 3)
        this.CheckGetPrimeDividersSet(storage, 7, 7)
        this.CheckGetPrimeDividersSet(storage, 8, 2)
        this.CheckGetPrimeDividersSet(storage, 9, 3)
        this.CheckGetPrimeDividersSet(storage, 10, 2, 5)
        this.CheckGetPrimeDividersSet(storage, 11, 11)
        this.CheckGetPrimeDividersSet(storage, 12, 2, 3)
        this.CheckGetPrimeDividersSet(storage, 13, 13)
        this.CheckGetPrimeDividersSet(storage, 14, 2, 7)
        this.CheckGetPrimeDividersSet(storage, 15, 3, 5)
        this.CheckGetPrimeDividersSet(storage, 16, 2)
        this.CheckGetPrimeDividersSet(storage, 17, 17)
        this.CheckGetPrimeDividersSet(storage, 18, 2, 3)
        this.CheckGetPrimeDividersSet(storage, 19, 19)
        this.CheckGetPrimeDividersSet(storage, 20, 2, 5)

    [<Test>]
    member public this.CalcPhiFunction() =
        let storage = NumbersPrimeDividersStorage.Create(20)
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

    member private this.CheckGetPrimeDividersSet(storage: NumbersPrimeDividersStorage, number: int, [<ParamArray>] primeDividers: int[]) =
        let primeDividersSet = primeDividers |> Set.ofArray
        Assert.AreEqual(primeDividersSet, storage.GetPrimeDividersSet(number))
