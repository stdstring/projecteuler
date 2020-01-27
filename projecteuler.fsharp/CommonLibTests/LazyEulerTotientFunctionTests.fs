namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib

[<TestFixture>]
type LazyEulerTotientFunctionTests() =

    [<Test>]
    member public this.Create() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> LazyEulerTotientFunction.Create(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> LazyEulerTotientFunction.Create(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> LazyEulerTotientFunction.Create(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> LazyEulerTotientFunction.Create(125000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> LazyEulerTotientFunction.Create(2) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> LazyEulerTotientFunction.Create(3) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> LazyEulerTotientFunction.Create(10) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> LazyEulerTotientFunction.Create(19) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> LazyEulerTotientFunction.Create(100) |> Assert.IsNotNull)

    [<Test>]
    member public this.GetValue() =
        this.CheckGetValue(2, 1, 1)
        this.CheckGetValue(3, 1, 1, 2)
        this.CheckGetValue(4, 1, 1, 2, 2)
        this.CheckGetValue(5, 1, 1, 2, 2, 4)
        this.CheckGetValue(6, 1, 1, 2, 2, 4, 2)
        this.CheckGetValue(7, 1, 1, 2, 2, 4, 2, 6)
        this.CheckGetValue(8, 1, 1, 2, 2, 4, 2, 6, 4)
        this.CheckGetValue(9, 1, 1, 2, 2, 4, 2, 6, 4, 6)
        this.CheckGetValue(10, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4)
        this.CheckGetValue(11, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10)
        this.CheckGetValue(12, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4)
        this.CheckGetValue(13, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12)
        this.CheckGetValue(14, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6)
        this.CheckGetValue(15, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8)
        this.CheckGetValue(16, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8)
        this.CheckGetValue(17, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16)
        this.CheckGetValue(18, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6)
        this.CheckGetValue(19, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18)

    [<Test>]
    member public this.IsPrime() =
        this.CheckIsPrime(2, true)
        this.CheckIsPrime(3, true, true)
        this.CheckIsPrime(4, true, true, false)
        this.CheckIsPrime(5, true, true, false, true)
        this.CheckIsPrime(6, true, true, false, true, false)
        this.CheckIsPrime(7, true, true, false, true, false, true)
        this.CheckIsPrime(8, true, true, false, true, false, true, false)
        this.CheckIsPrime(9, true, true, false, true, false, true, false, false)
        this.CheckIsPrime(10, true, true, false, true, false, true, false, false, false)
        this.CheckIsPrime(11, true, true, false, true, false, true, false, false, false, true)
        this.CheckIsPrime(12, true, true, false, true, false, true, false, false, false, true, false)
        this.CheckIsPrime(13, true, true, false, true, false, true, false, false, false, true, false, true)
        this.CheckIsPrime(14, true, true, false, true, false, true, false, false, false, true, false, true, false)
        this.CheckIsPrime(15, true, true, false, true, false, true, false, false, false, true, false, true, false, false)
        this.CheckIsPrime(16, true, true, false, true, false, true, false, false, false, true, false, true, false, false, false)
        this.CheckIsPrime(17, true, true, false, true, false, true, false, false, false, true, false, true, false, false, false, true)
        this.CheckIsPrime(18, true, true, false, true, false, true, false, false, false, true, false, true, false, false, false, true, false)
        this.CheckIsPrime(19, true, true, false, true, false, true, false, false, false, true, false, true, false, false, false, true, false, true)

    [<Test>]
    member public this.GetPrimes() =
        Assert.AreEqual([2], LazyEulerTotientFunction.Create(2).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3], LazyEulerTotientFunction.Create(3).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3], LazyEulerTotientFunction.Create(4).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5], LazyEulerTotientFunction.Create(5).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5], LazyEulerTotientFunction.Create(6).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], LazyEulerTotientFunction.Create(7).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], LazyEulerTotientFunction.Create(8).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], LazyEulerTotientFunction.Create(9).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], LazyEulerTotientFunction.Create(10).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11], LazyEulerTotientFunction.Create(11).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11], LazyEulerTotientFunction.Create(12).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], LazyEulerTotientFunction.Create(13).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], LazyEulerTotientFunction.Create(14).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], LazyEulerTotientFunction.Create(15).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], LazyEulerTotientFunction.Create(16).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13; 17], LazyEulerTotientFunction.Create(17).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13; 17], LazyEulerTotientFunction.Create(18).GetPrimes() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13; 17; 19], LazyEulerTotientFunction.Create(19).GetPrimes() |> Seq.toList)

    member private this.CheckGetValue(maxNumber: int, [<ParamArray>] expectedValues: int[]) =
        let eulerTotientFunction = maxNumber |> LazyEulerTotientFunction.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(0) |> ignore) |> ignore
        //Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(eulerTotientFunction.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, 1 + shift |> eulerTotientFunction.GetValue))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(0L) |> ignore) |> ignore
        //Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(eulerTotientFunction.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue |> int64, shift |> int64 |> (+) 1L |> eulerTotientFunction.GetValue))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(0I) |> ignore) |> ignore
        //Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.GetValue(eulerTotientFunction.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue |> bigint, shift |> bigint |> (+) 1I |> eulerTotientFunction.GetValue))

    member private this.CheckIsPrime(maxNumber: int, [<ParamArray>] expectedValues: bool[]) =
        let eulerTotientFunction = maxNumber |> LazyEulerTotientFunction.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(eulerTotientFunction.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, 2 + shift |> eulerTotientFunction.IsPrime))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(eulerTotientFunction.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, shift |> int64 |> (+) 2L |> eulerTotientFunction.IsPrime))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> eulerTotientFunction.IsPrime(eulerTotientFunction.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, shift |> bigint |> (+) 2I |> eulerTotientFunction.IsPrime))