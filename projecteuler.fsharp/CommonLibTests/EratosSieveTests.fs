namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type EratosSieveTests() =

    // TODO (std_string) : think about moving into the common utils
    let convertOptionValue (converter: 'TInput -> 'TOutput) (optionValue: 'TInput option) =
        match optionValue with
        | None -> None
        | Some value -> value |> converter |> Some

    [<Test>]
    member public this.Create() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(1000000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(2) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(3) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(10) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(19) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(100) |> ClassicAssert.IsNotNull)

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

    [<Test>]
    member public this.GetNextPrime() =
        this.CheckGetNextPrime(2, None)
        this.CheckGetNextPrime(3, Some 3, None)
        this.CheckGetNextPrime(4, Some 3, None, None)
        this.CheckGetNextPrime(5, Some 3, Some 5, Some 5, None)
        this.CheckGetNextPrime(6, Some 3, Some 5, Some 5, None, None)
        this.CheckGetNextPrime(7, Some 3, Some 5, Some 5, Some 7, Some 7, None)
        this.CheckGetNextPrime(8, Some 3, Some 5, Some 5, Some 7, Some 7, None, None)
        this.CheckGetNextPrime(9, Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None)
        this.CheckGetNextPrime(10, Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None, None)
        this.CheckGetNextPrime(11, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None)
        this.CheckGetNextPrime(12, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None, None)
        this.CheckGetNextPrime(13, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None)
        this.CheckGetNextPrime(14, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None)
        this.CheckGetNextPrime(15, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None, None)

    [<Test>]
    member public this.ToSeq() =
        ClassicAssert.AreEqual([2], EratosSieve.Create(2).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3], EratosSieve.Create(3).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3], EratosSieve.Create(4).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5], EratosSieve.Create(5).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5], EratosSieve.Create(6).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieve.Create(7).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieve.Create(8).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieve.Create(9).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieve.Create(10).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11], EratosSieve.Create(11).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11], EratosSieve.Create(12).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieve.Create(13).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieve.Create(14).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieve.Create(15).ToSeq() |> Seq.toList)

    member private this.CheckIsPrime(maxNumber: int, [<ParamArray>] expectedValues: bool[]) =
        let sieve = maxNumber |> EratosSieve.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, sieve.IsPrime(2 + shift)))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, shift |> int64 |> (+) 2L |> sieve.IsPrime))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, shift |> bigint |> (+) 2I |> sieve.IsPrime))

    member private this.CheckGetNextPrime(maxNumber: int, [<ParamArray>] expectedValues: int option[]) =
        let sieve = maxNumber |> EratosSieve.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, sieve.GetNextPrime(2 + shift)))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> convertOptionValue int64, shift |> int64 |> (+) 2L |> sieve.GetNextPrime))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> convertOptionValue bigint, shift |> bigint |> (+) 2I |> sieve.GetNextPrime))

[<TestFixture>]
type EratosSieveWithSmallestPrimeFactorsTests() =

    // TODO (std_string) : think about moving into the common utils
    let convertOptionValue (converter: 'TInput -> 'TOutput) (optionValue: 'TInput option) =
        match optionValue with
        | None -> None
        | Some value -> value |> converter |> Some

    [<Test>]
    member public this.Create() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieveWithSmallestPrimeFactors.Create(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieveWithSmallestPrimeFactors.Create(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieveWithSmallestPrimeFactors.Create(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieveWithSmallestPrimeFactors.Create(500000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> EratosSieveWithSmallestPrimeFactors.Create(2) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieveWithSmallestPrimeFactors.Create(3) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieveWithSmallestPrimeFactors.Create(10) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieveWithSmallestPrimeFactors.Create(19) |> ClassicAssert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieveWithSmallestPrimeFactors.Create(100) |> ClassicAssert.IsNotNull)

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

    [<Test>]
    member public this.GetNextPrime() =
        this.CheckGetNextPrime(2, None)
        this.CheckGetNextPrime(3, Some 3, None)
        this.CheckGetNextPrime(4, Some 3, None, None)
        this.CheckGetNextPrime(5, Some 3, Some 5, Some 5, None)
        this.CheckGetNextPrime(6, Some 3, Some 5, Some 5, None, None)
        this.CheckGetNextPrime(7, Some 3, Some 5, Some 5, Some 7, Some 7, None)
        this.CheckGetNextPrime(8, Some 3, Some 5, Some 5, Some 7, Some 7, None, None)
        this.CheckGetNextPrime(9, Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None)
        this.CheckGetNextPrime(10, Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None, None)
        this.CheckGetNextPrime(11, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None)
        this.CheckGetNextPrime(12, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None, None)
        this.CheckGetNextPrime(13, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None)
        this.CheckGetNextPrime(14, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None)
        this.CheckGetNextPrime(15, Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None, None)

    [<Test>]
    member public this.ToSeq() =
        ClassicAssert.AreEqual([2], EratosSieveWithSmallestPrimeFactors.Create(2).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3], EratosSieveWithSmallestPrimeFactors.Create(3).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3], EratosSieveWithSmallestPrimeFactors.Create(4).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5], EratosSieveWithSmallestPrimeFactors.Create(5).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5], EratosSieveWithSmallestPrimeFactors.Create(6).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieveWithSmallestPrimeFactors.Create(7).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieveWithSmallestPrimeFactors.Create(8).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieveWithSmallestPrimeFactors.Create(9).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7], EratosSieveWithSmallestPrimeFactors.Create(10).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11], EratosSieveWithSmallestPrimeFactors.Create(11).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11], EratosSieveWithSmallestPrimeFactors.Create(12).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieveWithSmallestPrimeFactors.Create(13).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieveWithSmallestPrimeFactors.Create(14).ToSeq() |> Seq.toList)
        ClassicAssert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieveWithSmallestPrimeFactors.Create(15).ToSeq() |> Seq.toList)

    [<Test>]
    member public this.GetItem() =
        this.CheckGetItem(2, 2)
        this.CheckGetItem(3, 2, 3)
        this.CheckGetItem(4, 2, 3, 2)
        this.CheckGetItem(5, 2, 3, 2, 5)
        this.CheckGetItem(6, 2, 3, 2, 5, 2)
        this.CheckGetItem(7, 2, 3, 2, 5, 2, 7)
        this.CheckGetItem(8, 2, 3, 2, 5, 2, 7, 2)
        this.CheckGetItem(9, 2, 3, 2, 5, 2, 7, 2, 3)
        this.CheckGetItem(10, 2, 3, 2, 5, 2, 7, 2, 3, 2)
        this.CheckGetItem(11, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11)
        this.CheckGetItem(12, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2)
        this.CheckGetItem(13, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13)
        this.CheckGetItem(14, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2)
        this.CheckGetItem(15, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3)
        this.CheckGetItem(16, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2)
        this.CheckGetItem(17, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17)
        this.CheckGetItem(18, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2)
        this.CheckGetItem(19, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19)
        this.CheckGetItem(20, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2)
        this.CheckGetItem(21, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3)
        this.CheckGetItem(22, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2)
        this.CheckGetItem(23, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23)
        this.CheckGetItem(24, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2)
        this.CheckGetItem(25, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5)
        this.CheckGetItem(26, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2)
        this.CheckGetItem(27, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3)
        this.CheckGetItem(28, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2)
        this.CheckGetItem(29, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29)
        this.CheckGetItem(30, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2)
        this.CheckGetItem(31, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31)
        this.CheckGetItem(32, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2)
        this.CheckGetItem(33, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3)
        this.CheckGetItem(34, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2)
        this.CheckGetItem(35, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5)
        this.CheckGetItem(36, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2)
        this.CheckGetItem(37, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37)
        this.CheckGetItem(38, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2)
        this.CheckGetItem(39, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2, 3)
        this.CheckGetItem(40, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2, 3, 2)
        this.CheckGetItem(41, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2, 3, 2, 41)
        this.CheckGetItem(42, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2, 3, 2, 41, 2)
        this.CheckGetItem(43, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2, 3, 2, 41, 2, 43)
        this.CheckGetItem(44, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2, 3, 2, 41, 2, 43, 2)
        this.CheckGetItem(45, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5, 2, 3, 2, 29, 2, 31, 2, 3, 2, 5, 2, 37, 2, 3, 2, 41, 2, 43, 2, 3)

    [<Test>]
    member public this.CalcSigma0() =
        this.CheckCalcSigma0(2, 2)
        this.CheckCalcSigma0(3, 2, 2)
        this.CheckCalcSigma0(4, 2, 2, 3)
        this.CheckCalcSigma0(5, 2, 2, 3, 2)
        this.CheckCalcSigma0(6, 2, 2, 3, 2, 4)
        this.CheckCalcSigma0(7, 2, 2, 3, 2, 4, 2)
        this.CheckCalcSigma0(8, 2, 2, 3, 2, 4, 2, 4)
        this.CheckCalcSigma0(9, 2, 2, 3, 2, 4, 2, 4, 3)
        this.CheckCalcSigma0(10, 2, 2, 3, 2, 4, 2, 4, 3, 4)
        this.CheckCalcSigma0(11, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2)
        this.CheckCalcSigma0(12, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6)
        this.CheckCalcSigma0(13, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2)
        this.CheckCalcSigma0(14, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4)
        this.CheckCalcSigma0(15, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4)
        this.CheckCalcSigma0(16, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5)

    [<Test>]
    member public this.CalcSigma1() =
        this.CheckCalcSigma1(2, 3)
        this.CheckCalcSigma1(3, 3, 4)
        this.CheckCalcSigma1(4, 3, 4, 7)
        this.CheckCalcSigma1(5, 3, 4, 7, 6)
        this.CheckCalcSigma1(6, 3, 4, 7, 6, 12)
        this.CheckCalcSigma1(7, 3, 4, 7, 6, 12, 8)
        this.CheckCalcSigma1(8, 3, 4, 7, 6, 12, 8, 15)
        this.CheckCalcSigma1(9, 3, 4, 7, 6, 12, 8, 15, 13)
        this.CheckCalcSigma1(10, 3, 4, 7, 6, 12, 8, 15, 13, 18)
        this.CheckCalcSigma1(11, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12)
        this.CheckCalcSigma1(12, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28)
        this.CheckCalcSigma1(13, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14)
        this.CheckCalcSigma1(14, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24)
        this.CheckCalcSigma1(15, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24, 24)
        this.CheckCalcSigma1(16, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24, 24, 31)

    [<Test>]
    member public this.CalcEulerTotientFunction() =
        this.CheckCalcEulerTotientFunction(2, 1)
        this.CheckCalcEulerTotientFunction(3, 1, 2)
        this.CheckCalcEulerTotientFunction(4, 1, 2, 2)
        this.CheckCalcEulerTotientFunction(5, 1, 2, 2, 4)
        this.CheckCalcEulerTotientFunction(6, 1, 2, 2, 4, 2)
        this.CheckCalcEulerTotientFunction(7, 1, 2, 2, 4, 2, 6)
        this.CheckCalcEulerTotientFunction(8, 1, 2, 2, 4, 2, 6, 4)
        this.CheckCalcEulerTotientFunction(9, 1, 2, 2, 4, 2, 6, 4, 6)
        this.CheckCalcEulerTotientFunction(10, 1, 2, 2, 4, 2, 6, 4, 6, 4)
        this.CheckCalcEulerTotientFunction(11, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10)
        this.CheckCalcEulerTotientFunction(12, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4)
        this.CheckCalcEulerTotientFunction(13, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12)
        this.CheckCalcEulerTotientFunction(14, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6)
        this.CheckCalcEulerTotientFunction(15, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8)
        this.CheckCalcEulerTotientFunction(16, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8)
        this.CheckCalcEulerTotientFunction(17, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16)
        this.CheckCalcEulerTotientFunction(18, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6)
        this.CheckCalcEulerTotientFunction(19, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18)

    member private this.CheckIsPrime(maxNumber: int, [<ParamArray>] expectedValues: bool[]) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, 2 + shift |> sieve.IsPrime))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, shift |> int64 |> (+) 2L |> sieve.IsPrime))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, shift |> bigint |> (+) 2I |> sieve.IsPrime))

    member private this.CheckGetNextPrime(maxNumber: int, [<ParamArray>] expectedValues: int option[]) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, 2 + shift |> sieve.GetNextPrime))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> convertOptionValue int64, shift |> int64 |> (+) 2L |> sieve.GetNextPrime))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> convertOptionValue bigint, shift |> bigint |> (+) 2I |> sieve.GetNextPrime))

    member private this.CheckGetItem(maxNumber: int, [<ParamArray>] expectedValues: int[]) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[-1] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[0] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[1] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[sieve.MaxNumber + 1] |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, sieve.[2 + shift]))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[-1L] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[0L] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[1L] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[sieve.MaxNumber |> int64 |> (+) 1L] |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> int64, sieve.[shift |> int64 |> (+) 2L]))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[-1I] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[0I] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[1I] |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.[sieve.MaxNumber |> bigint |> (+) 1I] |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> bigint, sieve.[shift |> bigint |> (+) 2I]))

    member private this.CheckCalcSigma0(maxNumber: int, [<ParamArray>] expectedValues: int[]) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, 2 + shift |> sieve.CalcSigma0))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> int64, shift |> int64 |> (+) 2L |> sieve.CalcSigma0))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma0(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> bigint, shift |> bigint |> (+) 2I |> sieve.CalcSigma0))

    member private this.CheckCalcSigma1(maxNumber: int, [<ParamArray>] expectedValues: int[]) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, 2 + shift |> sieve.CalcSigma1))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> int64, shift |> int64 |> (+) 2L |> sieve.CalcSigma1))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcSigma1(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> bigint, shift |> bigint |> (+) 2I |> sieve.CalcSigma1))

    member private this.CheckCalcEulerTotientFunction(maxNumber: int, [<ParamArray>] expectedValues: int[]) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue, 2 + shift |> sieve.CalcEulerTotientFunction))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> int64, shift |> int64 |> (+) 2L |> sieve.CalcEulerTotientFunction))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.CalcEulerTotientFunction(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> ClassicAssert.AreEqual(expectedValue |> bigint, shift |> bigint |> (+) 2I |> sieve.CalcEulerTotientFunction))