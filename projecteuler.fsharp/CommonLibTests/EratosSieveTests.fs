namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib

[<TestFixture>]
type EratosSieveTests() =

    // TODO (std_string) : think about moving into the common utils
    let convertOptionValue (converter: 'TInput -> 'TOutput) (optionValue: 'TInput option) =
        match optionValue with
        | None -> None
        | Some value -> value |> converter |> Some

    [<Test>]
    member public this.CreateSieve() =
        let sieveBuilder = EratosSieveBuilder()
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieveBuilder.CreateSieve(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieveBuilder.CreateSieve(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieveBuilder.CreateSieve(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieveBuilder.CreateSieve(1000000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> sieveBuilder.CreateSieve(2) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> sieveBuilder.CreateSieve(3) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> sieveBuilder.CreateSieve(10) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> sieveBuilder.CreateSieve(19) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> sieveBuilder.CreateSieve(100) |> Assert.IsNotNull)

    [<Test>]
    member public this.IsPrime() =
        let sieveBuilder = EratosSieveBuilder()
        this.CheckIsPrime(sieveBuilder.CreateSieve(2), true)
        this.CheckIsPrime(sieveBuilder.CreateSieve(3), true, true)
        this.CheckIsPrime(sieveBuilder.CreateSieve(4), true, true, false)
        this.CheckIsPrime(sieveBuilder.CreateSieve(5), true, true, false, true)
        this.CheckIsPrime(sieveBuilder.CreateSieve(6), true, true, false, true, false)
        this.CheckIsPrime(sieveBuilder.CreateSieve(7), true, true, false, true, false, true)
        this.CheckIsPrime(sieveBuilder.CreateSieve(8), true, true, false, true, false, true, false)
        this.CheckIsPrime(sieveBuilder.CreateSieve(9), true, true, false, true, false, true, false, false)
        this.CheckIsPrime(sieveBuilder.CreateSieve(10), true, true, false, true, false, true, false, false, false)
        this.CheckIsPrime(sieveBuilder.CreateSieve(11), true, true, false, true, false, true, false, false, false, true)
        this.CheckIsPrime(sieveBuilder.CreateSieve(12), true, true, false, true, false, true, false, false, false, true, false)
        this.CheckIsPrime(sieveBuilder.CreateSieve(13), true, true, false, true, false, true, false, false, false, true, false, true)
        this.CheckIsPrime(sieveBuilder.CreateSieve(14), true, true, false, true, false, true, false, false, false, true, false, true, false)
        this.CheckIsPrime(sieveBuilder.CreateSieve(15), true, true, false, true, false, true, false, false, false, true, false, true, false, false)

    [<Test>]
    member public this.GetNextPrime() =
        let sieveBuilder = EratosSieveBuilder()
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(2), None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(3), Some 3, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(4), Some 3, None, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(5), Some 3, Some 5, Some 5, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(6), Some 3, Some 5, Some 5, None, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(7), Some 3, Some 5, Some 5, Some 7, Some 7, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(8), Some 3, Some 5, Some 5, Some 7, Some 7, None, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(9), Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(10), Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(11), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(12), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(13), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(14), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None)
        this.CheckGetNextPrime(sieveBuilder.CreateSieve(15), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None, None)

    [<Test>]
    member public this.ToSeq() =
        let sieveBuilder = EratosSieveBuilder()
        Assert.AreEqual([2], sieveBuilder.CreateSieve(2).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3], sieveBuilder.CreateSieve(3).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3], sieveBuilder.CreateSieve(4).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5], sieveBuilder.CreateSieve(5).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5], sieveBuilder.CreateSieve(6).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], sieveBuilder.CreateSieve(7).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], sieveBuilder.CreateSieve(8).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], sieveBuilder.CreateSieve(9).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], sieveBuilder.CreateSieve(10).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11], sieveBuilder.CreateSieve(11).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11], sieveBuilder.CreateSieve(12).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], sieveBuilder.CreateSieve(13).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], sieveBuilder.CreateSieve(14).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], sieveBuilder.CreateSieve(15).ToSeq() |> Seq.toList)

    member private this.CheckIsPrime(sieve: EratosSieve, [<ParamArray>] expectedValues: bool[]) =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, sieve.IsPrime(2 + shift)))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, shift |> int64 |> (+) 2L |> sieve.IsPrime))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.IsPrime(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, shift |> bigint |> (+) 2I |> sieve.IsPrime))

    member private this.CheckGetNextPrime(sieve: EratosSieve, [<ParamArray>] expectedValues: int option[]) =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber + 1) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue, sieve.GetNextPrime(2 + shift)))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber |> int64 |> (+) 1L) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue |> convertOptionValue int64, shift |> int64 |> (+) 2L |> sieve.GetNextPrime))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> sieve.GetNextPrime(sieve.MaxNumber |> bigint |> (+) 1I) |> ignore) |> ignore
        expectedValues |> Seq.iteri (fun shift expectedValue -> Assert.AreEqual(expectedValue |> convertOptionValue bigint, shift |> bigint |> (+) 2I |> sieve.GetNextPrime))
