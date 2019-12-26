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
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> EratosSieve.Create(1000000001) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(2) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(3) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(10) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(19) |> Assert.IsNotNull)
        Assert.DoesNotThrow(fun() -> EratosSieve.Create(100) |> Assert.IsNotNull)

    [<Test>]
    member public this.IsPrime() =
        this.CheckIsPrime(EratosSieve.Create(2), true)
        this.CheckIsPrime(EratosSieve.Create(3), true, true)
        this.CheckIsPrime(EratosSieve.Create(4), true, true, false)
        this.CheckIsPrime(EratosSieve.Create(5), true, true, false, true)
        this.CheckIsPrime(EratosSieve.Create(6), true, true, false, true, false)
        this.CheckIsPrime(EratosSieve.Create(7), true, true, false, true, false, true)
        this.CheckIsPrime(EratosSieve.Create(8), true, true, false, true, false, true, false)
        this.CheckIsPrime(EratosSieve.Create(9), true, true, false, true, false, true, false, false)
        this.CheckIsPrime(EratosSieve.Create(10), true, true, false, true, false, true, false, false, false)
        this.CheckIsPrime(EratosSieve.Create(11), true, true, false, true, false, true, false, false, false, true)
        this.CheckIsPrime(EratosSieve.Create(12), true, true, false, true, false, true, false, false, false, true, false)
        this.CheckIsPrime(EratosSieve.Create(13), true, true, false, true, false, true, false, false, false, true, false, true)
        this.CheckIsPrime(EratosSieve.Create(14), true, true, false, true, false, true, false, false, false, true, false, true, false)
        this.CheckIsPrime(EratosSieve.Create(15), true, true, false, true, false, true, false, false, false, true, false, true, false, false)

    [<Test>]
    member public this.GetNextPrime() =
        this.CheckGetNextPrime(EratosSieve.Create(2), None)
        this.CheckGetNextPrime(EratosSieve.Create(3), Some 3, None)
        this.CheckGetNextPrime(EratosSieve.Create(4), Some 3, None, None)
        this.CheckGetNextPrime(EratosSieve.Create(5), Some 3, Some 5, Some 5, None)
        this.CheckGetNextPrime(EratosSieve.Create(6), Some 3, Some 5, Some 5, None, None)
        this.CheckGetNextPrime(EratosSieve.Create(7), Some 3, Some 5, Some 5, Some 7, Some 7, None)
        this.CheckGetNextPrime(EratosSieve.Create(8), Some 3, Some 5, Some 5, Some 7, Some 7, None, None)
        this.CheckGetNextPrime(EratosSieve.Create(9), Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None)
        this.CheckGetNextPrime(EratosSieve.Create(10), Some 3, Some 5, Some 5, Some 7, Some 7, None, None, None, None)
        this.CheckGetNextPrime(EratosSieve.Create(11), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None)
        this.CheckGetNextPrime(EratosSieve.Create(12), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, None, None)
        this.CheckGetNextPrime(EratosSieve.Create(13), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None)
        this.CheckGetNextPrime(EratosSieve.Create(14), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None)
        this.CheckGetNextPrime(EratosSieve.Create(15), Some 3, Some 5, Some 5, Some 7, Some 7, Some 11, Some 11, Some 11, Some 11, Some 13, Some 13, None, None, None)

    [<Test>]
    member public this.ToSeq() =
        Assert.AreEqual([2], EratosSieve.Create(2).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3], EratosSieve.Create(3).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3], EratosSieve.Create(4).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5], EratosSieve.Create(5).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5], EratosSieve.Create(6).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], EratosSieve.Create(7).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], EratosSieve.Create(8).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], EratosSieve.Create(9).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7], EratosSieve.Create(10).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11], EratosSieve.Create(11).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11], EratosSieve.Create(12).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieve.Create(13).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieve.Create(14).ToSeq() |> Seq.toList)
        Assert.AreEqual([2; 3; 5; 7; 11; 13], EratosSieve.Create(15).ToSeq() |> Seq.toList)

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
