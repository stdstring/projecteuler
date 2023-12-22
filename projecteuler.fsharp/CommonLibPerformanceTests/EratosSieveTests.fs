namespace CommonLibPerformanceTests

open CommonLib
open NUnit.Framework
open CommonLibPerformanceTests.Utils

[<TestFixture>]
[<Category("Performance")>]
type EratosSieveTests() =

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    [<TestCase(100000)>]
    [<TestCase(1000000)>]
    [<TestCase(10000000)>]
    [<TestCase(100000000)>]
    [<TestCase(1000000000)>]
    member public this.CreateSieve(maxNumber: int) =
        (fun () -> EratosSieve.Create(maxNumber) |> ignore) |> MeasureUtils.MeasureExecutionTime |> printf "Execution time = %d ms"

[<TestFixture>]
[<Category("Performance")>]
type EratosSieveWithSmallestPrimeFactorsTests() =

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    [<TestCase(100000)>]
    [<TestCase(1000000)>]
    [<TestCase(10000000)>]
    [<TestCase(100000000)>]
    [<TestCase(500000000)>]
    member public this.CreateSieve(maxNumber: int) =
        (fun () -> EratosSieveWithSmallestPrimeFactors.Create(maxNumber) |> ignore) |> MeasureUtils.MeasureExecutionTime |> printf "Execution time = %d ms"
