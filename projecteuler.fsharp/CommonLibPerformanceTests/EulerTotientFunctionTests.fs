namespace CommonLibPerformanceTests

open CommonLib
open NUnit.Framework
open CommonLibPerformanceTests.Utils

[<TestFixture>]
[<Category("Performance")>]
type EulerTotientFunctionTests() =

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    [<TestCase(100000)>]
    [<TestCase(1000000)>]
    [<TestCase(10000000)>]
    [<TestCase(100000000)>]
    //[<TestCase(250000000)>] - executes more than 1 minute
    member public this.CreateSieve(maxNumber: int) =
        (fun () -> EulerTotientFunction.Create(maxNumber) |> ignore) |> MeasureUtils.MeasureExecutionTime |> ignore