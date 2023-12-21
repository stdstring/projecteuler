namespace CommonLibPerformanceTests

open NUnit.Framework
open CommonLib
open CommonLibPerformanceTests.Utils

[<TestFixture>]
[<Category("Performance")>]
type PythagoreanTriplesGeneratorTests() =

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    [<TestCase(100000)>]
    [<TestCase(1000000)>]
    [<TestCase(10000000)>]
    member public this.GeneratePrimitiveWithPerimeterSelector(perimeter: int) =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X + triple.Y + triple.Z)
        (fun () -> generator.GeneratePrimitiveTriples(perimeter) |> ignore) |> MeasureUtils.MeasureExecutionTime |> printf "Execution time = %d ms"

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    [<TestCase(100000)>]
    [<TestCase(1000000)>]
    [<TestCase(10000000)>]
    member public this.GenerateWithPerimeterSelector(perimeter: int) =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X + triple.Y + triple.Z)
        (fun () -> generator.GenerateTriples(perimeter) |> ignore) |> MeasureUtils.MeasureExecutionTime |> printf "Execution time = %d ms"
