namespace CommonLibPerformanceTests.Utils

open System.Diagnostics

[<AbstractClass; Sealed>]
type MeasureUtils =

    static member public MeasureExecutionTime (operation: unit -> unit) =
        let stopwatch = Stopwatch.StartNew()
        operation ()
        stopwatch.Stop()
        stopwatch.ElapsedMilliseconds
