namespace Sudoku.Core

open System;
open System.Collections.Generic;

type public Solver() =

    member public this.Solve(source: Grid) =
        let predictionStack = new Stack<PredictionContext.Context>();
        let rec solveImpl (calculationContext: CalculationContext.Context) =
            match calculationContext |> Calculation.calculate with
            | Calculation.CalculationResult.Stop -> predictionStack |> Prediction.applyNextPrediction |> CalculationContext.create |> solveImpl
            | Calculation.CalculationResult.Finish calculationContextAfter -> calculationContextAfter.grid
            | Calculation.CalculationResult.NeedPrediction calculationContextAfter -> calculationContextAfter |> Prediction.createPrediction predictionStack |> CalculationContext.create |> solveImpl
        source |> CalculationContext.create |> solveImpl