namespace Sudoku.Core

open System.Collections.Generic;

module Prediction=
    let applyNextPrediction (predictionStack: Stack<PredictionContext.Context>) =
        while predictionStack.Peek().freeCells.Count = 0 do predictionStack.Pop() |> ignore
        let predictContext = predictionStack.Peek()
        let cell = predictContext.freeCells.Dequeue()
        let grid = predictContext.grid.Clone()
        grid.[cell] <- predictContext.number
        grid

    let createPrediction (predictionStack: Stack<PredictionContext.Context>) (calculationContext: CalculationContext.Context) =
        let predictContext = calculationContext.grid |> PredictionContext.create
        predictContext |> predictionStack.Push
        predictionStack |> applyNextPrediction
