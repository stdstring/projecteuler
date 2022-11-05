namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System.Collections.Generic
open Checked
open CommonLib

module Problem098Impl =
    type SquareNumbersData = {Numbers: ResizeArray<int64>[]; NumbersDict: IDictionary<int64, int list>}

open Problem098Impl

[<TestFixture>]
type Problem098() =

    let rec fillSquareNumbersData (leftBorder: int64) (rightBorder: int64) (index: int) (number: int64) (maxNumber: int64) (data: SquareNumbersData) =
        match number * number with
        | square when square >= maxNumber -> ()
        | square when square >= rightBorder ->
            data |> fillSquareNumbersData rightBorder (10L * rightBorder) (index + 1) number maxNumber
        | square ->
            data.Numbers.[index].Add(square)
            data.NumbersDict.Add(square, square |> NumbersDigits.GetDigits)
            data |> fillSquareNumbersData leftBorder rightBorder index (number + 1L) maxNumber

    let createSquareNumbersData (maxWordSize: int) =
        let data = {SquareNumbersData.Numbers = Array.init maxWordSize (fun _ -> new ResizeArray<int64>()); SquareNumbersData.NumbersDict = new Dictionary<int64, int list>()}
        let maxNumber = pown 10L maxWordSize
        data |> fillSquareNumbersData 1L 10L 0 1L maxNumber
        data

    let rec createMatchData (word: string) (digits: int list) =
        let matchData = new Dictionary<char, int>()
        let digitData =  new Dictionary<int, char>()
        let rec createMatchDataImpl (chars: char list) (digits: int list) =
            match chars, digits with
            | [], [] -> matchData :> IDictionary<char, int> |> Some
            | ch :: _, digit :: _ when matchData.ContainsKey(ch) && matchData.[ch] <> digit -> None
            | ch :: _, digit :: _ when digitData.ContainsKey(digit) && digitData.[digit] <> ch -> None
            | ch :: charsRest, digit :: digitsRest ->
                matchData.[ch]<-digit
                digitData.[digit]<-ch
                createMatchDataImpl charsRest digitsRest
            | _ -> failwith "Unexpected branch of match expression"
        createMatchDataImpl (word |> Seq.toList) digits

    let createNumber (word: string) (matchData: IDictionary<char, int>) =
        word |> Seq.fold (fun number ch -> matchData.[ch] |> int64 |> (+) (10L * number)) 0L

    let processWords (word1: string) (word2: string) (squareNumbersData: SquareNumbersData) =
        let chooseFun (number: int64) =
            match createMatchData word1 squareNumbersData.NumbersDict.[number] with
            | None -> None
            | Some matchData when matchData.[word2.[0]] = 0 -> None
            | Some matchData ->
                let number2 = matchData |> createNumber word2
                match number2 |> squareNumbersData.NumbersDict.ContainsKey with
                | false -> None
                | true -> [number; number2] |> Some
        squareNumbersData.Numbers.[word1.Length - 1] |> Seq.choose chooseFun |> Seq.concat

    let solveImpl (dataFilename: string) =
        let words = File.ReadAllText(Path.Combine("Data", dataFilename)).Split(',') |> Array.map (fun word -> word.Trim('\"'))
        let wordsStorage = new Dictionary<string, ResizeArray<string>>()
        let iterFun (word: string) =
            let key = System.String.Join("", word |> Seq.sort)
            if key |> wordsStorage.ContainsKey |> not then
                wordsStorage.Add(key, new ResizeArray<string>())
            wordsStorage.[key].Add(word)
        words |> Array.iter iterFun
        let wordPairs = wordsStorage |> Seq.filter (fun kvPair -> kvPair.Value.Count = 2) |> Seq.map (fun kvPair -> kvPair.Value.[0], kvPair.Value.[1])
        let squareNumbersData = wordPairs |> Seq.map (fun pair -> (fst pair).Length) |> Seq.max |> createSquareNumbersData
        wordPairs |> Seq.map (fun (word1, word2) -> squareNumbersData |> processWords word1 word2) |> Seq.concat |> Seq.max |> int

    [<TestCase("problem_098.dat", 18769, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)
