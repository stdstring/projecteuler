namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

[<TestFixture>]
type Problem345() =

    let containsColumn (index: int) (column: int) =
        index &&& (1 <<< column) > 0

    let appendColumn (index: int) (column: int) =
        index ||| (1 <<< column)

    let solveImpl (matrix: int[,]) =
        let size = 0 |> matrix.GetLength
        let rec processRow (storage: int[]) (row: int) (prevIndicies: ISet<int>) =
            match row with
            | _ when row = size -> ()
            | _ ->
                let currentIndicies = HashSet<int>() :> ISet<int>
                for index in prevIndicies do
                    let value = storage.[index]
                    for column in 0 .. size - 1 do
                        if column |> containsColumn index |> not then
                            let newIndex = column |> appendColumn index
                            storage.[newIndex] <- max storage.[newIndex] (value + matrix.[row, column])
                            newIndex |> currentIndicies.Add |> ignore
                currentIndicies |> processRow storage (row + 1)
        let storageSize = 1 <<< size
        let storage = storageSize |> Array.zeroCreate
        seq {0 .. size - 1} |> Seq.iter (fun column -> let index = (1 <<< column) in storage.[index] <- matrix.[0, column])
        seq {0 .. size - 1} |> Seq.map (fun column -> 1 <<< column) |> HashSet<int> |> processRow storage 1
        storage.[storageSize - 1]

    [<TestCase(3315, TimeThresholds.HardTimeLimit)>]
    member public this.SolveExample(expectedAnswer: int, timeLimit: int) =
        let matrix = array2D [[7; 53; 183; 439; 863]; [497; 383; 563; 79; 973]; [287; 63; 343; 169; 583]; [627; 343; 773; 959; 943]; [767; 473; 103; 699; 303]]
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, matrix)

    [<TestCase(13938, TimeThresholds.HardTimeLimit)>]
    member public this.SolveProblem(expectedAnswer: int, timeLimit: int) =
        let matrix = array2D [[7; 53; 183; 439; 863; 497; 383; 563; 79; 973; 287; 63; 343; 169; 583];
                              [627; 343; 773; 959; 943; 767; 473; 103; 699; 303; 957; 703; 583; 639; 913];
                              [447; 283; 463; 29; 23; 487; 463; 993; 119; 883; 327; 493; 423; 159; 743];
                              [217; 623; 3; 399; 853; 407; 103; 983; 89; 463; 290; 516; 212; 462; 350];
                              [960; 376; 682; 962; 300; 780; 486; 502; 912; 800; 250; 346; 172; 812; 350];
                              [870; 456; 192; 162; 593; 473; 915; 45; 989; 873; 823; 965; 425; 329; 803];
                              [973; 965; 905; 919; 133; 673; 665; 235; 509; 613; 673; 815; 165; 992; 326];
                              [322; 148; 972; 962; 286; 255; 941; 541; 265; 323; 925; 281; 601; 95; 973];
                              [445; 721; 11; 525; 473; 65; 511; 164; 138; 672; 18; 428; 154; 448; 848];
                              [414; 456; 310; 312; 798; 104; 566; 520; 302; 248; 694; 976; 430; 392; 198];
                              [184; 829; 373; 181; 631; 101; 969; 613; 840; 740; 778; 458; 284; 760; 390];
                              [821; 461; 843; 513; 17; 901; 711; 993; 293; 157; 274; 94; 192; 156; 574];
                              [34; 124; 4; 878; 450; 476; 712; 914; 838; 669; 875; 299; 823; 329; 699];
                              [815; 559; 813; 459; 522; 788; 168; 586; 966; 232; 308; 833; 251; 631; 107];
                              [813; 883; 451; 509; 615; 77; 281; 613; 459; 205; 380; 274; 302; 35; 805]]
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, matrix)
