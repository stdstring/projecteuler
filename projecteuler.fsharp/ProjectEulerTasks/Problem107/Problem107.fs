namespace ProjectEulerTasks

open CommonLib.Collections
open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

module Problem107Impl =
    type Edge = {NodeA: int; NodeB: int; Weight: int}

    type Node = {NodeA: int; Edges: Edge list}

open Problem107Impl

[<TestFixture>]
type Problem107() =

    [<Literal>]
    let EdgeAbsenceValue = "-"

    let parseRowData (rowIndex: int) (rowData: string) =
        let columnsData = rowData.Split(",")
        seq {0 .. columnsData.Length - 1} |>
        Seq.filter (fun index -> columnsData.[index] <> EdgeAbsenceValue) |>
        Seq.map (fun index -> {Edge.NodeA = rowIndex; NodeB = index; Weight = columnsData.[index] |> int})

    let parseIntoEdgeRepresentation (rowsData: string[]) =
        seq {0 .. rowsData.Length - 1} |> Seq.map (fun index -> rowsData.[index] |> parseRowData index) |> Seq.concat |> Seq.filter (fun edge -> edge.NodeA < edge.NodeB)

    let parseIntoNodeRepresentation (rowsData: string[]) =
        seq {0 .. rowsData.Length - 1} |> Seq.map (fun index -> {Node.NodeA = index; Node.Edges = rowsData.[index] |> parseRowData index |> Seq.toList}) |> Seq.toArray

    // TODO (std_string) : think about movint into CommonLib
    // Kruskal's algorithm
    let kruskalImpl (nodesCount: int) (sourceEdges: seq<Edge>) =
        // TODO (std_string) : think about movint into CommonLib
        // disjoint-set or union–find data structure
        let link = [|0 .. nodesCount - 1|]
        let size = Array.create nodesCount 1
        let rec find (node: int) =
            match node = link.[node] with
            | true -> node
            | false ->
                link.[node] <- (find link.[node])
                link.[node]
        let same (edge: Edge) = (find edge.NodeA) = (find edge.NodeB)
        let unite (edge: Edge) =
            let a = edge.NodeA |> find
            let b = edge.NodeB |> find
            match size.[a] < size.[b] with
            | true ->
                size.[b] <- size.[b] + size.[a]
                link.[a] <- b
            | false ->
                size.[a] <- size.[a] + size.[b]
                link.[b] <- a
        // implementation
        let sourceEdges = sourceEdges |> Seq.sortBy (fun edge -> edge.Weight)
        let destEdges = new ResizeArray<Edge>()
        for sourceEdge in sourceEdges do
            if sourceEdge |> same |> not then
                sourceEdge |> unite
                destEdges.Add(sourceEdge)
        destEdges :> seq<Edge>

    let solveUsingKruskalAlgorithm (rowsData: string[]) =
        let calcSum (edges: seq<Edge>) = edges |> Seq.sumBy (fun edge -> edge.Weight)
        let nodesCount = rowsData.Length
        let sourceEdges = rowsData |> parseIntoEdgeRepresentation
        let sourceSum = sourceEdges |> calcSum
        let destEdges = sourceEdges |> kruskalImpl nodesCount
        let destSum = destEdges |> calcSum
        sourceSum - destSum

    // TODO (std_string) : think about movint into CommonLib
    // Prim's algorithm
    let primImpl (sourceNodes: Node[]) =
        let destNodes = sourceNodes |> Array.map (fun node -> {Node.NodeA = node.NodeA; Node.Edges = []})
        let usedNodes = Array.create sourceNodes.Length false
        let minPriorityQueue = MinPriorityQueue<Edge>(fun (left, right) -> left.Weight - right.Weight)
        let addEdgesIntoQueue (node: Node) =
            node.Edges |> Seq.iter (fun edge -> edge |> minPriorityQueue.Insert)
        let addEdge (edge: Edge) =
            let reverseEdge = {Edge.NodeA = edge.NodeB; Edge.NodeB = edge.NodeA; Edge.Weight = edge.Weight}
            destNodes.[edge.NodeA] <- {destNodes.[edge.NodeA] with Edges = edge :: destNodes.[edge.NodeA].Edges}
            destNodes.[edge.NodeB] <- {destNodes.[edge.NodeB] with Edges = reverseEdge :: destNodes.[edge.NodeB].Edges}
        usedNodes.[0]<-true
        sourceNodes.[0] |> addEdgesIntoQueue
        while minPriorityQueue.IsEmpty |> not do
            let edge = minPriorityQueue.ExtractMin()
            if usedNodes.[edge.NodeB] |> not then
                usedNodes.[edge.NodeB] <- true
                edge |> addEdge
                sourceNodes.[edge.NodeB] |> addEdgesIntoQueue
        destNodes

    let solveUsingPrimAlgorithm (rowsData: string[]) =
        let calcSum (nodes: Node[]) = (nodes |> Seq.map (fun node -> node.Edges |> Seq.sumBy (fun edge -> edge.Weight)) |> Seq.sum) / 2
        let sourceNodes = rowsData |> parseIntoNodeRepresentation
        let sourceSum = sourceNodes |> calcSum
        let destNodes = sourceNodes |> primImpl
        let destSum = destNodes |> calcSum
        sourceSum - destSum

    let solveImpl (dataFilename: string) (algorithmImpl: string[] -> int) =
        let content = File.ReadAllText(Path.Combine("Data", dataFilename))
        content.Split("\r\n") |> algorithmImpl

    [<TestCase("problem_107_example.dat", 150, TimeThresholds.HardTimeLimit)>]
    [<TestCase("problem_107.dat", 259679, TimeThresholds.HardTimeLimit)>]
    member public this.SolveUsingKruskalAlgorithm(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename, solveUsingKruskalAlgorithm)

    [<TestCase("problem_107_example.dat", 150, TimeThresholds.HardTimeLimit)>]
    [<TestCase("problem_107.dat", 259679, TimeThresholds.HardTimeLimit)>]
    member public this.SolveUsingPrimAlgorithm(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename, solveUsingPrimAlgorithm)