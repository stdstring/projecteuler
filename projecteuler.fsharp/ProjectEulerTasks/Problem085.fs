namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles (see https://projecteuler.net/problem=85).
// Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.

module Problem085Impl =
    type SearchRange = {Left: int; Right: int}
    type SearchResult = {Area: int; RectanglesCount: int}

open Problem085Impl

[<TestFixture>]
type Problem085() =

    let calcRectanglesCount (width: int) (height: int) =
        let area = width * height
        area * (area + width + height + 1) / 4

    let rec findSearchRange (rectanglesCount: int) (searchRange: SearchRange) (rectanglesCountGenerator: int->int) (nextBorderGenerator: int->int) =
        let leftRectanglesCount = searchRange.Left |> rectanglesCountGenerator
        let rightRectanglesCount = searchRange.Right |> rectanglesCountGenerator
        match rectanglesCount with
        | _ when (leftRectanglesCount < rectanglesCount) && (rightRectanglesCount > rectanglesCount) -> searchRange;
        | _ when leftRectanglesCount = rectanglesCount -> {searchRange with Right = searchRange.Left}
        | _ when rightRectanglesCount = rectanglesCount -> {searchRange with Left = searchRange.Right}
        | _ ->
            let nextSearchRange = {SearchRange.Left = searchRange.Right; SearchRange.Right = searchRange.Right |> nextBorderGenerator}
            findSearchRange rectanglesCount nextSearchRange rectanglesCountGenerator nextBorderGenerator

    let findNearestRectangleValue (rectanglesCount: int) (currentArea: int) (searchResult: SearchResult) =
        let foldFun (currentSearchResult: SearchResult) (divider: int) =
            let width, height = divider, currentArea / divider
            match calcRectanglesCount width height with
            | currentRectanglesCount when abs (currentRectanglesCount - rectanglesCount) < abs (currentSearchResult.RectanglesCount - rectanglesCount) ->
                {SearchResult.Area = currentArea; SearchResult.RectanglesCount = currentRectanglesCount}
            | _ -> currentSearchResult
        currentArea |> NumbersDividers.GetDividers |> List.fold foldFun searchResult

    let findNearestValue (minArea: int) (maxArea: int) (rectanglesCount: int) =
        let initSearchResult = {SearchResult.Area = 0; SearchResult.RectanglesCount = 0}
        let searchResult = seq {minArea .. maxArea} |> Seq.fold (fun searchResult area -> searchResult |> findNearestRectangleValue rectanglesCount area) initSearchResult
        searchResult.Area

    let rec findLineBottomValue (rectanglesCount: int) (searchRange: SearchRange) =
        match searchRange with
        | _ when searchRange.Left = searchRange.Right -> searchRange.Left
        | _ when (searchRange.Left + 1) = searchRange.Right ->
            match calcRectanglesCount searchRange.Left 1 with
            | leftRectanglesCount when leftRectanglesCount >= rectanglesCount -> searchRange.Left
            | _ -> searchRange.Right
        | _ ->
            let middle = (searchRange.Left + searchRange.Right) / 2
            match calcRectanglesCount middle 1 with
            | middleRectanglesCount when middleRectanglesCount < rectanglesCount -> {searchRange with Left = middle} |> findLineBottomValue rectanglesCount
            | middleRectanglesCount when middleRectanglesCount > rectanglesCount -> {searchRange with Right = middle} |> findLineBottomValue rectanglesCount
            | middleRectanglesCount when middleRectanglesCount = rectanglesCount -> middle
            | _ -> failwith "Unexpected branch of match expression"

    let findBottomEval (rectanglesCount: int) =
        let initSearchRange = {SearchRange.Left = 1; SearchRange.Right = 10}
        let searchRange = findSearchRange rectanglesCount initSearchRange (fun width -> calcRectanglesCount width 1) (fun rightBorder -> rightBorder * 10)
        let lineMinWidth = findLineBottomValue rectanglesCount searchRange
        lineMinWidth * 1

    let findTopEval (rectanglesCount: int) =
        let initSearchRange = {SearchRange.Left = 1; SearchRange.Right = 2}
        let searchRange = findSearchRange rectanglesCount initSearchRange (fun width -> calcRectanglesCount width width) (fun rightBorder -> rightBorder + 1)
        searchRange.Right * searchRange.Right

    let solveImpl (rectanglesCount: int) =
        // Math:
        // Let we have the rectangle N * M, where min(N, M) = M; S = N * M - area value of this rectangle
        // Let C(A, B) - quantity of the unique rectangles A * B, which can be situated inside the source rectangle N * M (unique ?)
        // Let S(N) - sum of an arithmetic progression from 1 to N
        // Than we have the following relations:
        // C(1, 1) = N * M
        // C(2, 1) = (N - 1) * M
        // C(3, 1) = (N - 2) * M
        // ...
        // C(N, 1) = 1 * M
        // C(1, 1) + C(2, 1) + ... + C(N, 1) = M * (N + (N - 1) + ... + 1) = M * S(N) - total quantity of unique rectangles kind A * 1 where A = 1 .. N
        // C(2, 1) = N * (M - 1)
        // C(2, 2) = (N - 1) * (M - 1)
        // ...
        // C(N, 2) = 1 * (M - 1)
        // C(2, 1) + C(2, 2) + ... + C(N, 2) = (M - 1) * (N + (N - 1) + ... + 1) = (M - 1) * S(N) - total quantity of unique rectangles kind A * 2 where A = 1 .. N
        // ...
        // M * S(N) + (M - 1) * S(N) + ... 1 * S(N) = S(N) * (M + (M - 1) + ... + 1) = S(N) * S(M) - total quantity of all unique rectangles
        // S(N) = N * (N + 1) / 2 = (N^2 + N) / 2
        // S(N) * S(M) = (N^2 + N) * (M^2 + M) / 4 = (N^2 * M^2 + N * M^2 + N^2 * M + N * M) / 4 = (S^2 + S * (N + M + 1)) / 4
        // We can see, that S(N) * S(M) = A * (N + M) + B, where A, B - some constants for some giving area value S
        // Since S = N * M => S(N) * S(M) = A * (N + S / N) + B
        // Let we see the following function f(X) = A * (X + S / X) + B
        // f'(X) = A * (1 - S / X^2), consider f'(X) = 0 => 1 - S / X^2 = 0 => S / X^2 = 1 => X^2 = S => X = S^(1/2)
        // Anyone can see, that at X = S^(1/2) function f(X) has the minimum.
        // And anyone can see, that at X = S (or at X = 1) function f(X) has the maximum on [1, S] segment.

        let minArea = findBottomEval rectanglesCount
        let maxArea = findTopEval rectanglesCount
        findNearestValue minArea maxArea rectanglesCount

    [<TestCase(2000000, 2772, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(rectanglesCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, rectanglesCount)