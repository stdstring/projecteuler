namespace CommonLib

open System

type PellSolution =
    struct
        val X: bigint
        val Y: bigint
        new(x: bigint, y: bigint) = { X = x; Y = y }
    end

[<AbstractClass; Sealed>]
type PellEquation =

    static member public FindFirstSolution(d: int, c: int) =
        PellEquation.FindFirstSolution(d |> bigint, c |> bigint)

    static member public FindFirstSolution(d: int64, c: int64) =
        PellEquation.FindFirstSolution(d |> bigint, c |> bigint)

    // Algorithm (from http://mathworld.wolfram.com/PellEquation.html):
    // Pell equation of the form x^2 - D * y^2 = 1 can be solved by finding the continued fraction [a(0), a(1), ...] of sqrt(D).
    // Let ignore the trivial solution x = 1, y = 0.
    // Let p(n) / q(n) denote the n-th convergent [a(0), a(1), ..., a(n)],
    // then we will have solved Pell equation if we can find a convergent which obeys the identity p(n)^2 - D * q(n)^2 = (-1)^(n+1).
    // Amazingly, this turns out to always be possible as a result of the fact that the continued fraction of a quadratic surd always becomes periodic at some term a(r+1),
    // where a(r+1) = 2 * a(0), i.e., sqrt(D) = [a(0), a(1), ..., a(r), 2 * a(0), ...].
    // To compute the continued fraction convergents to sqrt(D), use the usual recurrence relations:
    // a(0) = [sqrt(D)]
    // p(0) = a(0)
    // p(1) = a(0) * a(1) + 1
    // p(n) = a(n) * p(n-1) + p(n-2)
    // q(0) = 1
    // q(1) = a1
    // q(n) = a(n) * q(n-1) + q(n-2)
    // where [x] is the floor function. For reasons to be explained shortly, also compute the two additional quantities P(n) and Q(n) defined by
    // P(0) = 0
    // P(1) = a(0)
    // Pn = a(n-1) * Q(n-1) - P(n-1)
    // Q(0) = 1
    // Q(1) = D - a(0) * a(0)
    // Q(n) = (D - P(n) * P(n)) / Q(n-1)
    // a(n) = [(a(0) + P(n)) / Q(n)]
    // Let a(r+1) = 2 * a(0) be the term at which the continued fraction becomes periodic (which will always happen for a quadratic surd).
    // For the Pell equation x^2 - D * y^2 = 1 with r odd, (-1)^(r+1) is positive and the solution in terms of smallest integers is x = p(r) and y = q(r), where p(r) / q(r) is the r-th convergent.
    // If r is even, then (-1)^(r+1) is negative, but p(2r+1)^2 - D * q(2r+1)^2 = 1, so the solution in smallest integers is x = p(2r+1), y = q(2r+1).
    // The equation x^2 - D * y^2 = -1 can be solved analogously to the equation with +1 on the right side if r is even, but has no solution if r is odd:
    // x = p(r) and y = q(r) for r is even, no solution for r is odd
    static member public FindFirstSolution(d: bigint, c: bigint) =
        match d, c with
        | _ when (c <> 1I) && (c <> -1I) -> raise (NotSupportedException())
        | _ when (d <= 0I) -> raise (ArgumentOutOfRangeException("d"))
        | _ when Numbers.IsPerfectSquare(d) -> None
        | _ -> PellEquation.FindFirstSolutionImpl(d, c)

    static member public FindNSolution(firstSolution: PellSolution, d: int, c: int, n: int) =
        PellEquation.FindNSolution(firstSolution, d |> bigint, c|> bigint, n)

    static member public FindNSolution(firstSolution: PellSolution, d: int64, c: int64, n: int) =
        PellEquation.FindNSolution(firstSolution, d |> bigint, c|> bigint, n)

    // Algorithm (from http://mathworld.wolfram.com/PellEquation.html):
    // Given one solution (x,y)=(p,q) (which can be found as above), a whole family of solutions can be found by taking each side to the n-th power, x^2 - D * y^2= (p^2 - D * q^2)^n = 1.
    // Factoring gives the following:
    // (x + sqrt(D) * y) * (x - sqrt(D) * y) = (p + sqrt(D) * q)^n * (p - sqrt(D) * q)^n
    // and
    // x + sqrt(D) * y  = (p + sqrt(D) * q)^n
    // x - sqrt(D) * y  = (p - sqrt(D) * q)^n,
    // which gives the family of solutions
    // x = ((p + q * sqrt(D))^n + (p - q * sqrt(D))^n) / 2
    // y = ((p + q * sqrt(D))^n - (p - q * sqrt(D))^n) / (2 * sqrt(D)).
    // These solutions also hold for x^2 - D * y^2 = -1, except that n can take on only odd values.
    // Binomial theorem (from https://en.wikipedia.org/wiki/Binomial_theorem):
    // (a + b)^n = C(0, n) * a^n + C(1, n) * a^(n-1) * b + ... + C(k, n) * a^(n-k) * b^k + ... + C(n, n) * b^n, where C(k, n) = n! / (k! * (n - k)!) - Binomial coefficient
    // (a + b)^n + (a - b)^n = C(0, n) * a^n + C(1, n) * a^(n-1) * b + C(2, n) * a^(n-2) * b^2 + ... + C(0, n) * a^n - C(1, n) * a^(n-1) * b + C(2, n) * a^(n-2) * b^2 + ... =
    // 2 * C(0, n) * a^n  + 2 * C(2, n) * a^(n-2) * b^2 + ...
    // (a + b)^n + (a - b)^n = C(0, n) * a^n + C(1, n) * a^(n-1) * b + C(2, n) * a^(n-2) * b^2 + ... - C(0, n) * a^n + C(1, n) * a^(n-1) * b - C(2, n) * a^(n-2) * b^2 + ... =
    // 2 * C(1, n) * a^(n-1) * b + 2 * C(3, n) * a^(n-3) * b^3 + ...
    // from this is following:
    // x = ((p + q * sqrt(D))^n + (p - q * sqrt(D))^n) / 2 =
    // (2 * C(0, n) * p^n  + 2 * C(2, n) * p^(n-2) * q^2 * D + ... + 2 * C(k, n) * p^(n-k) * q^k * D^(k/2) [k is even] + ...)/2 = 
    // C(0, n) * p^n  + C(2, n) * p^(n-2) * q^2 * D + ... + C(k, n) * p^(n-k) * q^k * D^(k/2) [k is even] + ...
    // y = ((p + q * sqrt(D))^n - (p - q * sqrt(D))^n) / (2 * sqrt(D)) =
    // (2 * C(1, n) * p^(n-1) * q * D^(1/2) + 2 * C(3, n) * p^(n-3) * q^3 * D^(3/2) + ...) / (2 * sqrt(D)) =
    // C(1, n) * p^(n-1) * q + 2 * C(3, n) * p^(n-3) * q^3 * D + ... + C(k, n) * p^(n-k) * q^k * D^((k-1)/2) [k is odd] + ...
    static member public FindNSolution(firstSolution: PellSolution, d: bigint, c: bigint, n: int) =
        match d, c with
        | _ when (c <> 1I) && (c <> -1I) -> raise (NotSupportedException())
        | _ when (c = -1I) && (n % 2 = 0) -> raise (ArgumentException())
        | _ when (d <= 0I) -> raise (ArgumentOutOfRangeException("d"))
        | _ when Numbers.IsPerfectSquare(d) -> raise (ArgumentException())
        | _ -> PellSolution(PellEquation.FindX(firstSolution, d, n), PellEquation.FindY(firstSolution, d, n))

    static member private FindFirstSolutionImpl(d: bigint, c: bigint) =
        let a0 = d |> float |> sqrt |> bigint
        let p0 = a0
        let q0 = 1I
        let pBig0 = 0I
        let qBig0 = 1I
        let pBig1 = a0
        let qBig1 = d - a0 * a0
        let a1 = (a0 + pBig1) / qBig1
        let p1 = a0 * a1 + 1I
        let q1 = a1
        let iteration, aN, pN, pNPrev, qN, qNPrev, pBigN, qBigN = PellEquation.FindPeriod(d, a0, a1, p1, p0, q1, q0, pBig1, qBig1, 1)
        let prevIteration = iteration - 1
        match prevIteration, c with
        | _ when (prevIteration % 2 = 1) && (c = -1I) -> None
        | _ when (prevIteration % 2 = 0) && (c = -1I) -> PellSolution(pNPrev, qNPrev) |> Some
        | _ when (prevIteration % 2 = 1) && (c = 1I) -> PellSolution(pNPrev, qNPrev) |> Some
        | _ when (prevIteration % 2 = 0) && (c = 1I) -> PellEquation.FindEvenSolution(d, a0, aN, pN, pNPrev, qN, qNPrev, pBigN, qBigN, iteration, 2 * prevIteration + 1)
        | _ -> failwith "Unexpected branch of match expression"

    static member private FindPeriod(d: bigint, a0: bigint, aN: bigint, pN: bigint, pNPrev: bigint, qN: bigint, qNPrev: bigint, pBigN: bigint, qBigN: bigint, iteration: int) =
        match aN with
        | _ when aN = 2I * a0 -> iteration, aN, pN, pNPrev, qN, qNPrev, pBigN, qBigN
        | _ ->
            let aNext, pNext, qNext, pBigNext, qBigNext = PellEquation.ProcessIteration(d, a0, aN, pN, pNPrev, qN, qNPrev, pBigN, qBigN)
            PellEquation.FindPeriod(d, a0, aNext, pNext, pN, qNext, qN, pBigNext, qBigNext, iteration + 1)

    static member private FindEvenSolution(d: bigint, a0: bigint, aN: bigint, pN: bigint, pNPrev: bigint, qN: bigint, qNPrev: bigint, pBigN: bigint, qBigN: bigint, iteration: int, maxIteration: int) =
        match iteration with
        | _ when iteration = maxIteration -> PellSolution(pN, qN) |> Some
        | _ ->
            let aNext, pNext, qNext, pBigNext, qBigNext = PellEquation.ProcessIteration(d, a0, aN, pN, pNPrev, qN, qNPrev, pBigN, qBigN)
            PellEquation.FindEvenSolution(d, a0, aNext, pNext, pN, qNext, qN, pBigNext, qBigNext, iteration + 1, maxIteration)

    static member private ProcessIteration(d: bigint, a0: bigint, aN: bigint, pN: bigint, pNPrev: bigint, qN: bigint, qNPrev: bigint, pBigN: bigint, qBigN: bigint) =
        let pBigNext = aN * qBigN - pBigN
        let qBigNext = (d - pBigNext * pBigNext) / qBigN
        let aNext = (a0 + pBigNext) / qBigNext
        let pNext = aNext * pN + pNPrev
        let qNext = aNext * qN + qNPrev
        aNext, pNext, qNext, pBigNext, qBigNext

    static member private FindX(firstSolution : PellSolution, d : bigint, n: int) =
        let rec findX (k: int) (result: bigint) =
            match k with
            | _ when k > n -> result
            | _ ->
                let value = Numbers.CalcBinomialCoeff(n, k) * (pown firstSolution.X (n - k)) * (pown firstSolution.Y k) * (pown d (k / 2))
                findX (k + 2) (result + value)
        findX 0 0I

    static member private FindY(firstSolution : PellSolution, d : bigint, n: int) =
        let rec findY (k: int) (result: bigint) =
            match k with
            | _ when k > n -> result
            | _ ->
                let value = Numbers.CalcBinomialCoeff(n, k) * (pown firstSolution.X (n - k)) * (pown firstSolution.Y k) * (pown d (k / 2))
                findY (k + 2) (result + value)
        findY 1 0I
