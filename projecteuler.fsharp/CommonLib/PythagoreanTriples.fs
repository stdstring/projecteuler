namespace CommonLib

open System.Collections.Generic

type PythagoreanTriple = {X: int; Y: int; Z: int}

type PythagoreanTriplesGenerator(criterionSelector: PythagoreanTriple -> int) =

    let criterionSelector = criterionSelector

    let createTriple (m: int) (n: int) =
        {PythagoreanTriple.X = m * m - n * n; PythagoreanTriple.Y = 2 * m * n; PythagoreanTriple.Z = m * m + n * n}

    let scaleTriple (primitiveTriple: PythagoreanTriple) (k: int) =
        {PythagoreanTriple.X = k * primitiveTriple.X; PythagoreanTriple.Y = k * primitiveTriple.Y; PythagoreanTriple.Z = k * primitiveTriple.Z}

    let generateTriples (maxCriterionValue: int) (primitiveTriples: ISet<PythagoreanTriple>) =
        let triples = HashSet<PythagoreanTriple>()
        for primitiveTriple in primitiveTriples do
            triples.Add(primitiveTriple) |> ignore
            let scaleTriple = primitiveTriple |> scaleTriple
            let mutable k = 2
            let mutable triple = scaleTriple k
            while triple |> criterionSelector |> (>=) maxCriterionValue do
                triples.Add(triple) |> ignore
                k <- k + 1
                triple <- scaleTriple k
        triples :> ISet<PythagoreanTriple>

    member public this.GeneratePrimitiveTriples(maxCriterionValue: int) =
        let primitiveTriples = HashSet<PythagoreanTriple>()
        let mutable m = 2
        let mutable tripleLeft = createTriple m 1
        let mutable tripleRight = createTriple m (m - 1)
        while (tripleLeft |> criterionSelector |> (>=) maxCriterionValue) || (tripleRight |> criterionSelector |> (>=) maxCriterionValue) do
            let nSeq = seq{1 .. m - 1} |> Seq.skipWhile (fun value -> createTriple m value |> criterionSelector |> (<) maxCriterionValue)
                                       |> Seq.takeWhile (fun value -> createTriple m value |> criterionSelector |> (>=) maxCriterionValue)
            for n in nSeq do
                if (NumbersRelation.CalcGCD(m, n) = 1) && ((m - n) % 2 = 1) then
                    primitiveTriples.Add(createTriple m n) |> ignore
            m <- m + 1
            tripleLeft <- createTriple m 1
            tripleRight <- createTriple m (m - 1)
        primitiveTriples :> ISet<PythagoreanTriple>

    member public this.GenerateTriples(maxCriterionValue: int) =
        maxCriterionValue |> this.GeneratePrimitiveTriples |> generateTriples maxCriterionValue