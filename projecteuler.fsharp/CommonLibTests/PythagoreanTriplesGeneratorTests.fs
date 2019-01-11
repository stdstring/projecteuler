namespace CommonLibTests

open NUnit.Framework
open CommonLib

[<TestFixture>]
type PythagoreanTriplesGeneratorTests() =

    let createTriple (x: int, y: int, z: int) =
        {PythagoreanTriple.X = x; PythagoreanTriple.Y = y; PythagoreanTriple.Z = z}

    [<Test>]
    member public this.GeneratePrimitiveWithZSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.Z)
        let actualZ100 = generator.GeneratePrimitiveTriples(100) |> Seq.sortBy (fun triple -> triple.Z) |> Seq.toArray
        let expextedZ100 = [|(3, 4, 5); (5, 12, 13); (15, 8, 17); (7, 24, 25);
                            (21, 20, 29); (35, 12, 37); (9, 40, 41); (45, 28, 53);
                            (11, 60, 61); (33, 56, 65); (63, 16, 65); (55, 48, 73);
                            (13, 84, 85); (77, 36, 85); (39, 80, 89); (65, 72, 97)|]
        Assert.AreEqual(expextedZ100 |> Array.map (fun triple -> triple |> createTriple), actualZ100)
        let actualZ100300 = generator.GeneratePrimitiveTriples(300) |>
                            Seq.filter (fun triple -> (100 < triple.Z) && (triple.Z <= 300)) |>
                            Seq.sortBy (fun triple -> triple.Z) |>
                            Seq.toArray
        let expectedZ100300 = [|(99, 20, 101); (91, 60, 109); (15, 112, 113); (117, 44, 125);
                                (105, 88, 137); (17, 144, 145); (143, 24, 145); (51, 140, 149);
                                (85, 132, 157); (119, 120, 169); (165, 52, 173); (19, 180, 181);
                                (57, 176, 185); (153, 104, 185); (95, 168, 193); (195, 28, 197);
                                (133, 156, 205); (187, 84, 205); (21, 220, 221); (171, 140, 221);
                                (221, 60, 229); (105, 208, 233); (209, 120, 241); (255, 32, 257);
                                (23, 264, 265); (247, 96, 265); (69, 260, 269); (115, 252, 277);
                                (231, 160, 281); (161, 240, 289); (285, 68, 293)|]
        Assert.AreEqual(expectedZ100300 |> Array.map (fun triple -> triple |> createTriple), actualZ100300)

    [<Test>]
    member public this.GenerateWithZSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.Z)
        let actual = generator.GenerateTriples(40) |> Seq.sortBy (fun triple -> triple.Z) |> Seq.toArray
        let expexted = [|(3, 4, 5); (6, 8, 10); (5, 12, 13); (9, 12, 15);
                         (15, 8, 17); (12, 16, 20); (15, 20, 25); (7, 24, 25);
                         (10, 24, 26); (21, 20, 29); (18, 24, 30); (30, 16, 34);
                         (21, 28, 35); (35, 12, 37); (15, 36, 39); (24, 32, 40)|]
        Assert.AreEqual(expexted |> Array.map (fun triple -> triple |> createTriple), actual)

    [<Test>]
    member public this.GeneratePrimitiveWithXSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X)
        let actual = generator.GeneratePrimitiveTriples(20) |> Seq.sortBy (fun triple -> triple.X) |> Seq.toArray
        let expexted = [|(3, 4, 5); (5, 12, 13); (7, 24, 25); (9, 40, 41); (11, 60, 61);
                         (13, 84, 85); (15, 8, 17); (15, 112, 113); (17, 144, 145); (19, 180, 181)|]
        Assert.AreEqual(expexted |> Array.map (fun triple -> triple |> createTriple), actual)

    [<Test>]
    member public this.GenerateWithXSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X)
        let actual = generator.GenerateTriples(20) |> Seq.sortBy (fun triple -> triple.X) |> Seq.toArray
        let expexted = [|(3, 4, 5); (5, 12, 13); (6, 8, 10); (7, 24, 25);
                         (9, 12, 15); (9, 40, 41); (10, 24, 26); (11, 60, 61);
                         (12, 16, 20); (13, 84, 85); (14, 48, 50); (15, 20, 25);
                         (15, 36, 39); (15, 8, 17); (15, 112, 113); (17, 144, 145);
                         (18, 24, 30); (18, 80, 82); (19, 180, 181); (20, 48, 52)|]
        Assert.AreEqual(expexted |> Array.map (fun triple -> triple |> createTriple), actual)

    [<Test>]
    member public this.GeneratePrimitiveWithYSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.Y)
        let actual = generator.GeneratePrimitiveTriples(30) |> Seq.sortBy (fun triple -> triple.Y) |> Seq.toArray
        let expexted = [|(3, 4, 5); (15, 8, 17); (5, 12, 13); (35, 12, 37);
                         (63, 16, 65); (21, 20, 29); (99, 20, 101); (7, 24, 25);
                         (143, 24, 145); (45, 28, 53); (195, 28, 197)|]
        Assert.AreEqual(expexted |> Array.map (fun triple -> triple |> createTriple), actual)

    [<Test>]
    member public this.GenerateWithYSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.Y)
        let actual = generator.GenerateTriples(30) |> Seq.sortBy (fun triple -> triple.Y) |> Seq.toArray
        let expexted = [|(3, 4, 5); (6, 8, 10); (15, 8, 17); (9, 12, 15);
                         (5, 12, 13); (35, 12, 37); (12, 16, 20); (30, 16, 34);
                         (63, 16, 65); (15, 20, 25); (21, 20, 29); (99, 20, 101);
                         (18, 24, 30); (10, 24, 26); (45, 24, 51); (7, 24, 25);
                         (70, 24, 74); (143, 24, 145); (21, 28, 35); (45, 28, 53); (195, 28, 197)|]
        Assert.AreEqual(expexted |> Array.map (fun triple -> triple |> createTriple), actual)

    [<Test>]
    member public this.GeneratePrimitiveWithPerimeterSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X + triple.Y + triple.Z)
        let actual = generator.GeneratePrimitiveTriples(100) |> Seq.sortBy (fun triple -> triple.X + triple.Y + triple.Z) |> Seq.toArray
        let expexted = [|(3, 4, 5); (5, 12, 13); (15, 8, 17); (7, 24, 25); (21, 20, 29); (35, 12, 37); (9, 40, 41)|]
        Assert.AreEqual(expexted |> Array.map (fun triple -> triple |> createTriple), actual)

    [<Test>]
    member public this.GenerateWithPerimeterSelector() =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X + triple.Y + triple.Z)
        let actual = generator.GenerateTriples(100) |> Seq.sortBy (fun triple -> triple.X + triple.Y + triple.Z) |> Seq.toArray
        let expexted = [|(3, 4, 5); (6, 8, 10); (5, 12, 13); (9, 12, 15);
                         (15, 8, 17); (12, 16, 20); (7, 24, 25); (15, 20, 25);
                         (10, 24, 26); (21, 20, 29); (18, 24, 30); (30, 16, 34);
                         (21, 28, 35); (35, 12, 37); (15, 36, 39); (9, 40, 41); (24, 32, 40)|]
        Assert.AreEqual(expexted |> Array.map (fun triple -> triple |> createTriple), actual)
