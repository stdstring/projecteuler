﻿namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type PermutationsTests() =

    [<Test>]
    member public this.GetPermutationBadArgs() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(-1I, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(24I, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPermutation(0I, []) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(-1I, 2, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(12I, 2, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(0I, -1, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(0I, 0, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(0I, 5, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPermutation(0I, 2, []) |> ignore) |> ignore

    [<Test>]
    member public this.GetPermutationForAlphabet1() =
        ClassicAssert.AreEqual(['a'], Permutations.GetPermutation(0I, ['a']))
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(1I, ['a']) |> ignore) |> ignore

    [<Test>]
    member public this.GetPermutationForAlphabet4() =
        ClassicAssert.AreEqual(['a'; 'b'; 'c'; 'd'], Permutations.GetPermutation(0I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'b'; 'd'; 'c'], Permutations.GetPermutation(1I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'c'; 'b'; 'd'], Permutations.GetPermutation(2I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'c'; 'd'; 'b'], Permutations.GetPermutation(3I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'd'; 'b'; 'c'], Permutations.GetPermutation(4I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'd'; 'c'; 'b'], Permutations.GetPermutation(5I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'a'; 'c'; 'd'], Permutations.GetPermutation(6I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'a'; 'd'; 'c'], Permutations.GetPermutation(7I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'c'; 'a'; 'd'], Permutations.GetPermutation(8I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'c'; 'd'; 'a'], Permutations.GetPermutation(9I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'd'; 'a'; 'c'], Permutations.GetPermutation(10I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'd'; 'c'; 'a'], Permutations.GetPermutation(11I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'a'; 'b'; 'd'], Permutations.GetPermutation(12I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'a'; 'd'; 'b'], Permutations.GetPermutation(13I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'b'; 'a'; 'd'], Permutations.GetPermutation(14I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'b'; 'd'; 'a'], Permutations.GetPermutation(15I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'd'; 'a'; 'b'], Permutations.GetPermutation(16I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'd'; 'b'; 'a'], Permutations.GetPermutation(17I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'a'; 'b'; 'c'], Permutations.GetPermutation(18I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'a'; 'c'; 'b'], Permutations.GetPermutation(19I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'b'; 'a'; 'c'], Permutations.GetPermutation(20I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'b'; 'c'; 'a'], Permutations.GetPermutation(21I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'c'; 'a'; 'b'], Permutations.GetPermutation(22I, ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'c'; 'b'; 'a'], Permutations.GetPermutation(23I, ['a'; 'b'; 'c'; 'd']))
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(24I, ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore

    [<Test>]
    member public this.GetPermutationForAlphabet5Size3() =
        ClassicAssert.AreEqual(['a'; 'b'; 'c'], Permutations.GetPermutation(0I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'b'; 'd'], Permutations.GetPermutation(1I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'b'; 'e'], Permutations.GetPermutation(2I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'b'], Permutations.GetPermutation(3I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'd'], Permutations.GetPermutation(4I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'e'], Permutations.GetPermutation(5I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'b'], Permutations.GetPermutation(6I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'c'], Permutations.GetPermutation(7I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'e'], Permutations.GetPermutation(8I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'b'], Permutations.GetPermutation(9I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'c'], Permutations.GetPermutation(10I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'd'], Permutations.GetPermutation(11I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'c'], Permutations.GetPermutation(12I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'd'], Permutations.GetPermutation(13I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'e'], Permutations.GetPermutation(14I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'a'], Permutations.GetPermutation(15I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'd'], Permutations.GetPermutation(16I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'e'], Permutations.GetPermutation(17I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'a'], Permutations.GetPermutation(18I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'c'], Permutations.GetPermutation(19I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'e'], Permutations.GetPermutation(20I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'a'], Permutations.GetPermutation(21I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'c'], Permutations.GetPermutation(22I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'd'], Permutations.GetPermutation(23I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'b'], Permutations.GetPermutation(24I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'd'], Permutations.GetPermutation(25I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'e'], Permutations.GetPermutation(26I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'a'], Permutations.GetPermutation(27I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'd'], Permutations.GetPermutation(28I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'e'], Permutations.GetPermutation(29I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'a'], Permutations.GetPermutation(30I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'b'], Permutations.GetPermutation(31I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'e'], Permutations.GetPermutation(32I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'a'], Permutations.GetPermutation(33I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'b'], Permutations.GetPermutation(34I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'd'], Permutations.GetPermutation(35I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'b'], Permutations.GetPermutation(36I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'c'], Permutations.GetPermutation(37I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'e'], Permutations.GetPermutation(38I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'a'], Permutations.GetPermutation(39I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'c'], Permutations.GetPermutation(40I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'e'], Permutations.GetPermutation(41I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'a'], Permutations.GetPermutation(42I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'b'], Permutations.GetPermutation(43I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'e'], Permutations.GetPermutation(44I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'a'], Permutations.GetPermutation(45I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'b'], Permutations.GetPermutation(46I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'c'], Permutations.GetPermutation(47I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'b'], Permutations.GetPermutation(48I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'c'], Permutations.GetPermutation(49I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'd'], Permutations.GetPermutation(50I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'a'], Permutations.GetPermutation(51I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'c'], Permutations.GetPermutation(52I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'd'], Permutations.GetPermutation(53I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'a'], Permutations.GetPermutation(54I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'b'], Permutations.GetPermutation(55I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'd'], Permutations.GetPermutation(56I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'a'], Permutations.GetPermutation(57I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'b'], Permutations.GetPermutation(58I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'c'], Permutations.GetPermutation(59I, 3, ['a'; 'b'; 'c'; 'd'; 'e']))
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetPermutation(60I, 3, ['a'; 'b'; 'c'; 'd'; 'e']) |> ignore) |> ignore

    [<Test>]
    member public this.GetLexicographicalNumberBadArgs() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetLexicographicalNumber(['c'; 'b'; 'a'], []) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetLexicographicalNumber([], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetLexicographicalNumber(['d'; 'c'; 'b'; 'a'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetLexicographicalNumber(['a'; 'y'; 'c'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetLexicographicalNumber(['x'; 'y'; 'z'], ['a'; 'b'; 'c']) |> ignore) |> ignore

    [<Test>]
    member public this.GetLexicographicalNumberForAlphabet1() =
        ClassicAssert.AreEqual(0I, Permutations.GetLexicographicalNumber(['a'], ['a']))

    [<Test>]
    member public this.GetLexicographicalNumberForAlphabet4() =
        ClassicAssert.AreEqual(0I, Permutations.GetLexicographicalNumber(['a'; 'b'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(1I, Permutations.GetLexicographicalNumber(['a'; 'b'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(2I, Permutations.GetLexicographicalNumber(['a'; 'c'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(3I, Permutations.GetLexicographicalNumber(['a'; 'c'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(4I, Permutations.GetLexicographicalNumber(['a'; 'd'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(5I, Permutations.GetLexicographicalNumber(['a'; 'd'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(6I, Permutations.GetLexicographicalNumber(['b'; 'a'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(7I, Permutations.GetLexicographicalNumber(['b'; 'a'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(8I, Permutations.GetLexicographicalNumber(['b'; 'c'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(9I, Permutations.GetLexicographicalNumber(['b'; 'c'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(10I, Permutations.GetLexicographicalNumber(['b'; 'd'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(11I, Permutations.GetLexicographicalNumber(['b'; 'd'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(12I, Permutations.GetLexicographicalNumber(['c'; 'a'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(13I, Permutations.GetLexicographicalNumber(['c'; 'a'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(14I, Permutations.GetLexicographicalNumber(['c'; 'b'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(15I, Permutations.GetLexicographicalNumber(['c'; 'b'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(16I, Permutations.GetLexicographicalNumber(['c'; 'd'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(17I, Permutations.GetLexicographicalNumber(['c'; 'd'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(18I, Permutations.GetLexicographicalNumber(['d'; 'a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(19I, Permutations.GetLexicographicalNumber(['d'; 'a'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(20I, Permutations.GetLexicographicalNumber(['d'; 'b'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(21I, Permutations.GetLexicographicalNumber(['d'; 'b'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(22I, Permutations.GetLexicographicalNumber(['d'; 'c'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(23I, Permutations.GetLexicographicalNumber(['d'; 'c'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd']))

    [<Test>]
    member public this.GetLexicographicalNumberForAlphabet5Size3() =
        ClassicAssert.AreEqual(0I, Permutations.GetLexicographicalNumber(['a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(1I, Permutations.GetLexicographicalNumber(['a'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(2I, Permutations.GetLexicographicalNumber(['a'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(3I, Permutations.GetLexicographicalNumber(['a'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(4I, Permutations.GetLexicographicalNumber(['a'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(5I, Permutations.GetLexicographicalNumber(['a'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(6I, Permutations.GetLexicographicalNumber(['a'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(7I, Permutations.GetLexicographicalNumber(['a'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(8I, Permutations.GetLexicographicalNumber(['a'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(9I, Permutations.GetLexicographicalNumber(['a'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(10I, Permutations.GetLexicographicalNumber(['a'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(11I, Permutations.GetLexicographicalNumber(['a'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(12I, Permutations.GetLexicographicalNumber(['b'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(13I, Permutations.GetLexicographicalNumber(['b'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(14I, Permutations.GetLexicographicalNumber(['b'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(15I, Permutations.GetLexicographicalNumber(['b'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(16I, Permutations.GetLexicographicalNumber(['b'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(17I, Permutations.GetLexicographicalNumber(['b'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(18I, Permutations.GetLexicographicalNumber(['b'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(19I, Permutations.GetLexicographicalNumber(['b'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(20I, Permutations.GetLexicographicalNumber(['b'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(21I, Permutations.GetLexicographicalNumber(['b'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(22I, Permutations.GetLexicographicalNumber(['b'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(23I, Permutations.GetLexicographicalNumber(['b'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(24I, Permutations.GetLexicographicalNumber(['c'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(25I, Permutations.GetLexicographicalNumber(['c'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(26I, Permutations.GetLexicographicalNumber(['c'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(27I, Permutations.GetLexicographicalNumber(['c'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(28I, Permutations.GetLexicographicalNumber(['c'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(29I, Permutations.GetLexicographicalNumber(['c'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(30I, Permutations.GetLexicographicalNumber(['c'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(31I, Permutations.GetLexicographicalNumber(['c'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(32I, Permutations.GetLexicographicalNumber(['c'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(33I, Permutations.GetLexicographicalNumber(['c'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(34I, Permutations.GetLexicographicalNumber(['c'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(35I, Permutations.GetLexicographicalNumber(['c'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(36I, Permutations.GetLexicographicalNumber(['d'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(37I, Permutations.GetLexicographicalNumber(['d'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(38I, Permutations.GetLexicographicalNumber(['d'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(39I, Permutations.GetLexicographicalNumber(['d'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(40I, Permutations.GetLexicographicalNumber(['d'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(41I, Permutations.GetLexicographicalNumber(['d'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(42I, Permutations.GetLexicographicalNumber(['d'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(43I, Permutations.GetLexicographicalNumber(['d'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(44I, Permutations.GetLexicographicalNumber(['d'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(45I, Permutations.GetLexicographicalNumber(['d'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(46I, Permutations.GetLexicographicalNumber(['d'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(47I, Permutations.GetLexicographicalNumber(['d'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(48I, Permutations.GetLexicographicalNumber(['e'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(49I, Permutations.GetLexicographicalNumber(['e'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(50I, Permutations.GetLexicographicalNumber(['e'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(51I, Permutations.GetLexicographicalNumber(['e'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(52I, Permutations.GetLexicographicalNumber(['e'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(53I, Permutations.GetLexicographicalNumber(['e'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(54I, Permutations.GetLexicographicalNumber(['e'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(55I, Permutations.GetLexicographicalNumber(['e'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(56I, Permutations.GetLexicographicalNumber(['e'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(57I, Permutations.GetLexicographicalNumber(['e'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(58I, Permutations.GetLexicographicalNumber(['e'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(59I, Permutations.GetLexicographicalNumber(['e'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))

    [<Test>]
    member public this.GetLexicographicalNumberSupBadArgs() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetLexicographicalNumberSup([]) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetLexicographicalNumberSup([], 2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetLexicographicalNumberSup(['a', 'b', 'c'], -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetLexicographicalNumberSup(['a', 'b', 'c'], 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetLexicographicalNumberSup(['a', 'b', 'c'], 4) |> ignore) |> ignore

    [<Test>]
    member public this.GetLexicographicalNumberSup() =
        ClassicAssert.AreEqual(1I, Permutations.GetLexicographicalNumberSup(['a']))
        ClassicAssert.AreEqual(1I, Permutations.GetLexicographicalNumberSup(['a'], 1))
        ClassicAssert.AreEqual(2I, Permutations.GetLexicographicalNumberSup(['a'; 'b']))
        ClassicAssert.AreEqual(2I, Permutations.GetLexicographicalNumberSup(['a'; 'b'], 1))
        ClassicAssert.AreEqual(2I, Permutations.GetLexicographicalNumberSup(['a'; 'b'], 2))
        ClassicAssert.AreEqual(6I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c']))
        ClassicAssert.AreEqual(3I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'], 1))
        ClassicAssert.AreEqual(6I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'], 2))
        ClassicAssert.AreEqual(6I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'], 3))
        ClassicAssert.AreEqual(24I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(4I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'], 1))
        ClassicAssert.AreEqual(12I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'], 2))
        ClassicAssert.AreEqual(24I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'], 3))
        ClassicAssert.AreEqual(24I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'], 4))
        ClassicAssert.AreEqual(120I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(5I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'; 'e'], 1))
        ClassicAssert.AreEqual(20I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'; 'e'], 2))
        ClassicAssert.AreEqual(60I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'; 'e'], 3))
        ClassicAssert.AreEqual(120I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'; 'e'], 4))
        ClassicAssert.AreEqual(120I, Permutations.GetLexicographicalNumberSup(['a'; 'b'; 'c'; 'd'; 'e'], 5))

    [<Test>]
    member public this.GetNextPermutationBadArgs() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetNextPermutation(['c'; 'b'; 'a'], []) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetNextPermutation([], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Permutations.GetNextPermutation(['d'; 'c'; 'b'; 'a'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetNextPermutation(['a'; 'y'; 'c'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetNextPermutation(['x'; 'y'; 'z'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetNextPermutation(['c'; 'b'; 'a'], ['a'; 'b'; 'c']) |> ignore) |> ignore

    [<Test>]
    member public this.GetNextPermutationForAlphabet1() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetNextPermutation(['a'], ['a']) |> ignore) |> ignore

    [<Test>]
    member public this.GetNextPermutationForAlphabet4() =
        ClassicAssert.AreEqual(['a'; 'b'; 'd'; 'c'], Permutations.GetNextPermutation(['a'; 'b'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'c'; 'b'; 'd'], Permutations.GetNextPermutation(['a'; 'b'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'c'; 'd'; 'b'], Permutations.GetNextPermutation(['a'; 'c'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'd'; 'b'; 'c'], Permutations.GetNextPermutation(['a'; 'c'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'd'; 'c'; 'b'], Permutations.GetNextPermutation(['a'; 'd'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'a'; 'c'; 'd'], Permutations.GetNextPermutation(['a'; 'd'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'a'; 'd'; 'c'], Permutations.GetNextPermutation(['b'; 'a'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'c'; 'a'; 'd'], Permutations.GetNextPermutation(['b'; 'a'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'c'; 'd'; 'a'], Permutations.GetNextPermutation(['b'; 'c'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'd'; 'a'; 'c'], Permutations.GetNextPermutation(['b'; 'c'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'd'; 'c'; 'a'], Permutations.GetNextPermutation(['b'; 'd'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'a'; 'b'; 'd'], Permutations.GetNextPermutation(['b'; 'd'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'a'; 'd'; 'b'], Permutations.GetNextPermutation(['c'; 'a'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'b'; 'a'; 'd'], Permutations.GetNextPermutation(['c'; 'a'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'b'; 'd'; 'a'], Permutations.GetNextPermutation(['c'; 'b'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'd'; 'a'; 'b'], Permutations.GetNextPermutation(['c'; 'b'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'd'; 'b'; 'a'], Permutations.GetNextPermutation(['c'; 'd'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'a'; 'b'; 'c'], Permutations.GetNextPermutation(['c'; 'd'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'a'; 'c'; 'b'], Permutations.GetNextPermutation(['d'; 'a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'b'; 'a'; 'c'], Permutations.GetNextPermutation(['d'; 'a'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'b'; 'c'; 'a'], Permutations.GetNextPermutation(['d'; 'b'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'c'; 'a'; 'b'], Permutations.GetNextPermutation(['d'; 'b'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'c'; 'b'; 'a'], Permutations.GetNextPermutation(['d'; 'c'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd']))
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetNextPermutation(['d'; 'c'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore

    member public this.GetNextPermutationForAlphabet5Size3() =
        ClassicAssert.AreEqual(['a'; 'b'; 'd'], Permutations.GetNextPermutation(['a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'b'; 'e'], Permutations.GetNextPermutation(['a'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'b'], Permutations.GetNextPermutation(['a'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'd'], Permutations.GetNextPermutation(['a'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'e'], Permutations.GetNextPermutation(['a'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'b'], Permutations.GetNextPermutation(['a'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'c'], Permutations.GetNextPermutation(['a'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'e'], Permutations.GetNextPermutation(['a'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'b'], Permutations.GetNextPermutation(['a'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'c'], Permutations.GetNextPermutation(['a'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'd'], Permutations.GetNextPermutation(['a'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'c'], Permutations.GetNextPermutation(['a'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'd'], Permutations.GetNextPermutation(['b'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'e'], Permutations.GetNextPermutation(['b'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'a'], Permutations.GetNextPermutation(['b'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'd'], Permutations.GetNextPermutation(['b'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'e'], Permutations.GetNextPermutation(['b'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'a'], Permutations.GetNextPermutation(['b'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'c'], Permutations.GetNextPermutation(['b'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'e'], Permutations.GetNextPermutation(['b'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'a'], Permutations.GetNextPermutation(['b'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'c'], Permutations.GetNextPermutation(['b'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'd'], Permutations.GetNextPermutation(['b'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'b'], Permutations.GetNextPermutation(['b'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'd'], Permutations.GetNextPermutation(['c'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'e'], Permutations.GetNextPermutation(['c'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'a'], Permutations.GetNextPermutation(['c'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'd'], Permutations.GetNextPermutation(['c'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'e'], Permutations.GetNextPermutation(['c'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'a'], Permutations.GetNextPermutation(['c'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'b'], Permutations.GetNextPermutation(['c'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'e'], Permutations.GetNextPermutation(['c'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'a'], Permutations.GetNextPermutation(['c'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'b'], Permutations.GetNextPermutation(['c'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'd'], Permutations.GetNextPermutation(['c'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'b'], Permutations.GetNextPermutation(['c'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'c'], Permutations.GetNextPermutation(['d'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'e'], Permutations.GetNextPermutation(['d'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'a'], Permutations.GetNextPermutation(['d'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'c'], Permutations.GetNextPermutation(['d'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'e'], Permutations.GetNextPermutation(['d'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'a'], Permutations.GetNextPermutation(['d'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'b'], Permutations.GetNextPermutation(['d'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'e'], Permutations.GetNextPermutation(['d'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'a'], Permutations.GetNextPermutation(['d'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'b'], Permutations.GetNextPermutation(['d'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'c'], Permutations.GetNextPermutation(['d'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'b'], Permutations.GetNextPermutation(['d'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'c'], Permutations.GetNextPermutation(['e'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'd'], Permutations.GetNextPermutation(['e'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'a'], Permutations.GetNextPermutation(['e'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'c'], Permutations.GetNextPermutation(['e'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'd'], Permutations.GetNextPermutation(['e'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'a'], Permutations.GetNextPermutation(['e'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'b'], Permutations.GetNextPermutation(['e'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'd'], Permutations.GetNextPermutation(['e'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'a'], Permutations.GetNextPermutation(['e'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'b'], Permutations.GetNextPermutation(['e'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'c'], Permutations.GetNextPermutation(['e'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetNextPermutation(['e'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']) |> ignore) |> ignore

    [<Test>]
    member public this.GetPrevPermutationBadArgs() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['c'; 'b'; 'a'], []) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation([], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['d'; 'c'; 'b'; 'a'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['a'; 'y'; 'c'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['x'; 'y'; 'z'], ['a'; 'b'; 'c']) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['a'; 'b'; 'c'], ['a'; 'b'; 'c']) |> ignore) |> ignore

    [<Test>]
    member public this.GetPrevPermutationForAlphabet1() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['a'], ['a']) |> ignore) |> ignore

    [<Test>]
    member public this.GetPrevPermutationForAlphabet4() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['a'; 'b'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd']) |> ignore) |> ignore
        ClassicAssert.AreEqual(['a'; 'b'; 'c'; 'd'], Permutations.GetPrevPermutation(['a'; 'b'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'b'; 'd'; 'c'], Permutations.GetPrevPermutation(['a'; 'c'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'c'; 'b'; 'd'], Permutations.GetPrevPermutation(['a'; 'c'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'c'; 'd'; 'b'], Permutations.GetPrevPermutation(['a'; 'd'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'd'; 'b'; 'c'], Permutations.GetPrevPermutation(['a'; 'd'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['a'; 'd'; 'c'; 'b'], Permutations.GetPrevPermutation(['b'; 'a'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'a'; 'c'; 'd'], Permutations.GetPrevPermutation(['b'; 'a'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'a'; 'd'; 'c'], Permutations.GetPrevPermutation(['b'; 'c'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'c'; 'a'; 'd'], Permutations.GetPrevPermutation(['b'; 'c'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'c'; 'd'; 'a'], Permutations.GetPrevPermutation(['b'; 'd'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'd'; 'a'; 'c'], Permutations.GetPrevPermutation(['b'; 'd'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['b'; 'd'; 'c'; 'a'], Permutations.GetPrevPermutation(['c'; 'a'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'a'; 'b'; 'd'], Permutations.GetPrevPermutation(['c'; 'a'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'a'; 'd'; 'b'], Permutations.GetPrevPermutation(['c'; 'b'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'b'; 'a'; 'd'], Permutations.GetPrevPermutation(['c'; 'b'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'b'; 'd'; 'a'], Permutations.GetPrevPermutation(['c'; 'd'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'd'; 'a'; 'b'], Permutations.GetPrevPermutation(['c'; 'd'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['c'; 'd'; 'b'; 'a'], Permutations.GetPrevPermutation(['d'; 'a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'a'; 'b'; 'c'], Permutations.GetPrevPermutation(['d'; 'a'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'a'; 'c'; 'b'], Permutations.GetPrevPermutation(['d'; 'b'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'b'; 'a'; 'c'], Permutations.GetPrevPermutation(['d'; 'b'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'b'; 'c'; 'a'], Permutations.GetPrevPermutation(['d'; 'c'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd']))
        ClassicAssert.AreEqual(['d'; 'c'; 'a'; 'b'], Permutations.GetPrevPermutation(['d'; 'c'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd']))

    member public this.GetPrevPermutationForAlphabet5Size3() =
        Assert.Throws<ArgumentException>(fun() -> Permutations.GetPrevPermutation(['a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']) |> ignore) |> ignore
        ClassicAssert.AreEqual(['a'; 'b'; 'c'], Permutations.GetPrevPermutation(['a'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'b'; 'd'], Permutations.GetPrevPermutation(['a'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'b'; 'e'], Permutations.GetPrevPermutation(['a'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'b'], Permutations.GetPrevPermutation(['a'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'd'], Permutations.GetPrevPermutation(['a'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'c'; 'e'], Permutations.GetPrevPermutation(['a'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'b'], Permutations.GetPrevPermutation(['a'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'c'], Permutations.GetPrevPermutation(['a'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'd'; 'e'], Permutations.GetPrevPermutation(['a'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'b'], Permutations.GetPrevPermutation(['a'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'c'], Permutations.GetPrevPermutation(['a'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['a'; 'e'; 'd'], Permutations.GetPrevPermutation(['b'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'c'], Permutations.GetPrevPermutation(['b'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'd'], Permutations.GetPrevPermutation(['b'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'a'; 'e'], Permutations.GetPrevPermutation(['b'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'a'], Permutations.GetPrevPermutation(['b'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'd'], Permutations.GetPrevPermutation(['b'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'c'; 'e'], Permutations.GetPrevPermutation(['b'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'a'], Permutations.GetPrevPermutation(['b'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'c'], Permutations.GetPrevPermutation(['b'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'd'; 'e'], Permutations.GetPrevPermutation(['b'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'a'], Permutations.GetPrevPermutation(['b'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'c'], Permutations.GetPrevPermutation(['b'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['b'; 'e'; 'd'], Permutations.GetPrevPermutation(['c'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'b'], Permutations.GetPrevPermutation(['c'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'd'], Permutations.GetPrevPermutation(['c'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'a'; 'e'], Permutations.GetPrevPermutation(['c'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'a'], Permutations.GetPrevPermutation(['c'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'd'], Permutations.GetPrevPermutation(['c'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'b'; 'e'], Permutations.GetPrevPermutation(['c'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'a'], Permutations.GetPrevPermutation(['c'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'b'], Permutations.GetPrevPermutation(['c'; 'd'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'd'; 'e'], Permutations.GetPrevPermutation(['c'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'a'], Permutations.GetPrevPermutation(['c'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'b'], Permutations.GetPrevPermutation(['c'; 'e'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['c'; 'e'; 'd'], Permutations.GetPrevPermutation(['d'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'b'], Permutations.GetPrevPermutation(['d'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'c'], Permutations.GetPrevPermutation(['d'; 'a'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'a'; 'e'], Permutations.GetPrevPermutation(['d'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'a'], Permutations.GetPrevPermutation(['d'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'c'], Permutations.GetPrevPermutation(['d'; 'b'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'b'; 'e'], Permutations.GetPrevPermutation(['d'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'a'], Permutations.GetPrevPermutation(['d'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'b'], Permutations.GetPrevPermutation(['d'; 'c'; 'e'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'c'; 'e'], Permutations.GetPrevPermutation(['d'; 'e'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'a'], Permutations.GetPrevPermutation(['d'; 'e'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'b'], Permutations.GetPrevPermutation(['d'; 'e'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['d'; 'e'; 'c'], Permutations.GetPrevPermutation(['e'; 'a'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'b'], Permutations.GetPrevPermutation(['e'; 'a'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'c'], Permutations.GetPrevPermutation(['e'; 'a'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'a'; 'd'], Permutations.GetPrevPermutation(['e'; 'b'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'a'], Permutations.GetPrevPermutation(['e'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'c'], Permutations.GetPrevPermutation(['e'; 'b'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'b'; 'd'], Permutations.GetPrevPermutation(['e'; 'c'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'a'], Permutations.GetPrevPermutation(['e'; 'c'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'b'], Permutations.GetPrevPermutation(['e'; 'c'; 'd'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'c'; 'd'], Permutations.GetPrevPermutation(['e'; 'd'; 'a'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'a'], Permutations.GetPrevPermutation(['e'; 'd'; 'b'], ['a'; 'b'; 'c'; 'd'; 'e']))
        ClassicAssert.AreEqual(['e'; 'd'; 'b'], Permutations.GetPrevPermutation(['e'; 'd'; 'c'], ['a'; 'b'; 'c'; 'd'; 'e']))