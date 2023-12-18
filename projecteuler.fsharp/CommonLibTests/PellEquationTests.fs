namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type PellEquationTests() =

    [<Test>]
    member public this.FindFirstSolutionForPositivePellEquation() =
        // int
        this.FindFirstSolution(1, 1)
        this.FindFirstSolution(2, 1, PellSolution(3I, 2I))
        this.FindFirstSolution(3, 1, PellSolution(2I, 1I))
        this.FindFirstSolution(4, 1)
        this.FindFirstSolution(5, 1, PellSolution(9I, 4I))
        this.FindFirstSolution(6, 1, PellSolution(5I, 2I))
        this.FindFirstSolution(7, 1, PellSolution(8I, 3I))
        this.FindFirstSolution(8, 1, PellSolution(3I, 1I))
        this.FindFirstSolution(9, 1)
        this.FindFirstSolution(10, 1, PellSolution(19I, 6I))
        // int64
        this.FindFirstSolution(1L, 1L)
        this.FindFirstSolution(2L, 1L, PellSolution(3I, 2I))
        this.FindFirstSolution(3L, 1L, PellSolution(2I, 1I))
        this.FindFirstSolution(4L, 1L)
        this.FindFirstSolution(5L, 1L, PellSolution(9I, 4I))
        this.FindFirstSolution(6L, 1L, PellSolution(5I, 2I))
        this.FindFirstSolution(7L, 1L, PellSolution(8I, 3I))
        this.FindFirstSolution(8L, 1L, PellSolution(3I, 1I))
        this.FindFirstSolution(9L, 1L)
        this.FindFirstSolution(10L, 1L, PellSolution(19I, 6I))
        // bigint
        this.FindFirstSolution(1I, 1I)
        this.FindFirstSolution(2I, 1I, PellSolution(3I, 2I))
        this.FindFirstSolution(3I, 1I, PellSolution(2I, 1I))
        this.FindFirstSolution(4I, 1I)
        this.FindFirstSolution(5I, 1I, PellSolution(9I, 4I))
        this.FindFirstSolution(6I, 1I, PellSolution(5I, 2I))
        this.FindFirstSolution(7I, 1I, PellSolution(8I, 3I))
        this.FindFirstSolution(8I, 1I, PellSolution(3I, 1I))
        this.FindFirstSolution(9I, 1I)
        this.FindFirstSolution(10I, 1I, PellSolution(19I, 6I))
        this.FindFirstSolution(11I, 1I, PellSolution(10I, 3I))
        this.FindFirstSolution(12I, 1I, PellSolution(7I, 2I))
        this.FindFirstSolution(13I, 1I, PellSolution(649I, 180I))
        this.FindFirstSolution(14I, 1I, PellSolution(15I, 4I))
        this.FindFirstSolution(15I, 1I, PellSolution(4I, 1I))
        this.FindFirstSolution(16I, 1I)
        this.FindFirstSolution(17I, 1I, PellSolution(33I, 8I))
        this.FindFirstSolution(18I, 1I, PellSolution(17I, 4I))
        this.FindFirstSolution(19I, 1I, PellSolution(170I, 39I))
        this.FindFirstSolution(20I, 1I, PellSolution(9I, 2I))
        this.FindFirstSolution(21I, 1I, PellSolution(55I, 12I))
        this.FindFirstSolution(22I, 1I, PellSolution(197I, 42I))
        this.FindFirstSolution(23I, 1I, PellSolution(24I, 5I))
        this.FindFirstSolution(24I, 1I, PellSolution(5I, 1I))
        this.FindFirstSolution(25I, 1I)
        this.FindFirstSolution(26I, 1I, PellSolution(51I, 10I))
        this.FindFirstSolution(27I, 1I, PellSolution(26I, 5I))
        this.FindFirstSolution(28I, 1I, PellSolution(127I, 24I))
        this.FindFirstSolution(29I, 1I, PellSolution(9801I, 1820I))
        this.FindFirstSolution(30I, 1I, PellSolution(11I, 2I))
        this.FindFirstSolution(31I, 1I, PellSolution(1520I, 273I))
        this.FindFirstSolution(32I, 1I, PellSolution(17I, 3I))
        this.FindFirstSolution(33I, 1I, PellSolution(23I, 4I))
        this.FindFirstSolution(34I, 1I, PellSolution(35I, 6I))
        this.FindFirstSolution(35I, 1I, PellSolution(6I, 1I))
        this.FindFirstSolution(36I, 1I)
        this.FindFirstSolution(37I, 1I, PellSolution(73I, 12I))
        this.FindFirstSolution(38I, 1I, PellSolution(37I, 6I))
        this.FindFirstSolution(39I, 1I, PellSolution(25I, 4I))
        this.FindFirstSolution(40I, 1I, PellSolution(19I, 3I))
        this.FindFirstSolution(41I, 1I, PellSolution(2049I, 320I))
        this.FindFirstSolution(42I, 1I, PellSolution(13I, 2I))
        this.FindFirstSolution(43I, 1I, PellSolution(3482I, 531I))
        this.FindFirstSolution(44I, 1I, PellSolution(199I, 30I))
        this.FindFirstSolution(45I, 1I, PellSolution(161I, 24I))
        this.FindFirstSolution(46I, 1I, PellSolution(24335I, 3588I))
        this.FindFirstSolution(47I, 1I, PellSolution(48I, 7I))
        this.FindFirstSolution(48I, 1I, PellSolution(7I, 1I))
        this.FindFirstSolution(49I, 1I)
        this.FindFirstSolution(50I, 1I, PellSolution(99I, 14I))
        this.FindFirstSolution(51I, 1I, PellSolution(50I, 7I))
        this.FindFirstSolution(52I, 1I, PellSolution(649I, 90I))
        this.FindFirstSolution(53I, 1I, PellSolution(66249I, 9100I))
        this.FindFirstSolution(54I, 1I, PellSolution(485I, 66I))
        this.FindFirstSolution(55I, 1I, PellSolution(89I, 12I))
        this.FindFirstSolution(56I, 1I, PellSolution(15I, 2I))
        this.FindFirstSolution(57I, 1I, PellSolution(151I, 20I))
        this.FindFirstSolution(58I, 1I, PellSolution(19603I, 2574I))
        this.FindFirstSolution(59I, 1I, PellSolution(530I, 69I))
        this.FindFirstSolution(60I, 1I, PellSolution(31I, 4I))
        this.FindFirstSolution(61I, 1I, PellSolution(1766319049I, 226153980I))
        this.FindFirstSolution(62I, 1I, PellSolution(63I, 8I))
        this.FindFirstSolution(63I, 1I, PellSolution(8I, 1I))
        this.FindFirstSolution(64I, 1I)
        this.FindFirstSolution(65I, 1I, PellSolution(129I, 16I))
        this.FindFirstSolution(66I, 1I, PellSolution(65I, 8I))
        this.FindFirstSolution(67I, 1I, PellSolution(48842I, 5967I))
        this.FindFirstSolution(68I, 1I, PellSolution(33I, 4I))
        this.FindFirstSolution(69I, 1I, PellSolution(7775I, 936I))
        this.FindFirstSolution(70I, 1I, PellSolution(251I, 30I))
        this.FindFirstSolution(71I, 1I, PellSolution(3480I, 413I))
        this.FindFirstSolution(72I, 1I, PellSolution(17I, 2I))
        this.FindFirstSolution(73I, 1I, PellSolution(2281249I, 267000I))
        this.FindFirstSolution(74I, 1I, PellSolution(3699I, 430I))
        this.FindFirstSolution(75I, 1I, PellSolution(26I, 3I))
        this.FindFirstSolution(76I, 1I, PellSolution(57799I, 6630I))
        this.FindFirstSolution(77I, 1I, PellSolution(351I, 40I))
        this.FindFirstSolution(78I, 1I, PellSolution(53I, 6I))
        this.FindFirstSolution(79I, 1I, PellSolution(80I, 9I))
        this.FindFirstSolution(80I, 1I, PellSolution(9I, 1I))
        this.FindFirstSolution(81I, 1I)
        this.FindFirstSolution(82I, 1I, PellSolution(163I, 18I))
        this.FindFirstSolution(83I, 1I, PellSolution(82I, 9I))
        this.FindFirstSolution(84I, 1I, PellSolution(55I, 6I))
        this.FindFirstSolution(85I, 1I, PellSolution(285769I, 30996I))
        this.FindFirstSolution(86I, 1I, PellSolution(10405I, 1122I))
        this.FindFirstSolution(87I, 1I, PellSolution(28I, 3I))
        this.FindFirstSolution(88I, 1I, PellSolution(197I, 21I))
        this.FindFirstSolution(89I, 1I, PellSolution(500001I, 53000I))
        this.FindFirstSolution(90I, 1I, PellSolution(19I, 2I))
        this.FindFirstSolution(91I, 1I, PellSolution(1574I, 165I))
        this.FindFirstSolution(92I, 1I, PellSolution(1151I, 120I))
        this.FindFirstSolution(93I, 1I, PellSolution(12151I, 1260I))
        this.FindFirstSolution(94I, 1I, PellSolution(2143295I, 221064I))
        this.FindFirstSolution(95I, 1I, PellSolution(39I, 4I))
        this.FindFirstSolution(96I, 1I, PellSolution(49I, 5I))
        this.FindFirstSolution(97I, 1I, PellSolution(62809633I, 6377352I))
        this.FindFirstSolution(98I, 1I, PellSolution(99I, 10I))
        this.FindFirstSolution(99I, 1I, PellSolution(10I, 1I))
        this.FindFirstSolution(100I, 1I)
        this.FindFirstSolution(101I, 1I, PellSolution(201I, 20I))
        this.FindFirstSolution(102I, 1I, PellSolution(101I, 10I))
        this.FindFirstSolution(103I, 1I, PellSolution(227528I, 22419I))
        this.FindFirstSolution(104I, 1I, PellSolution(51I, 5I))
        this.FindFirstSolution(105I, 1I, PellSolution(41I, 4I))
        this.FindFirstSolution(106I, 1I, PellSolution(32080051I, 3115890I))
        this.FindFirstSolution(107I, 1I, PellSolution(962I, 93I))
        this.FindFirstSolution(108I, 1I, PellSolution(1351I, 130I))
        this.FindFirstSolution(109I, 1I, PellSolution(158070671986249I, 15140424455100I))
        this.FindFirstSolution(110I, 1I, PellSolution(21I, 2I))
        this.FindFirstSolution(111I, 1I, PellSolution(295I, 28I))
        this.FindFirstSolution(112I, 1I, PellSolution(127I, 12I))
        this.FindFirstSolution(113I, 1I, PellSolution(1204353I, 113296I))
        this.FindFirstSolution(114I, 1I, PellSolution(1025I, 96I))
        this.FindFirstSolution(115I, 1I, PellSolution(1126I, 105I))
        this.FindFirstSolution(116I, 1I, PellSolution(9801I, 910I))
        this.FindFirstSolution(117I, 1I, PellSolution(649I, 60I))
        this.FindFirstSolution(118I, 1I, PellSolution(306917I, 28254I))
        this.FindFirstSolution(119I, 1I, PellSolution(120I, 11I))
        this.FindFirstSolution(120I, 1I, PellSolution(11I, 1I))
        this.FindFirstSolution(121I, 1I)
        this.FindFirstSolution(122I, 1I, PellSolution(243I, 22I))
        this.FindFirstSolution(123I, 1I, PellSolution(122I, 11I))
        this.FindFirstSolution(124I, 1I, PellSolution(4620799I, 414960I))
        this.FindFirstSolution(125I, 1I, PellSolution(930249I, 83204I))
        this.FindFirstSolution(126I, 1I, PellSolution(449I, 40I))
        this.FindFirstSolution(127I, 1I, PellSolution(4730624I, 419775I))
        this.FindFirstSolution(128I, 1I, PellSolution(577I, 51I))

    [<Test>]
    member public this.FindFirstSolutionForNegativePellEquation() =
        // int
        this.FindFirstSolution(1, -1)
        this.FindFirstSolution(2, -1, PellSolution(1I, 1I))
        this.FindFirstSolution(3, -1)
        this.FindFirstSolution(4, -1)
        this.FindFirstSolution(5, -1, PellSolution(2I, 1I))
        this.FindFirstSolution(6, -1)
        this.FindFirstSolution(7, -1)
        this.FindFirstSolution(8, -1)
        this.FindFirstSolution(9, -1)
        this.FindFirstSolution(10, -1, PellSolution(3I, 1I))
        // int64
        this.FindFirstSolution(1L, -1L)
        this.FindFirstSolution(2L, -1L, PellSolution(1I, 1I))
        this.FindFirstSolution(3L, -1L)
        this.FindFirstSolution(4L, -1L)
        this.FindFirstSolution(5L, -1L, PellSolution(2I, 1I))
        this.FindFirstSolution(6L, -1L)
        this.FindFirstSolution(7L, -1L)
        this.FindFirstSolution(8L, -1L)
        this.FindFirstSolution(9L, -1L)
        this.FindFirstSolution(10L, -1L, PellSolution(3I, 1I))
        // bigint
        this.FindFirstSolution(1I, -1I)
        this.FindFirstSolution(2I, -1I, PellSolution(1I, 1I))
        this.FindFirstSolution(3I, -1I)
        this.FindFirstSolution(4I, -1I)
        this.FindFirstSolution(5I, -1I, PellSolution(2I, 1I))
        this.FindFirstSolution(6I, -1I)
        this.FindFirstSolution(7I, -1I)
        this.FindFirstSolution(8I, -1I)
        this.FindFirstSolution(9I, -1I)
        this.FindFirstSolution(10I, -1I, PellSolution(3I, 1I))
        this.FindFirstSolution(11I, -1I)
        this.FindFirstSolution(12I, -1I)
        this.FindFirstSolution(13I, -1I, PellSolution(18I, 5I))
        this.FindFirstSolution(14I, -1I)
        this.FindFirstSolution(15I, -1I)
        this.FindFirstSolution(16I, -1I)
        this.FindFirstSolution(17I, -1I, PellSolution(4I, 1I))
        this.FindFirstSolution(18I, -1I)
        this.FindFirstSolution(19I, -1I)
        this.FindFirstSolution(20I, -1I)
        this.FindFirstSolution(21I, -1I)
        this.FindFirstSolution(22I, -1I)
        this.FindFirstSolution(23I, -1I)
        this.FindFirstSolution(24I, -1I)
        this.FindFirstSolution(25I, -1I)
        this.FindFirstSolution(26I, -1I, PellSolution(5I, 1I))
        this.FindFirstSolution(27I, -1I)
        this.FindFirstSolution(28I, -1I)
        this.FindFirstSolution(29I, -1I, PellSolution(70I, 13I))
        this.FindFirstSolution(30I, -1I)
        this.FindFirstSolution(31I, -1I)
        this.FindFirstSolution(32I, -1I)
        this.FindFirstSolution(33I, -1I)
        this.FindFirstSolution(34I, -1I)
        this.FindFirstSolution(35I, -1I)
        this.FindFirstSolution(36I, -1I)
        this.FindFirstSolution(37I, -1I, PellSolution(6I, 1I))
        this.FindFirstSolution(38I, -1I)
        this.FindFirstSolution(39I, -1I)
        this.FindFirstSolution(40I, -1I)
        this.FindFirstSolution(41I, -1I, PellSolution(32I, 5I))
        this.FindFirstSolution(42I, -1I)
        this.FindFirstSolution(43I, -1I)
        this.FindFirstSolution(44I, -1I)
        this.FindFirstSolution(45I, -1I)
        this.FindFirstSolution(46I, -1I)
        this.FindFirstSolution(47I, -1I)
        this.FindFirstSolution(48I, -1I)
        this.FindFirstSolution(49I, -1I)
        this.FindFirstSolution(50I, -1I, PellSolution(7I, 1I))
        this.FindFirstSolution(51I, -1I)
        this.FindFirstSolution(52I, -1I)
        this.FindFirstSolution(53I, -1I, PellSolution(182I, 25I))
        this.FindFirstSolution(54I, -1I)
        this.FindFirstSolution(55I, -1I)
        this.FindFirstSolution(56I, -1I)
        this.FindFirstSolution(57I, -1I)
        this.FindFirstSolution(58I, -1I, PellSolution(99I, 13I))
        this.FindFirstSolution(59I, -1I)
        this.FindFirstSolution(60I, -1I)
        this.FindFirstSolution(61I, -1I, PellSolution(29718I, 3805I))
        this.FindFirstSolution(62I, -1I)
        this.FindFirstSolution(63I, -1I)
        this.FindFirstSolution(64I, -1I)
        this.FindFirstSolution(65I, -1I, PellSolution(8I, 1I))
        this.FindFirstSolution(66I, -1I)
        this.FindFirstSolution(67I, -1I)
        this.FindFirstSolution(68I, -1I)
        this.FindFirstSolution(69I, -1I)
        this.FindFirstSolution(70I, -1I)
        this.FindFirstSolution(71I, -1I)
        this.FindFirstSolution(72I, -1I)
        this.FindFirstSolution(73I, -1I, PellSolution(1068I, 125I))
        this.FindFirstSolution(74I, -1I, PellSolution(43I, 5I))
        this.FindFirstSolution(75I, -1I)
        this.FindFirstSolution(76I, -1I)
        this.FindFirstSolution(77I, -1I)
        this.FindFirstSolution(78I, -1I)
        this.FindFirstSolution(79I, -1I)
        this.FindFirstSolution(80I, -1I)
        this.FindFirstSolution(81I, -1I)
        this.FindFirstSolution(82I, -1I, PellSolution(9I, 1I))
        this.FindFirstSolution(83I, -1I)
        this.FindFirstSolution(84I, -1I)
        this.FindFirstSolution(85I, -1I, PellSolution(378I, 41I))
        this.FindFirstSolution(86I, -1I)
        this.FindFirstSolution(87I, -1I)
        this.FindFirstSolution(88I, -1I)
        this.FindFirstSolution(89I, -1I, PellSolution(500I, 53I))
        this.FindFirstSolution(90I, -1I)
        this.FindFirstSolution(91I, -1I)
        this.FindFirstSolution(92I, -1I)
        this.FindFirstSolution(93I, -1I)
        this.FindFirstSolution(94I, -1I)
        this.FindFirstSolution(95I, -1I)
        this.FindFirstSolution(96I, -1I)
        this.FindFirstSolution(97I, -1I, PellSolution(5604I, 569I))
        this.FindFirstSolution(98I, -1I)
        this.FindFirstSolution(99I, -1I)
        this.FindFirstSolution(100I, -1I)
        this.FindFirstSolution(101I, -1I, PellSolution(10I, 1I))
        this.FindFirstSolution(102I, -1I)
        this.FindFirstSolution(103I, -1I)
        this.FindFirstSolution(104I, -1I)
        this.FindFirstSolution(105I, -1I)
        this.FindFirstSolution(106I, -1I, PellSolution(4005I, 389I))
        this.FindFirstSolution(107I, -1I)
        this.FindFirstSolution(108I, -1I)
        this.FindFirstSolution(109I, -1I, PellSolution(8890182I, 851525I))
        this.FindFirstSolution(110I, -1I)
        this.FindFirstSolution(111I, -1I)
        this.FindFirstSolution(112I, -1I)
        this.FindFirstSolution(113I, -1I, PellSolution(776I, 73I))
        this.FindFirstSolution(114I, -1I)
        this.FindFirstSolution(115I, -1I)
        this.FindFirstSolution(116I, -1I)
        this.FindFirstSolution(117I, -1I)
        this.FindFirstSolution(118I, -1I)
        this.FindFirstSolution(119I, -1I)
        this.FindFirstSolution(120I, -1I)
        this.FindFirstSolution(121I, -1I)
        this.FindFirstSolution(122I, -1I, PellSolution(11I, 1I))
        this.FindFirstSolution(123I, -1I)
        this.FindFirstSolution(124I, -1I)
        this.FindFirstSolution(125I, -1I, PellSolution(682I, 61I))
        this.FindFirstSolution(126I, -1I)
        this.FindFirstSolution(127I, -1I)
        this.FindFirstSolution(128I, -1I)

    [<Test>]
    member public this.FindFirstSolutionErrors() =
        // int
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindFirstSolution(2, 2) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindFirstSolution(2, -2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindFirstSolution(-1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindFirstSolution(0, 1) |> ignore) |> ignore
        // int64
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindFirstSolution(2L, 2L) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindFirstSolution(2L, -2L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindFirstSolution(-1L, 1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindFirstSolution(0L, 1L) |> ignore) |> ignore
        // bigint
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindFirstSolution(2I, 2I) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindFirstSolution(2I, -2I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindFirstSolution(-1I, 1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindFirstSolution(0I, 1I) |> ignore) |> ignore

    [<Test>]
    member public this.FindNSolutionForPositivePellEquation() =
        // int
        this.FindNSolution(PellSolution(8I, 3I), 7, 1, 1, PellSolution(8I, 3I))
        this.FindNSolution(PellSolution(8I, 3I), 7, 1, 2, PellSolution(127I, 48I))
        this.FindNSolution(PellSolution(8I, 3I), 7, 1, 3, PellSolution(2024I, 765I))
        this.FindNSolution(PellSolution(8I, 3I), 7, 1, 4, PellSolution(32257I, 12192I))
        this.FindNSolution(PellSolution(8I, 3I), 7, 1, 5, PellSolution(514088I, 194307I))
        this.FindNSolution(PellSolution(8I, 3I), 7, 1, 6, PellSolution(8193151I, 3096720I))
        this.FindNSolution(PellSolution(8I, 3I), 7, 1, 7, PellSolution(130576328I, 49353213I))
        this.FindNSolution(PellSolution(3I, 2I), 2, 1, 1, PellSolution(3I, 2I))
        this.FindNSolution(PellSolution(3I, 2I), 2, 1, 2, PellSolution(17I, 12I))
        this.FindNSolution(PellSolution(3I, 2I), 2, 1, 3, PellSolution(99I, 70I))
        this.FindNSolution(PellSolution(3I, 2I), 2, 1, 4, PellSolution(577I, 408I))
        // int64
        this.FindNSolution(PellSolution(8I, 3I), 7L, 1L, 1, PellSolution(8I, 3I))
        this.FindNSolution(PellSolution(8I, 3I), 7L, 1L, 2, PellSolution(127I, 48I))
        this.FindNSolution(PellSolution(8I, 3I), 7L, 1L, 3, PellSolution(2024I, 765I))
        this.FindNSolution(PellSolution(8I, 3I), 7L, 1L, 4, PellSolution(32257I, 12192I))
        this.FindNSolution(PellSolution(8I, 3I), 7L, 1L, 5, PellSolution(514088I, 194307I))
        this.FindNSolution(PellSolution(8I, 3I), 7L, 1L, 6, PellSolution(8193151I, 3096720I))
        this.FindNSolution(PellSolution(8I, 3I), 7L, 1L, 7, PellSolution(130576328I, 49353213I))
        this.FindNSolution(PellSolution(3I, 2I), 2L, 1L, 1, PellSolution(3I, 2I))
        this.FindNSolution(PellSolution(3I, 2I), 2L, 1L, 2, PellSolution(17I, 12I))
        this.FindNSolution(PellSolution(3I, 2I), 2L, 1L, 3, PellSolution(99I, 70I))
        this.FindNSolution(PellSolution(3I, 2I), 2L, 1L, 4, PellSolution(577I, 408I))
        // bigint
        this.FindNSolution(PellSolution(8I, 3I), 7I, 1I, 1, PellSolution(8I, 3I))
        this.FindNSolution(PellSolution(8I, 3I), 7I, 1I, 2, PellSolution(127I, 48I))
        this.FindNSolution(PellSolution(8I, 3I), 7I, 1I, 3, PellSolution(2024I, 765I))
        this.FindNSolution(PellSolution(8I, 3I), 7I, 1I, 4, PellSolution(32257I, 12192I))
        this.FindNSolution(PellSolution(8I, 3I), 7I, 1I, 5, PellSolution(514088I, 194307I))
        this.FindNSolution(PellSolution(8I, 3I), 7I, 1I, 6, PellSolution(8193151I, 3096720I))
        this.FindNSolution(PellSolution(8I, 3I), 7I, 1I, 7, PellSolution(130576328I, 49353213I))
        this.FindNSolution(PellSolution(3I, 2I), 2I, 1I, 1, PellSolution(3I, 2I))
        this.FindNSolution(PellSolution(3I, 2I), 2I, 1I, 2, PellSolution(17I, 12I))
        this.FindNSolution(PellSolution(3I, 2I), 2I, 1I, 3, PellSolution(99I, 70I))
        this.FindNSolution(PellSolution(3I, 2I), 2I, 1I, 4, PellSolution(577I, 408I))

    [<Test>]
    member public this.FindNSolutionForNegativePellEquation() =
        // int
        this.FindNSolution(PellSolution(1I, 1I), 2, -1, 1, PellSolution(1I, 1I))
        this.FindNSolution(PellSolution(1I, 1I), 2, -1, 3, PellSolution(7I, 5I))
        this.FindNSolution(PellSolution(1I, 1I), 2, -1, 5, PellSolution(41I, 29I))
        this.FindNSolution(PellSolution(1I, 1I), 2, -1, 7, PellSolution(239I, 169I))
        // int64
        this.FindNSolution(PellSolution(1I, 1I), 2L, -1L, 1, PellSolution(1I, 1I))
        this.FindNSolution(PellSolution(1I, 1I), 2L, -1L, 3, PellSolution(7I, 5I))
        this.FindNSolution(PellSolution(1I, 1I), 2L, -1L, 5, PellSolution(41I, 29I))
        this.FindNSolution(PellSolution(1I, 1I), 2L, -1L, 7, PellSolution(239I, 169I))
        // bigint
        this.FindNSolution(PellSolution(1I, 1I), 2I, -1I, 1, PellSolution(1I, 1I))
        this.FindNSolution(PellSolution(1I, 1I), 2I, -1I, 3, PellSolution(7I, 5I))
        this.FindNSolution(PellSolution(1I, 1I), 2I, -1I, 5, PellSolution(41I, 29I))
        this.FindNSolution(PellSolution(1I, 1I), 2I, -1I, 7, PellSolution(239I, 169I))

    [<Test>]
    member public this.FindNSolutionErrors() =
        // int
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2, 2, 1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2, -2, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), -1, 1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 0, 1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2, -1, 2) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(3I, 2I), 4, 1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(3I, 2I), 4, 1, 2) |> ignore) |> ignore
        // int64
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2L, 2L, 1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2L, -2L, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), -1L, 1L, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 0L, 1L, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2L, -1L, 2) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(3I, 2I), 4L, 1L, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(3I, 2I), 4L, 1L, 2) |> ignore) |> ignore
        // bigint
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2I, 2I, 1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2I, -2I, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), -1I, 1I, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 0I, 1I, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(1I, 1I), 2I, -1I, 2) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(3I, 2I), 4I, 1I, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> PellEquation.FindNSolution(PellSolution(3I, 2I), 4I, 1I, 2) |> ignore) |> ignore

    member private this.FindFirstSolution(d: int, c: int) =
        ClassicAssert.AreEqual(None, PellEquation.FindFirstSolution(d, c), "first solution of x^2 - {0} * y^2 = {1} equation", d, c)

    member private this.FindFirstSolution(d: int64, c: int64) =
        ClassicAssert.AreEqual(None, PellEquation.FindFirstSolution(d, c), "first solution of x^2 - {0} * y^2 = {1} equation", d, c)

    member private this.FindFirstSolution(d: bigint, c: bigint) =
        ClassicAssert.AreEqual(None, PellEquation.FindFirstSolution(d, c), "first solution of x^2 - {0} * y^2 = {1} equation", d, c)

    member private this.FindFirstSolution(d: int, c: int, expectedSolution: PellSolution) =
        ClassicAssert.AreEqual(expectedSolution |> Some, PellEquation.FindFirstSolution(d, c), "first solution of x^2 - {0} * y^2 = {1} equation", d, c)

    member private this.FindFirstSolution(d: int64, c: int64, expectedSolution: PellSolution) =
        ClassicAssert.AreEqual(expectedSolution |> Some, PellEquation.FindFirstSolution(d, c), "first solution of x^2 - {0} * y^2 = {1} equation", d, c)

    member private this.FindFirstSolution(d: bigint, c: bigint, expectedSolution: PellSolution) =
        ClassicAssert.AreEqual(expectedSolution |> Some, PellEquation.FindFirstSolution(d, c), "first solution of x^2 - {0} * y^2 = {1} equation", d, c)

    member private this.FindNSolution(firstSolution: PellSolution, d: int, c: int, n: int, expectedSolution: PellSolution) =
        ClassicAssert.AreEqual(expectedSolution, PellEquation.FindNSolution(firstSolution, d, c, n), "{0}-th solution of x^2 - {1} * y^2 = {2}", n, d, c)

    member private this.FindNSolution(firstSolution: PellSolution, d: int64, c: int64, n: int, expectedSolution: PellSolution) =
        ClassicAssert.AreEqual(expectedSolution, PellEquation.FindNSolution(firstSolution, d, c, n), "{0}-th solution of x^2 - {1} * y^2 = {2}", n, d, c)

    member private this.FindNSolution(firstSolution: PellSolution, d: bigint, c: bigint, n: int, expectedSolution: PellSolution) =
        ClassicAssert.AreEqual(expectedSolution, PellEquation.FindNSolution(firstSolution, d, c, n), "{0}-th solution of x^2 - {1} * y^2 = {2}", n, d, c)