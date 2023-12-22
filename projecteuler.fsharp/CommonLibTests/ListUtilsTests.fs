namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type ListUtilsTests() =

    [<Test>]
    member public this.GetCirularShift() =
        ClassicAssert.AreEqual([], ListUtils.GetCirularShift([]))
        ClassicAssert.AreEqual([1], ListUtils.GetCirularShift([1]))
        ClassicAssert.AreEqual([2; 1], ListUtils.GetCirularShift([1; 2]))
        ClassicAssert.AreEqual([2; 3; 1], ListUtils.GetCirularShift([1; 2; 3]))
        ClassicAssert.AreEqual([2; 3; 4; 1], ListUtils.GetCirularShift([1; 2; 3; 4]))
        ClassicAssert.AreEqual(["aaa"], ListUtils.GetCirularShift(["aaa"]))
        ClassicAssert.AreEqual(["bbb"; "aaa"], ListUtils.GetCirularShift(["aaa"; "bbb"]))
        ClassicAssert.AreEqual(["bbb"; "ccc"; "aaa"], ListUtils.GetCirularShift(["aaa"; "bbb"; "ccc"]))
        ClassicAssert.AreEqual(["bbb"; "ccc"; "ddd"; "aaa"], ListUtils.GetCirularShift(["aaa"; "bbb"; "ccc"; "ddd"]))

    [<Test>]
    member public this.ShiftToItem() =
        Assert.Throws<ArgumentException>(fun () -> ListUtils.ShiftToItem(1, []) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () -> ListUtils.ShiftToItem(1, [2]) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], ListUtils.ShiftToItem(1, [1]))
        Assert.Throws<ArgumentException>(fun () -> ListUtils.ShiftToItem(1, [2; 3; 2; 5]) |> ignore) |> ignore
        ClassicAssert.AreEqual([2; 3; 2; 5], ListUtils.ShiftToItem(2, [2; 3; 2; 5]))
        ClassicAssert.AreEqual([3; 2; 5; 2], ListUtils.ShiftToItem(3, [2; 3; 2; 5]))
        ClassicAssert.AreEqual([5; 2; 3; 2], ListUtils.ShiftToItem(5, [2; 3; 2; 5]))
        Assert.Throws<ArgumentException>(fun () -> ListUtils.ShiftToItem((1, 2), []) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () -> ListUtils.ShiftToItem((1, 2), [(2, 1)]) |> ignore) |> ignore
        ClassicAssert.AreEqual([(1, 2)], ListUtils.ShiftToItem((1, 2), [(1, 2)]))
        Assert.Throws<ArgumentException>(fun () -> ListUtils.ShiftToItem((1, 2), [(2, 1); (3, 3); (2, 1); (5, 7)]) |> ignore) |> ignore
        ClassicAssert.AreEqual([(2, 1); (3, 3); (2, 1); (5, 7)], ListUtils.ShiftToItem((2, 1), [(2, 1); (3, 3); (2, 1); (5, 7)]))
        ClassicAssert.AreEqual([(3, 3); (2, 1); (5, 7); (2, 1)], ListUtils.ShiftToItem((3, 3), [(2, 1); (3, 3); (2, 1); (5, 7)]))
        ClassicAssert.AreEqual([(5, 7); (2, 1); (3, 3); (2, 1)], ListUtils.ShiftToItem((5, 7), [(2, 1); (3, 3); (2, 1); (5, 7)]))

    [<Test>]
    member public this.GetAllCirularShift() =
        ClassicAssert.AreEqual(List.empty<obj list>, ListUtils.GetAllCirularShift([]))
        ClassicAssert.AreEqual([[1]], ListUtils.GetAllCirularShift([1]))
        ClassicAssert.AreEqual([[1; 2]; [2; 1]], ListUtils.GetAllCirularShift([1; 2]))
        ClassicAssert.AreEqual([[1; 2; 3]; [2; 3; 1]; [3; 1; 2]], ListUtils.GetAllCirularShift([1; 2; 3]))
        ClassicAssert.AreEqual([[1; 2; 3; 4]; [2; 3; 4; 1]; [3; 4; 1; 2]; [4; 1; 2; 3]], ListUtils.GetAllCirularShift([1; 2; 3; 4]))
        ClassicAssert.AreEqual([["aaa"]], ListUtils.GetAllCirularShift(["aaa"]))
        ClassicAssert.AreEqual([["aaa"; "bbb"]; ["bbb"; "aaa"]], ListUtils.GetAllCirularShift(["aaa"; "bbb"]))
        ClassicAssert.AreEqual([["aaa"; "bbb"; "ccc"]; ["bbb"; "ccc"; "aaa"]; ["ccc"; "aaa"; "bbb"]], ListUtils.GetAllCirularShift(["aaa"; "bbb"; "ccc"]))
        ClassicAssert.AreEqual([["aaa"; "bbb"; "ccc"; "ddd"]; ["bbb"; "ccc"; "ddd"; "aaa"]; ["ccc"; "ddd"; "aaa"; "bbb"]; ["ddd"; "aaa"; "bbb"; "ccc"]],
                               ListUtils.GetAllCirularShift(["aaa"; "bbb"; "ccc"; "ddd"]))
