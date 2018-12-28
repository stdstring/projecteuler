namespace CommonLibTests

open NUnit.Framework
open CommonLib

[<TestFixture>]
type ListUtilsTests() =

    [<Test>]
    member public this.GetCirularShift() =
        Assert.AreEqual([], ListUtils.GetCirularShift([]))
        Assert.AreEqual([1], ListUtils.GetCirularShift([1]))
        Assert.AreEqual([2; 1], ListUtils.GetCirularShift([1; 2]))
        Assert.AreEqual([2; 3; 1], ListUtils.GetCirularShift([1; 2; 3]))
        Assert.AreEqual([2; 3; 4; 1], ListUtils.GetCirularShift([1; 2; 3; 4]))
        Assert.AreEqual(["aaa"], ListUtils.GetCirularShift(["aaa"]))
        Assert.AreEqual(["bbb"; "aaa"], ListUtils.GetCirularShift(["aaa"; "bbb"]))
        Assert.AreEqual(["bbb"; "ccc"; "aaa"], ListUtils.GetCirularShift(["aaa"; "bbb"; "ccc"]))
        Assert.AreEqual(["bbb"; "ccc"; "ddd"; "aaa"], ListUtils.GetCirularShift(["aaa"; "bbb"; "ccc"; "ddd"]))

    [<Test>]
    member public this.GetAllCirularShift() =
        Assert.AreEqual([], ListUtils.GetAllCirularShift([]))
        Assert.AreEqual([[1]], ListUtils.GetAllCirularShift([1]))
        Assert.AreEqual([[1; 2]; [2; 1]], ListUtils.GetAllCirularShift([1; 2]))
        Assert.AreEqual([[1; 2; 3]; [2; 3; 1]; [3; 1; 2]], ListUtils.GetAllCirularShift([1; 2; 3]))
        Assert.AreEqual([[1; 2; 3; 4]; [2; 3; 4; 1]; [3; 4; 1; 2]; [4; 1; 2; 3]], ListUtils.GetAllCirularShift([1; 2; 3; 4]))
        Assert.AreEqual([["aaa"]], ListUtils.GetAllCirularShift(["aaa"]))
        Assert.AreEqual([["aaa"; "bbb"]; ["bbb"; "aaa"]], ListUtils.GetAllCirularShift(["aaa"; "bbb"]))
        Assert.AreEqual([["aaa"; "bbb"; "ccc"]; ["bbb"; "ccc"; "aaa"]; ["ccc"; "aaa"; "bbb"]], ListUtils.GetAllCirularShift(["aaa"; "bbb"; "ccc"]))
        Assert.AreEqual([["aaa"; "bbb"; "ccc"; "ddd"]; ["bbb"; "ccc"; "ddd"; "aaa"]; ["ccc"; "ddd"; "aaa"; "bbb"]; ["ddd"; "aaa"; "bbb"; "ccc"]], ListUtils.GetAllCirularShift(["aaa"; "bbb"; "ccc"; "ddd"]))
