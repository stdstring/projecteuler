namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type SafeArrayTests() =

    [<Test>]
    member public this.CreateErrors() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeArray<int>(5, 0, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeArray<int>(5, 2, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeArray<int>(5, -2, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeArray<int>(-2, -5, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeArray<int>(0, 5, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeArray<int>(11, 25, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeArray<int>(-4, 5, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeArray<int>(-11, -5, 0) |> ignore) |> ignore

    [<Test>]
    member public this.GetValue() =
        // storage of int in 1..3 range
        let storage1 = SafeArray<int>(1, 3, 0)
        storage1.Storage.[0]<-11
        storage1.Storage.[1]<-12
        storage1.Storage.[2]<-13
        ClassicAssert.AreEqual(0, storage1.GetValue(-1))
        ClassicAssert.AreEqual(0, storage1.GetValue(0))
        ClassicAssert.AreEqual(11, storage1.GetValue(1))
        ClassicAssert.AreEqual(12, storage1.GetValue(2))
        ClassicAssert.AreEqual(13, storage1.GetValue(3))
        ClassicAssert.AreEqual(0, storage1.GetValue(4))
        ClassicAssert.AreEqual(0, storage1.GetValue(-1L))
        ClassicAssert.AreEqual(0, storage1.GetValue(0L))
        ClassicAssert.AreEqual(11, storage1.GetValue(1L))
        ClassicAssert.AreEqual(12, storage1.GetValue(2L))
        ClassicAssert.AreEqual(13, storage1.GetValue(3L))
        ClassicAssert.AreEqual(0, storage1.GetValue(4L))
        ClassicAssert.AreEqual(0, storage1.GetValue(-1I))
        ClassicAssert.AreEqual(0, storage1.GetValue(0I))
        ClassicAssert.AreEqual(11, storage1.GetValue(1I))
        ClassicAssert.AreEqual(12, storage1.GetValue(2I))
        ClassicAssert.AreEqual(13, storage1.GetValue(3I))
        ClassicAssert.AreEqual(0, storage1.GetValue(4I))
        // storage of int in -3..-1 range
        let storage2 = SafeArray<int>(-3, -1, 0)
        storage2.Storage.[0]<-13
        storage2.Storage.[1]<-12
        storage2.Storage.[2]<-11
        ClassicAssert.AreEqual(0, storage2.GetValue(-4))
        ClassicAssert.AreEqual(13, storage2.GetValue(-3))
        ClassicAssert.AreEqual(12, storage2.GetValue(-2))
        ClassicAssert.AreEqual(11, storage2.GetValue(-1))
        ClassicAssert.AreEqual(0, storage2.GetValue(0))
        ClassicAssert.AreEqual(0, storage2.GetValue(1))
        ClassicAssert.AreEqual(0, storage2.GetValue(-4L))
        ClassicAssert.AreEqual(13, storage2.GetValue(-3L))
        ClassicAssert.AreEqual(12, storage2.GetValue(-2L))
        ClassicAssert.AreEqual(11, storage2.GetValue(-1L))
        ClassicAssert.AreEqual(0, storage2.GetValue(0L))
        ClassicAssert.AreEqual(0, storage2.GetValue(1L))
        ClassicAssert.AreEqual(0, storage2.GetValue(-4I))
        ClassicAssert.AreEqual(13, storage2.GetValue(-3I))
        ClassicAssert.AreEqual(12, storage2.GetValue(-2I))
        ClassicAssert.AreEqual(11, storage2.GetValue(-1I))
        ClassicAssert.AreEqual(0, storage2.GetValue(0I))
        ClassicAssert.AreEqual(0, storage2.GetValue(1I))
        // storage of string in 1..3 range
        let storage3 = SafeArray<string>(1, 3, "")
        storage3.Storage.[0]<-"iddqd"
        storage3.Storage.[1]<-"idkfa"
        storage3.Storage.[2]<-"idclip"
        ClassicAssert.AreEqual("", storage3.GetValue(-1))
        ClassicAssert.AreEqual("", storage3.GetValue(0))
        ClassicAssert.AreEqual("iddqd", storage3.GetValue(1))
        ClassicAssert.AreEqual("idkfa", storage3.GetValue(2))
        ClassicAssert.AreEqual("idclip", storage3.GetValue(3))
        ClassicAssert.AreEqual("", storage3.GetValue(4))
        ClassicAssert.AreEqual("", storage3.GetValue(-1L))
        ClassicAssert.AreEqual("", storage3.GetValue(0L))
        ClassicAssert.AreEqual("iddqd", storage3.GetValue(1L))
        ClassicAssert.AreEqual("idkfa", storage3.GetValue(2L))
        ClassicAssert.AreEqual("idclip", storage3.GetValue(3L))
        ClassicAssert.AreEqual("", storage3.GetValue(4L))
        ClassicAssert.AreEqual("", storage3.GetValue(-1I))
        ClassicAssert.AreEqual("", storage3.GetValue(0I))
        ClassicAssert.AreEqual("iddqd", storage3.GetValue(1I))
        ClassicAssert.AreEqual("idkfa", storage3.GetValue(2I))
        ClassicAssert.AreEqual("idclip", storage3.GetValue(3I))
        ClassicAssert.AreEqual("", storage3.GetValue(4I))

    [<Test>]
    member public this.SetValue() =
        // storage of int in 1..3 range
        let storage1 = SafeArray<int>(1, 3, 0)
        ClassicAssert.AreEqual([|0; 0; 0|], storage1.Storage)
        storage1.SetValue(-1, 666)
        storage1.SetValue(-1L, 667)
        storage1.SetValue(-1I, 668)
        storage1.SetValue(0, 776)
        storage1.SetValue(0L, 777)
        storage1.SetValue(0I, 778)
        storage1.SetValue(1, 11)
        storage1.SetValue(2L, 12)
        storage1.SetValue(3I, 13)
        storage1.SetValue(4, 886)
        storage1.SetValue(4L, 887)
        storage1.SetValue(4I, 888)
        ClassicAssert.AreEqual([|11; 12; 13|], storage1.Storage)
        // storage of int in -3..-1 range
        let storage2 = SafeArray<int>(-3, -1, 0)
        ClassicAssert.AreEqual([|0; 0; 0|], storage2.Storage)
        storage2.SetValue(-4, 666)
        storage2.SetValue(-4L, 667)
        storage2.SetValue(-4I, 668)
        storage2.SetValue(-3, 13)
        storage2.SetValue(-2L, 12)
        storage2.SetValue(-1I, 11)
        storage2.SetValue(0, 776)
        storage2.SetValue(0L, 777)
        storage2.SetValue(0I, 778)
        storage2.SetValue(1, 886)
        storage2.SetValue(1L, 887)
        storage2.SetValue(1I, 888)
        ClassicAssert.AreEqual([|13; 12; 11|], storage2.Storage)
        // storage of string in 1..3 range
        let storage3 = SafeArray<string>(1, 3, "")
        ClassicAssert.AreEqual([|""; ""; ""|], storage3.Storage)
        storage3.SetValue(-1, "impulse_666")
        storage3.SetValue(-1L, "impulse_667")
        storage3.SetValue(-1I, "impulse_668")
        storage3.SetValue(0, "impulse_776")
        storage3.SetValue(0L, "impulse_777")
        storage3.SetValue(0I, "impulse_778")
        storage3.SetValue(1, "iddqd")
        storage3.SetValue(2L, "idkfa")
        storage3.SetValue(3I, "idclip")
        storage3.SetValue(4, "impulse_886")
        storage3.SetValue(4L, "impulse_887")
        storage3.SetValue(4I, "impulse_888")
        ClassicAssert.AreEqual([|"iddqd"; "idkfa"; "idclip"|], storage3.Storage)