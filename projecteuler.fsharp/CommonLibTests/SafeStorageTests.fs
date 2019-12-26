namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib

[<TestFixture>]
type SafeStorageTests() =

    [<Test>]
    member public this.CreateErrors() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeStorage<int>(5, 0, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeStorage<int>(5, 2, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeStorage<int>(5, -2, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> SafeStorage<int>(-2, -5, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeStorage<int>(0, 5, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeStorage<int>(11, 25, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeStorage<int>(-4, 5, 0) |> ignore) |> ignore
        Assert.DoesNotThrow(fun() -> SafeStorage<int>(-11, -5, 0) |> ignore) |> ignore

    [<Test>]
    member public this.GetValue() =
        // storage of int in 1..3 range
        let storage1 = SafeStorage<int>(1, 3, 0)
        storage1.Storage.[0]<-11
        storage1.Storage.[1]<-12
        storage1.Storage.[2]<-13
        Assert.AreEqual(0, storage1.GetValue(-1))
        Assert.AreEqual(0, storage1.GetValue(0))
        Assert.AreEqual(11, storage1.GetValue(1))
        Assert.AreEqual(12, storage1.GetValue(2))
        Assert.AreEqual(13, storage1.GetValue(3))
        Assert.AreEqual(0, storage1.GetValue(4))
        Assert.AreEqual(0, storage1.GetValue(-1L))
        Assert.AreEqual(0, storage1.GetValue(0L))
        Assert.AreEqual(11, storage1.GetValue(1L))
        Assert.AreEqual(12, storage1.GetValue(2L))
        Assert.AreEqual(13, storage1.GetValue(3L))
        Assert.AreEqual(0, storage1.GetValue(4L))
        Assert.AreEqual(0, storage1.GetValue(-1I))
        Assert.AreEqual(0, storage1.GetValue(0I))
        Assert.AreEqual(11, storage1.GetValue(1I))
        Assert.AreEqual(12, storage1.GetValue(2I))
        Assert.AreEqual(13, storage1.GetValue(3I))
        Assert.AreEqual(0, storage1.GetValue(4I))
        // storage of int in -3..-1 range
        let storage2 = SafeStorage<int>(-3, -1, 0)
        storage2.Storage.[0]<-13
        storage2.Storage.[1]<-12
        storage2.Storage.[2]<-11
        Assert.AreEqual(0, storage2.GetValue(-4))
        Assert.AreEqual(13, storage2.GetValue(-3))
        Assert.AreEqual(12, storage2.GetValue(-2))
        Assert.AreEqual(11, storage2.GetValue(-1))
        Assert.AreEqual(0, storage2.GetValue(0))
        Assert.AreEqual(0, storage2.GetValue(1))
        Assert.AreEqual(0, storage2.GetValue(-4L))
        Assert.AreEqual(13, storage2.GetValue(-3L))
        Assert.AreEqual(12, storage2.GetValue(-2L))
        Assert.AreEqual(11, storage2.GetValue(-1L))
        Assert.AreEqual(0, storage2.GetValue(0L))
        Assert.AreEqual(0, storage2.GetValue(1L))
        Assert.AreEqual(0, storage2.GetValue(-4I))
        Assert.AreEqual(13, storage2.GetValue(-3I))
        Assert.AreEqual(12, storage2.GetValue(-2I))
        Assert.AreEqual(11, storage2.GetValue(-1I))
        Assert.AreEqual(0, storage2.GetValue(0I))
        Assert.AreEqual(0, storage2.GetValue(1I))
        // storage of string in 1..3 range
        let storage3 = SafeStorage<string>(1, 3, "")
        storage3.Storage.[0]<-"iddqd"
        storage3.Storage.[1]<-"idkfa"
        storage3.Storage.[2]<-"idclip"
        Assert.AreEqual("", storage3.GetValue(-1))
        Assert.AreEqual("", storage3.GetValue(0))
        Assert.AreEqual("iddqd", storage3.GetValue(1))
        Assert.AreEqual("idkfa", storage3.GetValue(2))
        Assert.AreEqual("idclip", storage3.GetValue(3))
        Assert.AreEqual("", storage3.GetValue(4))
        Assert.AreEqual("", storage3.GetValue(-1L))
        Assert.AreEqual("", storage3.GetValue(0L))
        Assert.AreEqual("iddqd", storage3.GetValue(1L))
        Assert.AreEqual("idkfa", storage3.GetValue(2L))
        Assert.AreEqual("idclip", storage3.GetValue(3L))
        Assert.AreEqual("", storage3.GetValue(4L))
        Assert.AreEqual("", storage3.GetValue(-1I))
        Assert.AreEqual("", storage3.GetValue(0I))
        Assert.AreEqual("iddqd", storage3.GetValue(1I))
        Assert.AreEqual("idkfa", storage3.GetValue(2I))
        Assert.AreEqual("idclip", storage3.GetValue(3I))
        Assert.AreEqual("", storage3.GetValue(4I))

    [<Test>]
    member public this.SetValue() =
        // storage of int in 1..3 range
        let storage1 = SafeStorage<int>(1, 3, 0)
        Assert.AreEqual([|0; 0; 0|], storage1.Storage)
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
        Assert.AreEqual([|11; 12; 13|], storage1.Storage)
        // storage of int in -3..-1 range
        let storage2 = SafeStorage<int>(-3, -1, 0)
        Assert.AreEqual([|0; 0; 0|], storage2.Storage)
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
        Assert.AreEqual([|13; 12; 11|], storage2.Storage)
        // storage of string in 1..3 range
        let storage3 = SafeStorage<string>(1, 3, "")
        Assert.AreEqual([|""; ""; ""|], storage3.Storage)
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
        Assert.AreEqual([|"iddqd"; "idkfa"; "idclip"|], storage3.Storage)