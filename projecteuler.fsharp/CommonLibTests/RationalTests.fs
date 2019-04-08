namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib.Rational

[<TestFixture>]
type RationalTests() =

    [<Test>]
    member public this.Create() =
        // process bad args
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber(1I, 0I) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber(1L, 0L) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber(1, 0) |> ignore) |> ignore
        // integer number
        this.Check(2I, 1I, new RationalNumber(2I))
        this.Check(-2I, 1I, new RationalNumber(-2I))
        this.Check(2I, 1I, new RationalNumber(2L))
        this.Check(-2I, 1I, new RationalNumber(-2L))
        this.Check(2I, 1I, new RationalNumber(2))
        this.Check(-2I, 1I, new RationalNumber(-2))
        // reduced rational number (into integer):
        this.Check(2I, 1I, new RationalNumber(4I, 2I))
        this.Check(-2I, 1I, new RationalNumber(-4I, 2I))
        this.Check(-2I, 1I, new RationalNumber(4I, -2I))
        this.Check(2I, 1I, new RationalNumber(-4I, -2I))
        this.Check(2I, 1I, new RationalNumber(4L, 2L))
        this.Check(-2I, 1I, new RationalNumber(-4L, 2L))
        this.Check(-2I, 1I, new RationalNumber(4L, -2L))
        this.Check(2I, 1I, new RationalNumber(-4L, -2L))
        this.Check(2I, 1I, new RationalNumber(4, 2))
        this.Check(-2I, 1I, new RationalNumber(-4, 2))
        this.Check(-2I, 1I, new RationalNumber(4, -2))
        this.Check(2I, 1I, new RationalNumber(-4, -2))
        // reduced rational number (into 1 / D form):
        this.Check(1I, 2I, new RationalNumber(2I, 4I))
        this.Check(-1I, 2I, new RationalNumber(-2I, 4I))
        this.Check(-1I, 2I, new RationalNumber(2I, -4I))
        this.Check(1I, 2I, new RationalNumber(-2I, -4I))
        this.Check(1I, 2I, new RationalNumber(2L, 4L))
        this.Check(-1I, 2I, new RationalNumber(-2L, 4L))
        this.Check(-1I, 2I, new RationalNumber(2L, -4L))
        this.Check(1I, 2I, new RationalNumber(-2L, -4L))
        this.Check(1I, 2I, new RationalNumber(2, 4))
        this.Check(-1I, 2I, new RationalNumber(-2, 4))
        this.Check(-1I, 2I, new RationalNumber(2, -4))
        this.Check(1I, 2I, new RationalNumber(-2, -4))
        // rational number
        this.Check(3I, 7I, new RationalNumber(3I, 7I))
        this.Check(-3I, 7I, new RationalNumber(-3I, 7I))
        this.Check(-3I, 7I, new RationalNumber(3I, -7I))
        this.Check(3I, 7I, new RationalNumber(-3I, -7I))
        this.Check(6I, 9I, new RationalNumber(6I, 9I))
        this.Check(-6I, 9I, new RationalNumber(-6I, 9I))
        this.Check(-6I, 9I, new RationalNumber(6I, -9I))
        this.Check(6I, 9I, new RationalNumber(-6I, -9I))
        this.Check(3I, 7I, new RationalNumber(3L, 7L))
        this.Check(-3I, 7I, new RationalNumber(-3L, 7L))
        this.Check(-3I, 7I, new RationalNumber(3L, -7L))
        this.Check(3I, 7I, new RationalNumber(-3L, -7L))
        this.Check(6I, 9I, new RationalNumber(6L, 9L))
        this.Check(-6I, 9I, new RationalNumber(-6L, 9L))
        this.Check(-6I, 9I, new RationalNumber(6L, -9L))
        this.Check(6I, 9I, new RationalNumber(-6L, -9L))
        this.Check(3I, 7I, new RationalNumber(3, 7))
        this.Check(-3I, 7I, new RationalNumber(-3, 7))
        this.Check(-3I, 7I, new RationalNumber(3, -7))
        this.Check(3I, 7I, new RationalNumber(-3, -7))
        this.Check(6I, 9I, new RationalNumber(6, 9))
        this.Check(-6I, 9I, new RationalNumber(-6, 9))
        this.Check(-6I, 9I, new RationalNumber(6, -9))
        this.Check(6I, 9I, new RationalNumber(-6, -9))

    [<Test>]
    member public this.IsZero() =
        Assert.IsTrue((new RationalNumber(0, 1)).IsZero)
        Assert.IsFalse((new RationalNumber(2, 1)).IsZero)
        Assert.IsFalse((new RationalNumber(2, 3)).IsZero)
        Assert.IsFalse((new RationalNumber(-2, 1)).IsZero)
        Assert.IsFalse((new RationalNumber(-2, 3)).IsZero)

    [<Test>]
    member public this.IsInteger() =
        Assert.IsTrue((new RationalNumber(0, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber(1, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber(2, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber(-1, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber(-2, 1)).IsInteger)
        Assert.IsFalse((new RationalNumber(1, 2)).IsInteger)
        Assert.IsFalse((new RationalNumber(2, 3)).IsInteger)
        Assert.IsFalse((new RationalNumber(-1, 2)).IsInteger)
        Assert.IsFalse((new RationalNumber(-2, 3)).IsInteger)

    [<Test>]
    member public this.Reverse() =
        this.Check(1I, 2I, (new RationalNumber(2I)).Reverse())
        this.Check(-1I, 2I, (new RationalNumber(-2I)).Reverse())
        this.Check(2I, 1I, (new RationalNumber(1I, 2I)).Reverse())
        this.Check(-2I, 1I, (new RationalNumber(-1I, 2I)).Reverse())
        this.Check(3I, 2I, (new RationalNumber(2I, 3I)).Reverse())
        this.Check(-3I, 2I, (new RationalNumber(-2I, 3I)).Reverse())
        this.Check(2I, 3I, (new RationalNumber(3I, 2I)).Reverse())
        this.Check(-2I, 3I, (new RationalNumber(-3I, 2I)).Reverse())

    [<Test>]
    member public this.Simplify() =
        this.Check(2I, 1I, (new RationalNumber(2I)).Simplify())
        this.Check(-2I, 1I, (new RationalNumber(-2I)).Simplify())
        this.Check(2I, 3I, (new RationalNumber(2I, 3I)).Simplify())
        this.Check(-2I, 3I, (new RationalNumber(-2I, 3I)).Simplify())
        this.Check(2I, 9I, (new RationalNumber(4I, 18I)).Simplify())
        this.Check(-2I, 9I, (new RationalNumber(-4I, 18I)).Simplify())

    [<Test>]
    member public this.OpAdd() =
        // two integer numbers
        this.Check(5I, 1I, new RationalNumber(2I) + new RationalNumber(3I))
        this.Check(-1I, 1I, new RationalNumber(2I) + new RationalNumber(-3I))
        this.Check(1I, 1I, new RationalNumber(-2I) + new RationalNumber(3I))
        this.Check(-5I, 1I, new RationalNumber(-2I) + new RationalNumber(-3I))
        this.Check(5I, 1I, new RationalNumber(3I) + new RationalNumber(2I))
        this.Check(1I, 1I, new RationalNumber(3I) + new RationalNumber(-2I))
        this.Check(-1I, 1I, new RationalNumber(-3I) + new RationalNumber(2I))
        this.Check(-5I, 1I, new RationalNumber(-3I) + new RationalNumber(-2I))
        // integer & rational numbers
        this.Check(12I, 5I, new RationalNumber(2I) + new RationalNumber(2I, 5I))
        this.Check(8I, 5I, new RationalNumber(2I) + new RationalNumber(-2I, 5I))
        this.Check(-8I, 5I, new RationalNumber(-2I) + new RationalNumber(2I, 5I))
        this.Check(-12I, 5I, new RationalNumber(-2I) + new RationalNumber(-2I, 5I))
        this.Check(12I, 5I, new RationalNumber(2I, 5I) + new RationalNumber(2I))
        this.Check(-8I, 5I, new RationalNumber(2I, 5I) + new RationalNumber(-2I))
        this.Check(8I, 5I, new RationalNumber(-2I, 5I) + new RationalNumber(2I))
        this.Check(-12I, 5I, new RationalNumber(-2I, 5I) + new RationalNumber(-2I))
        // two rational numbers
        this.Check(11I, 15I, new RationalNumber(1I, 3I) + new RationalNumber(2I, 5I))
        this.Check(-1I, 15I, new RationalNumber(1I, 3I) + new RationalNumber(-2I, 5I))
        this.Check(1I, 15I, new RationalNumber(-1I, 3I) + new RationalNumber(2I, 5I))
        this.Check(-11I, 15I, new RationalNumber(-1I, 3I) + new RationalNumber(-2I, 5I))
        this.Check(11I, 15I, new RationalNumber(2I, 5I) + new RationalNumber(1I, 3I))
        this.Check(1I, 15I, new RationalNumber(2I, 5I) + new RationalNumber(-1I, 3I))
        this.Check(-1I, 15I, new RationalNumber(-2I, 5I) + new RationalNumber(1I, 3I))
        this.Check(-11I, 15I, new RationalNumber(-2I, 5I) + new RationalNumber(-1I, 3I))
        this.Check(6I, 9I, new RationalNumber(1I, 9I) + new RationalNumber(5I, 9I))
        this.Check(6I, 9I, new RationalNumber(5I, 9I) + new RationalNumber(1I, 9I))
        this.Check(12I, 18I, new RationalNumber(4I, 9I) + new RationalNumber(4I, 18I))
        this.Check(12I, 18I, new RationalNumber(4I, 18I) + new RationalNumber(4I, 9I))

    [<Test>]
    member public this.OpSub() =
        // two integer numbers
        this.Check(-1I, 1I, new RationalNumber(2I) - new RationalNumber(3I))
        this.Check(5I, 1I, new RationalNumber(2I) - new RationalNumber(-3I))
        this.Check(-5I, 1I, new RationalNumber(-2I) - new RationalNumber(3I))
        this.Check(1I, 1I, new RationalNumber(-2I) - new RationalNumber(-3I))
        this.Check(1I, 1I, new RationalNumber(3I) - new RationalNumber(2I))
        this.Check(5I, 1I, new RationalNumber(3I) - new RationalNumber(-2I))
        this.Check(-5I, 1I, new RationalNumber(-3I) - new RationalNumber(2I))
        this.Check(-1I, 1I, new RationalNumber(-3I) - new RationalNumber(-2I))
        // integer & rational numbers
        this.Check(8I, 5I, new RationalNumber(2I) - new RationalNumber(2I, 5I))
        this.Check(12I, 5I, new RationalNumber(2I) - new RationalNumber(-2I, 5I))
        this.Check(-12I, 5I, new RationalNumber(-2I) - new RationalNumber(2I, 5I))
        this.Check(-8I, 5I, new RationalNumber(-2I) - new RationalNumber(-2I, 5I))
        this.Check(-8I, 5I, new RationalNumber(2I, 5I) - new RationalNumber(2I))
        this.Check(12I, 5I, new RationalNumber(2I, 5I) - new RationalNumber(-2I))
        this.Check(-12I, 5I, new RationalNumber(-2I, 5I) - new RationalNumber(2I))
        this.Check(8I, 5I, new RationalNumber(-2I, 5I) - new RationalNumber(-2I))
        // two rational numbers
        this.Check(-1I, 15I, new RationalNumber(1I, 3I) - new RationalNumber(2I, 5I))
        this.Check(11I, 15I, new RationalNumber(1I, 3I) - new RationalNumber(-2I, 5I))
        this.Check(-11I, 15I, new RationalNumber(-1I, 3I) - new RationalNumber(2I, 5I))
        this.Check(1I, 15I, new RationalNumber(-1I, 3I) - new RationalNumber(-2I, 5I))
        this.Check(1I, 15I, new RationalNumber(2I, 5I) - new RationalNumber(1I, 3I))
        this.Check(11I, 15I, new RationalNumber(2I, 5I) - new RationalNumber(-1I, 3I))
        this.Check(-11I, 15I, new RationalNumber(-2I, 5I) - new RationalNumber(1I, 3I))
        this.Check(-1I, 15I, new RationalNumber(-2I, 5I) - new RationalNumber(-1I, 3I))
        this.Check(6I, 9I, new RationalNumber(7I, 9I) - new RationalNumber(1I, 9I))
        this.Check(-6I, 9I, new RationalNumber(1I, 9I) - new RationalNumber(7I, 9I))
        this.Check(12I, 18I, new RationalNumber(14I, 18I) - new RationalNumber(1I, 9I))
        this.Check(-12I, 18I, new RationalNumber(1I, 9I) - new RationalNumber(14I, 18I))

    [<Test>]
    member public this.OpMult() =
        // two integer numbers
        this.Check(6I, 1I, new RationalNumber(2I) * new RationalNumber(3I))
        this.Check(-6I, 1I, new RationalNumber(2I) * new RationalNumber(-3I))
        this.Check(-6I, 1I, new RationalNumber(-2I) * new RationalNumber(3I))
        this.Check(6I, 1I, new RationalNumber(-2I) * new RationalNumber(-3I))
        this.Check(6I, 1I, new RationalNumber(3I) * new RationalNumber(2I))
        this.Check(-6I, 1I, new RationalNumber(3I) * new RationalNumber(-2I))
        this.Check(-6I, 1I, new RationalNumber(-3I) * new RationalNumber(2I))
        this.Check(6I, 1I, new RationalNumber(-3I) * new RationalNumber(-2I))
        // integer & rational numbers
        this.Check(4I, 5I, new RationalNumber(2I) * new RationalNumber(2I, 5I))
        this.Check(-4I, 5I, new RationalNumber(2I) * new RationalNumber(-2I, 5I))
        this.Check(-4I, 5I, new RationalNumber(-2I) * new RationalNumber(2I, 5I))
        this.Check(4I, 5I, new RationalNumber(-2I) * new RationalNumber(-2I, 5I))
        this.Check(24I, 18I, new RationalNumber(2I) * new RationalNumber(12I, 18I))
        this.Check(-24I, 18I, new RationalNumber(2I) * new RationalNumber(-12I, 18I))
        this.Check(-24I, 18I, new RationalNumber(-2I) * new RationalNumber(12I, 18I))
        this.Check(24I, 18I, new RationalNumber(-2I) * new RationalNumber(-12I, 18I))
        // two rational numbers
        this.Check(6I, 35I, new RationalNumber(3I, 7I) * new RationalNumber(2I, 5I))
        this.Check(-6I, 35I, new RationalNumber(3I, 7I) * new RationalNumber(-2I, 5I))
        this.Check(-6I, 35I, new RationalNumber(-3I, 7I) * new RationalNumber(2I, 5I))
        this.Check(6I, 35I, new RationalNumber(-3I, 7I) * new RationalNumber(-2I, 5I))
        this.Check(6I, 35I, new RationalNumber(2I, 5I) * new RationalNumber(3I, 7I))
        this.Check(-6I, 35I, new RationalNumber(2I, 5I) * new RationalNumber(-3I, 7I))
        this.Check(-6I, 35I, new RationalNumber(-2I, 5I) * new RationalNumber(3I, 7I))
        this.Check(6I, 35I, new RationalNumber(-2I, 5I) * new RationalNumber(-3I, 7I))
        this.Check(6I, 45I, new RationalNumber(2I, 9I) * new RationalNumber(3I, 5I))
        this.Check(-6I, 45I, new RationalNumber(2I, 9I) * new RationalNumber(-3I, 5I))
        this.Check(-6I, 45I, new RationalNumber(-2I, 9I) * new RationalNumber(3I, 5I))
        this.Check(6I, 45I, new RationalNumber(-2I, 9I) * new RationalNumber(-3I, 5I))
        this.Check(6I, 45I, new RationalNumber(3I, 5I) * new RationalNumber(2I, 9I))
        this.Check(-6I, 45I, new RationalNumber(3I, 5I) * new RationalNumber(-2I, 9I))
        this.Check(-6I, 45I, new RationalNumber(-3I, 5I) * new RationalNumber(2I, 9I))
        this.Check(6I, 45I, new RationalNumber(-3I, 5I) * new RationalNumber(-2I, 9I))
        this.Check(1I, 2I, new RationalNumber(2I, 3I) * new RationalNumber(3I, 4I))
        this.Check(1I, 2I, new RationalNumber(3I, 4I) * new RationalNumber(2I, 3I))
        this.Check(2I, 1I, new RationalNumber(4I, 3I) * new RationalNumber(3I, 2I))
        this.Check(2I, 1I, new RationalNumber(3I, 2I) * new RationalNumber(4I, 3I))

    [<Test>]
    member public this.OpDiv() =
        // two integer numbers
        this.Check(2I, 1I, new RationalNumber(4I) / new RationalNumber(2I))
        this.Check(-2I, 1I, new RationalNumber(4I) / new RationalNumber(-2I))
        this.Check(-2I, 1I, new RationalNumber(-4I) / new RationalNumber(2I))
        this.Check(2I, 1I, new RationalNumber(-4I) / new RationalNumber(-2I))
        this.Check(1I, 2I, new RationalNumber(2I) / new RationalNumber(4I))
        this.Check(-1I, 2I, new RationalNumber(2I) / new RationalNumber(-4I))
        this.Check(-1I, 2I, new RationalNumber(-2I) / new RationalNumber(4I))
        this.Check(1I, 2I, new RationalNumber(-2I) / new RationalNumber(-4I))
        this.Check(3I, 2I, new RationalNumber(3I) / new RationalNumber(2I))
        this.Check(-3I, 2I, new RationalNumber(3I) / new RationalNumber(-2I))
        this.Check(-3I, 2I, new RationalNumber(-3I) / new RationalNumber(2I))
        this.Check(3I, 2I, new RationalNumber(-3I) / new RationalNumber(-2I))
        this.Check(2I, 3I, new RationalNumber(2I) / new RationalNumber(3I))
        this.Check(-2I, 3I, new RationalNumber(2I) / new RationalNumber(-3I))
        this.Check(-2I, 3I, new RationalNumber(-2I) / new RationalNumber(3I))
        this.Check(2I, 3I, new RationalNumber(-2I) / new RationalNumber(-3I))
        // integer & rational numbers
        this.Check(3I, 1I, new RationalNumber(2I) / new RationalNumber(2I, 3I))
        this.Check(-3I, 1I, new RationalNumber(2I) / new RationalNumber(-2I, 3I))
        this.Check(-3I, 1I, new RationalNumber(-2I) / new RationalNumber(2I, 3I))
        this.Check(3I, 1I, new RationalNumber(-2I) / new RationalNumber(-2I, 3I))
        this.Check(1I, 3I, new RationalNumber(2I, 3I) / new RationalNumber(2I))
        this.Check(-1I, 3I, new RationalNumber(2I, 3I) / new RationalNumber(-2I))
        this.Check(-1I, 3I, new RationalNumber(-2I, 3I) / new RationalNumber(2I))
        this.Check(1I, 3I, new RationalNumber(-2I, 3I) / new RationalNumber(-2I))
        // two rational numbers
        this.Check(6I, 10I, new RationalNumber(2I, 5I) / new RationalNumber(2I, 3I))
        this.Check(-6I, 10I, new RationalNumber(2I, 5I) / new RationalNumber(-2I, 3I))
        this.Check(-6I, 10I, new RationalNumber(-2I, 5I) / new RationalNumber(2I, 3I))
        this.Check(6I, 10I, new RationalNumber(-2I, 5I) / new RationalNumber(-2I, 3I))
        this.Check(10I, 6I, new RationalNumber(2I, 3I) / new RationalNumber(2I, 5I))
        this.Check(-10I, 6I, new RationalNumber(2I, 3I) / new RationalNumber(-2I, 5I))
        this.Check(-10I, 6I, new RationalNumber(-2I, 3I) / new RationalNumber(2I, 5I))
        this.Check(10I, 6I, new RationalNumber(-2I, 3I) / new RationalNumber(-2I, 5I))
        this.Check(2I, 1I, new RationalNumber(4I, 3I) / new RationalNumber(2I, 3I))
        this.Check(-2I, 1I, new RationalNumber(4I, 3I) / new RationalNumber(-2I, 3I))
        this.Check(-2I, 1I, new RationalNumber(-4I, 3I) / new RationalNumber(2I, 3I))
        this.Check(2I, 1I, new RationalNumber(-4I, 3I) / new RationalNumber(-2I, 3I))
        this.Check(1I, 2I, new RationalNumber(2I, 3I) / new RationalNumber(4I, 3I))
        this.Check(-1I, 2I, new RationalNumber(2I, 3I) / new RationalNumber(-4I, 3I))
        this.Check(-1I, 2I, new RationalNumber(-2I, 3I) / new RationalNumber(4I, 3I))
        this.Check(1I, 2I, new RationalNumber(-2I, 3I) / new RationalNumber(-4I, 3I))

    [<Test>]
    member public this.Compare() =
        // two integer numbers
        Assert.IsTrue(new RationalNumber(2I) < new RationalNumber(3I))
        Assert.AreEqual(-1, (new RationalNumber(2I) :> IComparable).CompareTo(new RationalNumber(3I)))
        Assert.IsTrue(new RationalNumber(-3I) < new RationalNumber(-2I))
        Assert.AreEqual(-1, (new RationalNumber(-3I) :> IComparable).CompareTo(new RationalNumber(-2I)))
        Assert.IsTrue(new RationalNumber(2I) = new RationalNumber(2I))
        Assert.AreEqual(0, (new RationalNumber(2I) :> IComparable).CompareTo(new RationalNumber(2I)))
        Assert.IsTrue(new RationalNumber(-2I) = new RationalNumber(-2I))
        Assert.AreEqual(0, (new RationalNumber(-2I) :> IComparable).CompareTo(new RationalNumber(-2I)))
        Assert.IsTrue(new RationalNumber(3I) > new RationalNumber(2I))
        Assert.AreEqual(1, (new RationalNumber(3I) :> IComparable).CompareTo(new RationalNumber(2I)))
        Assert.IsTrue(new RationalNumber(-2I) > new RationalNumber(-3I))
        Assert.AreEqual(1, (new RationalNumber(-2I) :> IComparable).CompareTo(new RationalNumber(-3I)))
        // integer & rational numbers
        Assert.IsTrue(new RationalNumber(2I) < new RationalNumber(7I, 3I))
        Assert.AreEqual(-1, (new RationalNumber(2I) :> IComparable).CompareTo(new RationalNumber(7I, 3I)))
        Assert.IsTrue(new RationalNumber(7I, 3I) < new RationalNumber(3I))
        Assert.AreEqual(-1, (new RationalNumber(7I, 3I) :> IComparable).CompareTo(new RationalNumber(3I)))
        Assert.IsTrue(new RationalNumber(-7I, 3I) < new RationalNumber(-2I))
        Assert.AreEqual(-1, (new RationalNumber(-7I, 3I) :> IComparable).CompareTo(new RationalNumber(-2I)))
        Assert.IsTrue(new RationalNumber(-3I) < new RationalNumber(-7I, 3I))
        Assert.AreEqual(-1, (new RationalNumber(-3I) :> IComparable).CompareTo(new RationalNumber(-7I, 3I)))
        Assert.IsTrue(new RationalNumber(7I, 3I) > new RationalNumber(2I))
        Assert.AreEqual(1, (new RationalNumber(7I, 3I) :> IComparable).CompareTo(new RationalNumber(2I)))
        Assert.IsTrue(new RationalNumber(3I) > new RationalNumber(7I, 3I))
        Assert.AreEqual(1, (new RationalNumber(3I) :> IComparable).CompareTo(new RationalNumber(7I, 3I)))
        Assert.IsTrue(new RationalNumber(-2I) > new RationalNumber(-7I, 3I))
        Assert.AreEqual(1, (new RationalNumber(-2I) :> IComparable).CompareTo(new RationalNumber(-7I, 3I)))
        Assert.IsTrue(new RationalNumber(-7I, 3I) > new RationalNumber(-3I))
        Assert.AreEqual(1, (new RationalNumber(-7I, 3I) :> IComparable).CompareTo(new RationalNumber(-3I)))
        // two rational numbers
        Assert.IsTrue(new RationalNumber(1I, 3I) < new RationalNumber(1I, 2I))
        Assert.AreEqual(-1, (new RationalNumber(1I, 3I) :> IComparable).CompareTo(new RationalNumber(1I, 2I)))
        Assert.IsTrue(new RationalNumber(-1I, 2I) < new RationalNumber(-1I, 3I))
        Assert.AreEqual(-1, (new RationalNumber(-1I, 2I) :> IComparable).CompareTo(new RationalNumber(-1I, 3I)))
        Assert.IsTrue(new RationalNumber(2I, 5I) = new RationalNumber(2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(2I, 5I) :> IComparable).CompareTo(new RationalNumber(2I, 5I)))
        Assert.IsTrue(new RationalNumber(-2I, 5I) = new RationalNumber(-2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(-2I, 5I) :> IComparable).CompareTo(new RationalNumber(-2I, 5I)))
        Assert.IsTrue(new RationalNumber(2I, 5I) = new RationalNumber(4I, 10I))
        Assert.AreEqual(0, (new RationalNumber(2I, 5I) :> IComparable).CompareTo(new RationalNumber(4I, 10I)))
        Assert.IsTrue(new RationalNumber(-2I, 5I) = new RationalNumber(-4I, 10I))
        Assert.AreEqual(0, (new RationalNumber(-2I, 5I) :> IComparable).CompareTo(new RationalNumber(-4I, 10I)))
        Assert.IsTrue(new RationalNumber(4I, 10I) = new RationalNumber(2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(4I, 10I) :> IComparable).CompareTo(new RationalNumber(2I, 5I)))
        Assert.IsTrue(new RationalNumber(-4I, 10I) = new RationalNumber(-2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(-4I, 10I) :> IComparable).CompareTo(new RationalNumber(-2I, 5I)))
        Assert.IsTrue(new RationalNumber(1I, 2I) > new RationalNumber(1I, 3I))
        Assert.AreEqual(1, (new RationalNumber(1I, 2I) :> IComparable).CompareTo(new RationalNumber(1I, 3I)))
        Assert.IsTrue(new RationalNumber(-1I, 3I) > new RationalNumber(-1I, 2I))
        Assert.AreEqual(1, (new RationalNumber(-1I, 3I) :> IComparable).CompareTo(new RationalNumber(-1I, 2I)))


    [<Test>]
    member public this.EqualsAndGetHashCode() =
        // two integer numbers
        Assert.IsTrue((new RationalNumber(2I)).Equals(new RationalNumber(2I)))
        Assert.AreEqual((new RationalNumber(2I)).GetHashCode(), (new RationalNumber(2I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-2I)).Equals(new RationalNumber(-2I)))
        Assert.AreEqual((new RationalNumber(-2I)).GetHashCode(), (new RationalNumber(-2I)).GetHashCode())
        Assert.IsFalse((new RationalNumber(2I)).Equals(new RationalNumber(3I)))
        Assert.IsFalse((new RationalNumber(3I)).Equals(new RationalNumber(2I)))
        Assert.IsFalse((new RationalNumber(-2I)).Equals(new RationalNumber(-3I)))
        Assert.IsFalse((new RationalNumber(-3I)).Equals(new RationalNumber(-2I)))
        // integer & rational numbers
        Assert.IsFalse((new RationalNumber(2I)).Equals(new RationalNumber(2I, 3I)))
        Assert.IsFalse((new RationalNumber(2I, 3I)).Equals(new RationalNumber(2I)))
        Assert.IsFalse((new RationalNumber(-2I)).Equals(new RationalNumber(-2I, 3I)))
        Assert.IsFalse((new RationalNumber(-2I, 3I)).Equals(new RationalNumber(-2I)))
        // two rational numbers
        Assert.IsTrue((new RationalNumber(1I, 2I)).Equals(new RationalNumber(1I, 2I)))
        Assert.AreEqual((new RationalNumber(1I, 2I)).GetHashCode(), (new RationalNumber(1I, 2I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-1I, 2I)).Equals(new RationalNumber(-1I, 2I)))
        Assert.AreEqual((new RationalNumber(-1I, 2I)).GetHashCode(), (new RationalNumber(-1I, 2I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(2I, 5I)).Equals(new RationalNumber(4I, 10I)))
        Assert.AreEqual((new RationalNumber(2I, 5I)).GetHashCode(), (new RationalNumber(4I, 10I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-2I, 5I)).Equals(new RationalNumber(-4I, 10I)))
        Assert.AreEqual((new RationalNumber(-2I, 5I)).GetHashCode(), (new RationalNumber(-4I, 10I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(4I, 10I)).Equals(new RationalNumber(2I, 5I)))
        Assert.AreEqual((new RationalNumber(4I, 10I)).GetHashCode(), (new RationalNumber(2I, 5I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-4I, 10I)).Equals(new RationalNumber(-2I, 5I)))
        Assert.AreEqual((new RationalNumber(-4I, 10I)).GetHashCode(), (new RationalNumber(-2I, 5I)).GetHashCode())
        Assert.IsFalse((new RationalNumber(1I, 2I)).Equals(new RationalNumber(1I, 3I)))
        Assert.IsFalse((new RationalNumber(-1I, 2I)).Equals(new RationalNumber(-1I, 3I)))
        Assert.IsFalse((new RationalNumber(1I, 3I)).Equals(new RationalNumber(1I, 2I)))
        Assert.IsFalse((new RationalNumber(-1I, 3I)).Equals(new RationalNumber(-1I, 2I)))
        // rational number and other object
        Assert.IsFalse((new RationalNumber(1I, 2I)).Equals(0.5))
        Assert.IsFalse((0.5).Equals(new RationalNumber(1I, 2I)))
        Assert.IsFalse((new RationalNumber(1I, 2I)).Equals("0.5"))
        Assert.IsFalse("0.5".Equals(new RationalNumber(1I, 2I)))

    member private this.Check(extectedNumerator: int, expectedDenominator: int, actualNumber: RationalNumber) =
        this.Check(extectedNumerator |> bigint, expectedDenominator |> bigint, actualNumber)

    member private this.Check(extectedNumerator: int64, expectedDenominator: int64, actualNumber: RationalNumber) =
        this.Check(extectedNumerator |> bigint, expectedDenominator |> bigint, actualNumber)

    member private this.Check(expectedNumerator: bigint, expectedDenominator: bigint, actualNumber: RationalNumber) =
        Assert.AreEqual(expectedNumerator, actualNumber.Numerator)
        Assert.AreEqual(expectedDenominator, actualNumber.Denominator)