namespace CommonLibTests.Collections

open CommonLib.Collections
open NUnit.Framework
open NUnit.Framework.Legacy
open System

module PriorityQueueTestsImpl =
    type SomeData = {FieldA: int; FieldB: string}

    let creareSomeData (valueA: int) (valueB: string) = {SomeData.FieldA = valueA; SomeData.FieldB = valueB}

open PriorityQueueTestsImpl

[<TestFixture>]
type MaxPriorityQueueTests() =

    let insertItems(queue: MaxPriorityQueue<'TItem>) (items: 'TItem[]) =
        items |> Seq.iter (fun item -> item |> queue.Insert)

    let insertItemsWithCheckMax(queue: MaxPriorityQueue<'TItem>) (itemAndMaxPairs: ('TItem * 'TItem)[]) =
        for (item, expectedMax) in itemAndMaxPairs do
            item |> queue.Insert
            ClassicAssert.AreEqual(expectedMax, queue.GetMax)

    let checkExtractMax (queue: MaxPriorityQueue<'TItem>) (expectedMax: 'TItem[]) =
        for max in expectedMax do
            ClassicAssert.AreEqual(max, queue.ExtractMax())
        queue.IsEmpty |> ClassicAssert.IsTrue

    [<Test>]
    member public this.Insert() =
        // max priority queue for int
        let intQueue = MaxPriorityQueue<int>(fun (left, right) -> left - right)
        intQueue.IsEmpty |> ClassicAssert.IsTrue
        [|(1, 1); (4, 4); (3, 4); (13, 13); (11, 13)|] |> insertItemsWithCheckMax intQueue
        intQueue.ExtractMax() |> ignore
        666 |> intQueue.Insert
        ClassicAssert.AreEqual(666, intQueue.GetMax)
        // max priority queue for SomeData
        let someDataQueue = MaxPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        someDataQueue.IsEmpty |> ClassicAssert.IsTrue
        [|(creareSomeData 1 "IDDQD", creareSomeData 1 "IDDQD");
          (creareSomeData 4 "IDKFA", creareSomeData 4 "IDKFA");
          (creareSomeData 3 "IDCLIP", creareSomeData 4 "IDKFA");
          (creareSomeData 13 "IMPULSE 9", creareSomeData 13 "IMPULSE 9");
          (creareSomeData 11 "IMPULSE 666", creareSomeData 13 "IMPULSE 9")|] |> insertItemsWithCheckMax someDataQueue
        someDataQueue.ExtractMax() |> ignore
        creareSomeData 666 "666" |> someDataQueue.Insert
        ClassicAssert.AreEqual(creareSomeData 666 "666", someDataQueue.GetMax)

    [<Test>]
    member public this.GetMax() =
        // max priority queue for int
        let intQueue = MaxPriorityQueue<int>(fun (left, right) -> left - right)
        Assert.Throws<InvalidOperationException>(fun() -> intQueue.GetMax |> ignore) |> ignore
        [|(1, 1); (4, 4); (3, 4); (13, 13); (11, 13)|] |> insertItemsWithCheckMax intQueue
        // max priority queue for SomeData
        let someDataQueue = MaxPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        Assert.Throws<InvalidOperationException>(fun() -> someDataQueue.GetMax |> ignore) |> ignore
        [|(creareSomeData 1 "IDDQD", creareSomeData 1 "IDDQD");
          (creareSomeData 4 "IDKFA", creareSomeData 4 "IDKFA");
          (creareSomeData 3 "IDCLIP", creareSomeData 4 "IDKFA");
          (creareSomeData 13 "IMPULSE 9", creareSomeData 13 "IMPULSE 9");
          (creareSomeData 11 "IMPULSE 666", creareSomeData 13 "IMPULSE 9")|] |> insertItemsWithCheckMax someDataQueue

    [<Test>]
    member public this.ExtractMax() =
        // max priority queue for int
        let intQueue = MaxPriorityQueue<int>(fun (left, right) -> left - right)
        Assert.Throws<InvalidOperationException>(fun() -> intQueue.ExtractMax() |> ignore) |> ignore
        [|1; 4; 3; 13; 11|] |> insertItems intQueue
        [|13; 11; 4; 3; 1|] |> checkExtractMax intQueue
        // max priority queue for SomeData
        let someDataQueue = MaxPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        Assert.Throws<InvalidOperationException>(fun() -> someDataQueue.ExtractMax() |> ignore) |> ignore
        [|creareSomeData 1 "IDDQD"; creareSomeData 4 "IDKFA"; creareSomeData 3 "IDCLIP"; creareSomeData 13 "IMPULSE 9"; creareSomeData 11 "IMPULSE 666"|] |> insertItems someDataQueue
        [|creareSomeData 13 "IMPULSE 9"; creareSomeData 11 "IMPULSE 666"; creareSomeData 4 "IDKFA"; creareSomeData 3 "IDCLIP"; creareSomeData 1 "IDDQD"|] |> checkExtractMax someDataQueue

    [<Test>]
    member public this.IsEmpty() =
        // max priority queue for int
        let intQueue = MaxPriorityQueue<int>(fun (left, right) -> left - right)
        intQueue.IsEmpty |> ClassicAssert.IsTrue
        Assert.DoesNotThrow(fun() -> 666 |> intQueue.Insert)
        intQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> intQueue.GetMax |> ignore)
        intQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> intQueue.ExtractMax() |> ignore)
        intQueue.IsEmpty |> ClassicAssert.IsTrue
        // max priority queue for SomeData
        let someDataQueue = MaxPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        someDataQueue.IsEmpty |> ClassicAssert.IsTrue
        Assert.DoesNotThrow(fun() -> creareSomeData 666 "IDDQD" |> someDataQueue.Insert)
        someDataQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> someDataQueue.GetMax |> ignore)
        someDataQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> someDataQueue.ExtractMax() |> ignore)
        someDataQueue.IsEmpty |> ClassicAssert.IsTrue

[<TestFixture>]
type MinPriorityQueueTests() =

    let insertItems(queue: MinPriorityQueue<'TItem>) (items: 'TItem[]) =
        items |> Seq.iter (fun item -> item |> queue.Insert)

    let insertItemsWithCheckMin(queue: MinPriorityQueue<'TItem>) (itemAndMinPairs: ('TItem * 'TItem)[]) =
        for (item, expectedMin) in itemAndMinPairs do
            item |> queue.Insert
            ClassicAssert.AreEqual(expectedMin, queue.GetMin)

    let checkExtractMin (queue: MinPriorityQueue<'TItem>) (expectedMin: 'TItem[]) =
        for min in expectedMin do
            ClassicAssert.AreEqual(min, queue.ExtractMin())
        queue.IsEmpty |> ClassicAssert.IsTrue

    [<Test>]
    member public this.Insert() =
        // min priority queue for int
        let intQueue = MinPriorityQueue<int>(fun (left, right) -> left - right)
        intQueue.IsEmpty |> ClassicAssert.IsTrue
        [|(11, 11); (13, 11); (3, 3); (4, 3); (1, 1)|] |> insertItemsWithCheckMin intQueue
        intQueue.ExtractMin() |> ignore
        -19 |> intQueue.Insert
        ClassicAssert.AreEqual(-19, intQueue.GetMin)
        // min priority queue for SomeData
        let someDataQueue = MinPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        someDataQueue.IsEmpty |> ClassicAssert.IsTrue
        [|(creareSomeData 11 "IDDQD", creareSomeData 11 "IDDQD");
          (creareSomeData 13 "IDKFA", creareSomeData 11 "IDDQD");
          (creareSomeData 3 "IDCLIP", creareSomeData 3 "IDCLIP");
          (creareSomeData 4 "IMPULSE 9", creareSomeData 3 "IDCLIP");
          (creareSomeData 1 "IMPULSE 666", creareSomeData 1 "IMPULSE 666")|] |> insertItemsWithCheckMin someDataQueue
        someDataQueue.ExtractMin() |> ignore
        creareSomeData -19 "-19" |> someDataQueue.Insert
        ClassicAssert.AreEqual(creareSomeData -19 "-19", someDataQueue.GetMin)

    [<Test>]
    member public this.GetMin() =
        // min priority queue for int
        let intQueue = MinPriorityQueue<int>(fun (left, right) -> left - right)
        Assert.Throws<InvalidOperationException>(fun() -> intQueue.GetMin |> ignore) |> ignore
        [|(11, 11); (13, 11); (3, 3); (4, 3); (1, 1)|] |> insertItemsWithCheckMin intQueue
        // min priority queue for SomeData
        let someDataQueue = MinPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        Assert.Throws<InvalidOperationException>(fun() -> someDataQueue.GetMin |> ignore) |> ignore
        [|(creareSomeData 11 "IDDQD", creareSomeData 11 "IDDQD");
          (creareSomeData 13 "IDKFA", creareSomeData 11 "IDDQD");
          (creareSomeData 3 "IDCLIP", creareSomeData 3 "IDCLIP");
          (creareSomeData 4 "IMPULSE 9", creareSomeData 3 "IDCLIP");
          (creareSomeData 1 "IMPULSE 666", creareSomeData 1 "IMPULSE 666")|] |> insertItemsWithCheckMin someDataQueue

    [<Test>]
    member public this.ExtractMin() =
        // min priority queue for int
        let intQueue = MinPriorityQueue<int>(fun (left, right) -> left - right)
        Assert.Throws<InvalidOperationException>(fun() -> intQueue.ExtractMin() |> ignore) |> ignore
        [|13; 4; 11; 1; 3|] |> insertItems intQueue
        [|1; 3; 4; 11; 13|] |> checkExtractMin intQueue
        // min priority queue for SomeData
        let someDataQueue = MinPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        Assert.Throws<InvalidOperationException>(fun() -> someDataQueue.ExtractMin() |> ignore) |> ignore
        [|creareSomeData 13 "IDDQD"; creareSomeData 4 "IDKFA"; creareSomeData 11 "IDCLIP"; creareSomeData 1 "IMPULSE 9"; creareSomeData 3 "IMPULSE 666"|] |> insertItems someDataQueue
        [|creareSomeData 1 "IMPULSE 9"; creareSomeData 3 "IMPULSE 666"; creareSomeData 4 "IDKFA"; creareSomeData 11 "IDCLIP"; creareSomeData 13 "IDDQD"|] |> checkExtractMin someDataQueue

    [<Test>]
    member public this.IsEmpty() =
        // min priority queue for int
        let intQueue = MinPriorityQueue<int>(fun (left, right) -> left - right)
        intQueue.IsEmpty |> ClassicAssert.IsTrue
        Assert.DoesNotThrow(fun() -> 666 |> intQueue.Insert)
        intQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> intQueue.GetMin |> ignore)
        intQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> intQueue.ExtractMin() |> ignore)
        intQueue.IsEmpty |> ClassicAssert.IsTrue
        // min priority queue for SomeData
        let someDataQueue = MinPriorityQueue<SomeData>(fun (left, right) -> left.FieldA - right.FieldA)
        someDataQueue.IsEmpty |> ClassicAssert.IsTrue
        Assert.DoesNotThrow(fun() -> creareSomeData 666 "IDDQD" |> someDataQueue.Insert)
        someDataQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> someDataQueue.GetMin |> ignore)
        someDataQueue.IsEmpty |> ClassicAssert.IsFalse
        Assert.DoesNotThrow(fun() -> someDataQueue.ExtractMin() |> ignore)
        someDataQueue.IsEmpty |> ClassicAssert.IsTrue