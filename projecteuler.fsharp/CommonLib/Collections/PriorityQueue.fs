namespace CommonLib.Collections

open System

// TODO (std_string) : think about moving binary heap into separate data type
type PriorityQueue<'TItem> public(less: 'TItem * 'TItem -> bool) =

    let binaryHeap = ResizeArray<'TItem>()
    do
        // first entry isn't used
        Unchecked.defaultof<'TItem> |> binaryHeap.Add

    member public this.Insert(item: 'TItem) =
        item |> binaryHeap.Add
        binaryHeap.Count - 1 |> this.Swim

    member public this.GetTop =
        if binaryHeap.Count = 1 then
            raise (InvalidOperationException())
        binaryHeap.[1]

    member public this.ExtraxtTop() =
        if binaryHeap.Count = 1 then
            raise (InvalidOperationException())
        let top = binaryHeap.[1]
        binaryHeap.[1] <- binaryHeap.[binaryHeap.Count - 1]
        binaryHeap.RemoveAt(binaryHeap.Count - 1)
        1 |> this.Sink
        top

    member public this.IsEmpty = binaryHeap.Count = 1

    // binary heap interface

    // get parent index
    member private this.GetParent(current: int) = current / 2

    // get left child index
    member private this.GetLeftChild(current: int) = 2 * current

    // get right child index
    member private this.GetRightChild(current: int) = 2 * current + 1

    // is index valid
    member private this.IsValid(current: int) = (current > 0) && (current < binaryHeap.Count - 1)

    // from https://algs4.cs.princeton.edu/24pq/
    member private this.Swim(current: int) =
        let parent = current |> this.GetParent
        match parent with
        | _ when parent |> this.IsValid |> not -> ()
        | _ when less(binaryHeap.[parent], binaryHeap.[current]) |> not -> ()
        | _ ->
            this.Exchange(parent, current)
            parent |> this.Swim

    // from https://algs4.cs.princeton.edu/24pq/
    member private this.Sink(current: int) =
        let leftChild = current |> this.GetLeftChild
        let rightChild = leftChild + 1
        match leftChild with
        | _ when leftChild |> this.IsValid |> not -> ()
        | _ when (rightChild |> this.IsValid) && less(binaryHeap.[leftChild], binaryHeap.[rightChild]) ->
            match less(binaryHeap.[current], binaryHeap.[rightChild]) with
            | false -> ()
            | true ->
                this.Exchange(current, rightChild)
                rightChild |> this.Sink
        | _ ->
            match less(binaryHeap.[current], binaryHeap.[leftChild]) with
            | false -> ()
            | true ->
                this.Exchange(current, leftChild)
                leftChild |> this.Sink

    member private this.Exchange(left: int, right: int) =
        let value = binaryHeap.[left]
        binaryHeap.[left] <- binaryHeap.[right]
        binaryHeap.[right] <- value

type MaxPriorityQueue<'TItem> public(compare: 'TItem * 'TItem -> int) =

    let priorityQueue = PriorityQueue(fun (left, right) -> compare(left, right) < 0)

    member public this.Insert(item: 'TItem) =
        item |> priorityQueue.Insert

    member public this.GetMax = priorityQueue.GetTop

    member public this.ExtractMax() = priorityQueue.ExtraxtTop()

    member public this.IsEmpty = priorityQueue.IsEmpty

type MinPriorityQueue<'TItem> public(compare: 'TItem * 'TItem -> int) =

    let priorityQueue = PriorityQueue(fun (left, right) -> compare(left, right) > 0)

    member public this.Insert(item: 'TItem) =
        item |> priorityQueue.Insert

    member public this.GetMin = priorityQueue.GetTop

    member public this.ExtractMin() = priorityQueue.ExtraxtTop()

    member public this.IsEmpty = priorityQueue.IsEmpty