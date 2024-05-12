namespace CommonLib

open System

[<AbstractClass; Sealed>]
type ListUtils =

    static member public GetCirularShift(source: List<'TItem>) =
        match source with
        | [] -> []
        | [item] -> [item]
        | _ -> List.append (source |> List.tail) [source |> List.head]

    static member public ShiftToItem (item: 'TItem, source: 'TItem list) =
        let rec shiftToItemImpl (iteration: int) (count: int) (current: 'TItem list) =
            match iteration with
            | _ when iteration >= count -> raise (new ArgumentException())
            | _ ->
                match current with
                | head :: _ when head = item -> current
                | _ -> current |> ListUtils.GetCirularShift |> shiftToItemImpl (iteration + 1) count
        match source with
        | [] -> raise (new ArgumentException())
        | head :: [] when head <> item -> raise (new ArgumentException())
        | head :: [] when head = item -> source
        | _ ->
            let iterationCount = source.Length
            source |> shiftToItemImpl 0 iterationCount

    static member public GetAllCirularShift(source: List<'TItem>) =
        let _, dest = seq {1 .. source.Length} |> Seq.fold (fun (current, dest) _ -> (ListUtils.GetCirularShift(current), current :: dest)) (source, [])
        dest |> List.rev