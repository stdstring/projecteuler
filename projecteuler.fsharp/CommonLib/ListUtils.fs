namespace CommonLib

[<AbstractClass; Sealed>]
type ListUtils =

    static member public GetCirularShift(source: List<'TItem>) =
        match source with
        | [] -> []
        | [item] -> [item]
        | _ -> List.append (source |> List.tail) [source |> List.head]

    static member public GetAllCirularShift(source: List<'TItem>) =
        let _, dest = seq {1 .. source.Length} |> Seq.fold (fun (current, dest) _ -> (ListUtils.GetCirularShift(current), current :: dest)) (source, [])
        dest |> List.rev