namespace CommonLib

open System

[<AbstractClass; Sealed>]
type Permutations =

    static member public GetPermutation(lexicographicalNumber: bigint, alphabet: 'TItem list) =
        Permutations.GetPermutation(lexicographicalNumber, alphabet.Length, alphabet)

    static member public GetPermutation(lexicographicalNumber: bigint, size: int, alphabet: 'TItem list) =
        if lexicographicalNumber < 0I then
            raise (ArgumentOutOfRangeException("lexicographicalNumber"))
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if (size <= 0) || (size > alphabet.Length) then
            raise (ArgumentOutOfRangeException("size"))
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, size)
        if lexicographicalNumber >= lexicographicalNumberSup then
            raise (ArgumentOutOfRangeException("lexicographicalNumber"))
        Permutations.GetPermutation(lexicographicalNumber, lexicographicalNumberSup, size, alphabet, [])

    static member public GetLexicographicalNumber(items: 'TItem list, alphabet: 'TItem list) =
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if items.IsEmpty || (items.Length > alphabet.Length) then
            raise (ArgumentException("items"))
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, items.Length)
        Permutations.GetLexicographicalNumber(items, alphabet, 0I, lexicographicalNumberSup)

    static member public GetLexicographicalNumberSup(alphabet: 'TItem list) =
        Permutations.GetLexicographicalNumberSup(alphabet, alphabet.Length)

    static member public GetLexicographicalNumberSup(alphabet: 'TItem list, size: int) =
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if (size <= 0) || (size > alphabet.Length) then
            raise (ArgumentOutOfRangeException("size"))
        seq{0 .. size - 1} |> Seq.fold (fun result number -> (alphabet.Length - number) |> bigint |> (*) result) 1I

    static member public GetNextPermutation(currentPermutation: 'TItem list, alphabet: 'TItem list) =
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, currentPermutation.Length)
        let currentLexicographicalNumber = Permutations.GetLexicographicalNumber(currentPermutation, alphabet)
        if currentLexicographicalNumber + 1I = lexicographicalNumberSup then
            raise (ArgumentException("currentPermutation"))
        Permutations.GetPermutation(currentLexicographicalNumber + 1I, alphabet)

    static member public GetPrevPermutation(currentPermutation: 'TItem list, alphabet: 'TItem list) =
        let currentLexicographicalNumber = Permutations.GetLexicographicalNumber(currentPermutation, alphabet)
        if currentLexicographicalNumber = 0I then
            raise (ArgumentException("currentPermutation"))
        Permutations.GetPermutation(currentLexicographicalNumber - 1I, alphabet)

    static member private GetPermutation(lexicographicalNumber: bigint, lexicographicalNumberSup: bigint, size: int, alphabet: 'TItem list, items: 'TItem list) =
        match items, lexicographicalNumber with
        | _ when items.Length = size -> items |> List.rev
        | _ when lexicographicalNumber = 0I -> (items |> List.rev) @ (alphabet.[..size - items.Length - 1])
        | _ ->
            let newLexicographicalNumberSup = lexicographicalNumberSup / (alphabet.Length |> bigint)
            let item, newAlphabet = Permutations.ExtractElement(alphabet, lexicographicalNumber / newLexicographicalNumberSup |> int)
            Permutations.GetPermutation(lexicographicalNumber % newLexicographicalNumberSup, newLexicographicalNumberSup, size, newAlphabet, item :: items)

    static member private GetLexicographicalNumber(items: 'TItem list, alphabet: 'TItem list, lexicographicalNumber: bigint, lexicographicalNumberSup: bigint) =
        match items with
        | [] -> lexicographicalNumber
        | item :: itemsRest ->
            let newLexicographicalNumberSup = lexicographicalNumberSup / (alphabet.Length |> bigint)
            let index, newAlphabet = Permutations.FindAndExtractElement(alphabet, item)
            let newLexicographicalNumber = lexicographicalNumber + (index |> int |> bigint) * newLexicographicalNumberSup
            Permutations.GetLexicographicalNumber(itemsRest, newAlphabet, newLexicographicalNumber, newLexicographicalNumberSup)

    // TODO (std_string) : think about location
    // TODO (std_string) : think about approach - probably use array
    static member private ExtractElement(items: 'TItem list, index: int) =
        if (index < 0) || (index >= items.Length) then
            raise (ArgumentOutOfRangeException("index"))
        match index with
        | 0 -> items |> List.head, items |> List.tail
        | _ when index = items.Length - 1 -> items.[items.Length - 1], items.[..items.Length - 2]
        | _ -> items.[index], items.[..index - 1] @ items.[index + 1 ..]

    static member private FindAndExtractElement(items: 'TItem list, element: 'TItem) =
        match items |> List.tryFindIndex (fun item -> Object.Equals(item, element)) with
        | None -> raise (ArgumentException("element"))
        | Some index ->
            let _, newItems = Permutations.ExtractElement(items, index)
            index, newItems
