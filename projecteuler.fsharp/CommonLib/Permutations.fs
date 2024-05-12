namespace CommonLib

open System

[<AbstractClass; Sealed>]
type Permutations =

    static member public GenerateNextPermutationInPlace(currentPermutation: 'TItem[]) =
        // Narayana algorithm
        let mutable j = -1
        for index in seq {0 .. currentPermutation.Length - 2} do
            if currentPermutation.[index] < currentPermutation.[index + 1] then
                j <- index
        match j with
        | -1 -> None
        | _ ->
            let mutable l = j + 1
            for index in seq {j + 1 .. currentPermutation.Length - 1} do
                if currentPermutation.[j] < currentPermutation.[index] then
                    l <- index
            let jValue = currentPermutation.[j]
            currentPermutation.[j] <- currentPermutation.[l]
            let mutable start = j + 1
            let mutable finish = currentPermutation.Length - 1
            while start < finish do
                let temp = currentPermutation.[start]
                currentPermutation.[start] <- currentPermutation.[finish]
                currentPermutation.[finish] <- temp
                start <- start + 1
                finish <- finish - 1
            currentPermutation.[currentPermutation.Length - 1 - l + j + 1] <- jValue
            currentPermutation |> Some

    static member public GetPermutation(lexicographicalNumber: bigint, alphabet: 'TItem list) =
        Permutations.GetPermutation(lexicographicalNumber, alphabet.Length, alphabet)

    static member public GetPermutation(lexicographicalNumber: bigint, size: int, alphabet: 'TItem list) =
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if (size <= 0) || (size > alphabet.Length) then
            raise (ArgumentOutOfRangeException("size"))
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, size)
        match lexicographicalNumber with
        | _ when lexicographicalNumber < 0I -> None
        | _ when lexicographicalNumber >= lexicographicalNumberSup -> None
        | _ -> Permutations.GetPermutation(lexicographicalNumber, lexicographicalNumberSup, size, alphabet, []) |> Some

    static member public GeneratePermutations(alphabet: 'TItem list) =
        Permutations.GeneratePermutations(alphabet.Length, alphabet)

    static member public GeneratePermutations(size: int, alphabet: 'TItem list) =
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if (size <= 0) || (size > alphabet.Length) then
            raise (ArgumentOutOfRangeException("size"))
        let lexNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, size)
        seq {0I .. lexNumberSup - 1I} |> Seq.map (fun lexNumber -> Permutations.GetPermutation(lexNumber, lexNumberSup, size, alphabet, []))

    static member public GeneratePermutationsRev(alphabet: 'TItem list) =
        Permutations.GeneratePermutationsRev(alphabet.Length, alphabet)

    static member public GeneratePermutationsRev(size: int, alphabet: 'TItem list) =
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if (size <= 0) || (size > alphabet.Length) then
            raise (ArgumentOutOfRangeException("size"))
        let lexNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, size)
        seq {lexNumberSup - 1I .. -1I .. 0I} |> Seq.map (fun lexNumber -> Permutations.GetPermutation(lexNumber, lexNumberSup, size, alphabet, []))

    static member public GetLexicographicalNumber(items: 'TItem list, alphabet: 'TItem list) =
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if items.IsEmpty || (items.Length > alphabet.Length) then
            raise (ArgumentException("items"))
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, items.Length)
        Permutations.GetLexicographicalNumber(items, alphabet, 0I, lexicographicalNumberSup)

    static member public GetNextPermutation(currentPermutation: 'TItem list, alphabet: 'TItem list) =
        let currentLexNumber = Permutations.GetLexicographicalNumber(currentPermutation, alphabet)
        Permutations.GetPermutation(currentLexNumber + 1I, alphabet)

    static member public GetPrevPermutation(currentPermutation: 'TItem list, alphabet: 'TItem list) =
        let currentLexNumber = Permutations.GetLexicographicalNumber(currentPermutation, alphabet)
        Permutations.GetPermutation(currentLexNumber - 1I, alphabet)

    static member internal GetLexicographicalNumberSup(alphabet: 'TItem list) =
        Permutations.GetLexicographicalNumberSup(alphabet, alphabet.Length)

    static member internal GetLexicographicalNumberSup(alphabet: 'TItem list, size: int) =
        if alphabet.IsEmpty then
            raise (ArgumentException("alphabet"))
        if (size <= 0) || (size > alphabet.Length) then
            raise (ArgumentOutOfRangeException("size"))
        seq{0 .. size - 1} |> Seq.fold (fun result number -> (alphabet.Length - number) |> bigint |> (*) result) 1I

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
