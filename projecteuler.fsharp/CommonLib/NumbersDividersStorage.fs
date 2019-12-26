namespace CommonLib

open System
open System.Collections.Generic

module NumbersPrimeDividersStorageImpl =

    [<Literal>]
    let MinNumber = 2

open NumbersPrimeDividersStorageImpl

type NumbersPrimeDividersStorage internal (maxNumber: int, storage: ISet<int> []) =

    member public this.GetDividersSet(number: int) =
        if (number <= 0 || number > maxNumber) then
            raise (new ArgumentOutOfRangeException("number"))
        match number with
        | 1 -> new HashSet<int>() :> ISet<int>
        | _ -> storage.[number - MinNumber]

    member public this.CalcPhiFunction(number: int) =
        if (number < MinNumber || number > maxNumber) then
            raise (new ArgumentOutOfRangeException("number"))
        let mutable numerator, denominator = 1, 1
        for prime in storage.[number - MinNumber] do
            numerator<-numerator * (prime - 1)
            denominator<-denominator * prime
        (number |> int64) * (numerator |> int64) / (denominator |> int64) |> int

type NumbersDividersStorage internal (maxNumber: int, storage: ISet<int> []) =

    member public this.GetDividersSet(number: int) =
        if (number <= 0 || number > maxNumber) then
            raise (new ArgumentOutOfRangeException("number"))
        match number with
        | 1 -> new HashSet<int>([1]) :> ISet<int>
        | _ -> storage.[number - MinNumber]

[<AbstractClass; Sealed>]
type NumbersDividersStorageFactory =

    static member public CreatePrimeDividersStorage(maxNumber: int) =
        if (maxNumber < MinNumber) then
            raise (new ArgumentOutOfRangeException("maxNumber"))
        let primeDividersGenerator (prime: int) = let dividers = new HashSet<int>([prime]) in dividers :> ISet<int>
        let dividerAppender (divider: int) (dividers: ISet<int>) =
            let newDividers = new HashSet<int>(dividers)
            divider |> newDividers.Add |> ignore
            newDividers :> ISet<int>
        let storage = NumbersDividersStorageFactory.CreateDividerStorageImpl(maxNumber, primeDividersGenerator, dividerAppender)
        new NumbersPrimeDividersStorage(maxNumber, storage)

    static member public CreateDividersStorage(maxNumber: int) =
        if (maxNumber < MinNumber) then
            raise (new ArgumentOutOfRangeException("maxNumber"))
        let primeDividersGenerator (prime: int) = let dividers = new HashSet<int>([1; prime]) in dividers :> ISet<int>
        let dividerAppender (divider: int) (dividers: ISet<int>) =
            let newDividers = new HashSet<int>(dividers)
            dividers |> Seq.iter (fun number -> newDividers.Add(number * divider) |> ignore)
            newDividers :> ISet<int>
        let storage = NumbersDividersStorageFactory.CreateDividerStorageImpl(maxNumber, primeDividersGenerator, dividerAppender)
        new NumbersDividersStorage(maxNumber, storage)

    static member private CreateDividerStorageImpl(maxNumber: int, primeDividersGenerator: int->ISet<int>, dividerAppender: int->ISet<int>->ISet<int>) =
        let sieve = maxNumber |> EratosSieve.Create
        let rec findOddDivider (number: int) (divider: int) =
            match divider with
            | _ when number % divider = 0 -> divider
            | _ -> divider + 2 |> findOddDivider number
        let storage = Array.create (maxNumber - MinNumber + 1) null
        for number in seq {MinNumber .. maxNumber} do
            match number with
            | _ when number |> sieve.IsPrime ->
                storage.[number - MinNumber]<-number |> primeDividersGenerator
            | _ when number % 2 = 0 ->
                storage.[number - MinNumber]<-(storage.[number / 2 - MinNumber] |> dividerAppender 2)
            | _ ->
                let divider = 3 |> findOddDivider number
                storage.[number - MinNumber]<-(storage.[number / divider - MinNumber] |> dividerAppender divider)
        storage