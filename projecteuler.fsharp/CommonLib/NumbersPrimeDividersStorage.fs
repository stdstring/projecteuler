namespace CommonLib

open System

module NumbersPrimeDividersStorageImpl =

    [<Literal>]
    let MinNumber = 2

open NumbersPrimeDividersStorageImpl

type NumbersPrimeDividersStorage private (maxNumber: int, storage: Set<int> []) =

    member public this.GetPrimeDividersSet(number: int) =
        if (number < MinNumber || number > maxNumber) then
            raise (new ArgumentOutOfRangeException("number"))
        storage.[number - MinNumber]

    member public this.CalcPhiFunction(number: int) =
        if (number < MinNumber || number > maxNumber) then
            raise (new ArgumentOutOfRangeException("number"))
        let mutable numerator, denominator = 1, 1
        for prime in storage.[number - MinNumber] do
            numerator<-numerator * (prime - 1)
            denominator<-denominator * prime
        (number |> int64) * (numerator |> int64) / (denominator |> int64) |> int

    static member public Create(maxNumber: int) =
        if (maxNumber < MinNumber) then
            raise (new ArgumentOutOfRangeException("maxNumber"))
        let sieve = EratosSieve.Create(maxNumber)
        let storage = Array.create (maxNumber - MinNumber + 1) Set.empty
        let rec findOddDivider (number: int) (divider: int) =
            match number with
            | _ when number % divider = 0 -> divider
            | _ -> (sieve.GetNextPrime(divider)).Value |> findOddDivider number
        let processNumber = fun (number: int) ->
            match number with
            | _ when number |> sieve.IsPrime -> storage.[number - MinNumber]<-([number] |> Set.ofSeq)
            | _ when number % 2 = 0 -> storage.[number - MinNumber]<-(storage.[number / 2 - MinNumber] |> Set.add 2)
            | _ -> let divider = 3 |> findOddDivider number in storage.[number - MinNumber]<-(storage.[number / divider - MinNumber] |> Set.add divider)
        for number in seq {MinNumber .. maxNumber} do
            number |> processNumber
        new NumbersPrimeDividersStorage(maxNumber, storage)
