namespace CommonLib

open System

type LazyEulerTotientFunction private (maxNumber: int, sieve: EratosSieveWithSmallestPrimeFactors, data: int[]) =

    member public this.MaxNumber = maxNumber

    member public this.GetValue(number: int) =
        if (number < 1) || (number > maxNumber) then
            raise (ArgumentOutOfRangeException("number"))
        if data.[number] = 0 then
            data.[number] <- number |> sieve.CalcEulerTotientFunction
        data.[number]

    member public this.GetValue(number: int64) = number |> int |> this.GetValue |> int64

    member public this.GetValue(number: bigint) = number |> int |> this.GetValue |> bigint

    member public this.IsPrime(number: int) =
        number |> sieve.IsPrime

    member public this.IsPrime(number: int64) =
        number |> sieve.IsPrime

    member public this.IsPrime(number: bigint) =
        number |> sieve.IsPrime

    member public this.GetPrimes() =
        sieve.ToSeq()

    static member public Create(maxNumber: int) =
        let supportedMaxNumber = 125000000

        match maxNumber with
        | _ when (maxNumber < 2) || (maxNumber > supportedMaxNumber) -> raise (ArgumentOutOfRangeException("maxNumber"))
        | _ ->
            let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
            let data = Array.create (maxNumber + 1) 0
            data.[1] <- 1
            LazyEulerTotientFunction(maxNumber, sieve, data)