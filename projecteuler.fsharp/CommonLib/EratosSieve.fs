namespace CommonLib

open System

// Eratos == Eratosthene

module EratosSieveImpl =

    [<Literal>]
    let SupportedMaxNumber = 1000000000

    let calcNumber (index: int) = 2 * index + 3

    let calcIndex (number: int) = (number - 3) / 2

    let calcSieveSize (maxNumber: int) =
        match maxNumber % 2 with
        | 0 -> (maxNumber / 2) - 1
        | _ -> maxNumber / 2

    type EratosSieveSeqData = {mutable CurrentPrime: int option}

open EratosSieveImpl

type EratosSieve private (maxNumber: int, sieve: bool[]) =

    let rec getNextPrime (index: int) =
        match index with
        | _ when index >= sieve.Length -> None
        | _ when sieve.[index] -> Some (calcNumber index)
        | _ -> getNextPrime (index + 1)

    member public this.IsPrime(number: int) =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | 2 -> true
        | _ when (number % 2) = 0 -> false
        | _ -> sieve.[number |> calcIndex]

    member public this.IsPrime(number: int64) =
        this.IsPrime(number |> int)

    member public this.IsPrime(number: bigint) =
        this.IsPrime(number |> int)

    member public this.GetNextPrime(number: int) =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | 2 when maxNumber = 2 -> None
        | 2 -> Some 3
        | _ -> number |> calcIndex |> (+) 1 |> getNextPrime

    member public this.GetNextPrime(number: int64) =
        match this.GetNextPrime(number |> int) with
        | Some value -> value |> int64 |> Some
        | None -> None

    member public this.GetNextPrime(number: bigint) =
        match this.GetNextPrime(number |> int) with
        | Some value -> value |> bigint |> Some
        | None -> None

    member public this.ToSeq () =
        let generator = function
            | 0 -> (2, 2) |> Some
            | currentPrime ->
                match currentPrime |> this.GetNextPrime with
                | Some nextPrime -> (nextPrime, nextPrime) |> Some
                | None -> None
        Seq.unfold generator 0

    member public this.MaxNumber = maxNumber

    static member public Create(maxNumber: int) =
        let erasePrimeFactors (prime: int64) (maxNumber: int64) (sieve: bool[]) =
            let mutable number = prime * prime
            let delta =  2L * prime
            while number <= maxNumber do
                sieve.[number |> int |> calcIndex] <- false
                number <- number + delta

        let processIndex (maxNumber: int64) (sieve: bool[]) =
            for index = 0 to sieve.Length - 1 do
                if sieve.[index] then
                    sieve |> erasePrimeFactors (index |> calcNumber |> int64) maxNumber

        match maxNumber with
        | _ when (maxNumber < 2) || (maxNumber > SupportedMaxNumber) -> raise (ArgumentOutOfRangeException("maxNumber"))
        | 2 -> EratosSieve(maxNumber, Array.create 0 true)
        | _ ->
            let sieve = Array.create (calcSieveSize maxNumber) true
            sieve |> processIndex (maxNumber |> int64)
            EratosSieve(maxNumber, sieve)
