namespace CommonLib

open System

// Eratos == Eratosthene

module EratosSieveImpl =

    let calcNumber (index: int) = 2 * index + 3

    let calcIndex (number: int) = (number - 3) / 2

    let calcSieveSize (maxNumber: int) =
        match maxNumber % 2 with
        | 0 -> (maxNumber / 2) - 1
        | _ -> maxNumber / 2

open EratosSieveImpl

type EratosSieve private (maxNumber: int, sieve: bool[]) =

    let rec getNextPrime (index: int) =
        match index with
        | _ when index >= sieve.Length -> None
        | _ when sieve.[index] -> Some (calcNumber index)
        | _ -> getNextPrime (index + 1)

    member public this.MaxNumber = maxNumber

    member public this.IsPrime(number: int) =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | 2 -> true
        | _ when (number % 2) = 0 -> false
        | _ -> sieve.[number |> calcIndex]

    member public this.IsPrime(number: int64) = number |> int |> this.IsPrime

    member public this.IsPrime(number: bigint) = number |> int |> this.IsPrime

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

    static member public Create(maxNumber: int) =
        let erasePrimeFactors (prime: int64) (maxNumber: int64) (sieve: bool[]) =
            let mutable number = prime * prime
            let delta =  2L * prime
            while number <= maxNumber do
                sieve.[number |> int |> calcIndex] <- false
                number <- number + delta

        let generate (maxNumber: int64) (sieve: bool[]) =
            for index = 0 to sieve.Length - 1 do
                if sieve.[index] then
                    sieve |> erasePrimeFactors (index |> calcNumber |> int64) maxNumber

        let supportedMaxNumber = 1000000000

        match maxNumber with
        | _ when (maxNumber < 2) || (maxNumber > supportedMaxNumber) -> raise (ArgumentOutOfRangeException("maxNumber"))
        | 2 -> EratosSieve(maxNumber, Array.create 0 true)
        | _ ->
            let sieve = Array.create (calcSieveSize maxNumber) true
            sieve |> generate (maxNumber |> int64)
            EratosSieve(maxNumber, sieve)

type EratosSieveWithSmallestPrimeFactors private (maxNumber: int, data: int[]) =

    let rec getNextPrime (index: int) =
        let number = index |> calcNumber
        match index with
        | _ when index >= data.Length -> None
        | _ when data.[index] = number -> Some number
        | _ -> getNextPrime (index + 1)

    let eraseFactor (factor: int) (number: int) =
        let mutable rest = number
        let mutable count = 0
        while rest % factor = 0 do
            rest <- rest / factor
            count <- count + 1
        rest, count

    member public this.MaxNumber = maxNumber

    member public this.IsPrime(number: int) =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | 2 -> true
        | _ when (number % 2) = 0 -> false
        | _ -> data.[number |> calcIndex] = number

    member public this.IsPrime(number: int64) = number |> int |> this.IsPrime

    member public this.IsPrime(number: bigint) = number |> int |> this.IsPrime

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

    member public this.Item
        with get (number: int) =
            match number with
            | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
            | _ when number % 2 = 0 -> 2
            | _ -> data.[number |> calcIndex]

    member public this.Item with get (number: int64) = this.[number |> int] |> int64

    member public this.Item with get (number: bigint) = this.[number |> int] |> bigint

    // from https://en.wikipedia.org/wiki/Divisor_function
    member public this.CalcSigma0(number: int): int =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | _ -> number |> this.CalcSigma0Impl

    // from https://en.wikipedia.org/wiki/Divisor_function
    member public this.CalcSigma0(number: int64) = number |> int |> this.CalcSigma0 |> int64

    // from https://en.wikipedia.org/wiki/Divisor_function
    member public this.CalcSigma0(number: bigint) = number |> int |> this.CalcSigma0 |> bigint

    // from https://en.wikipedia.org/wiki/Divisor_function
    member public this.CalcSigma1(number: int): int =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | _ -> number |> this.CalcSigma1Impl

    // from https://en.wikipedia.org/wiki/Divisor_function
    member public this.CalcSigma1(number: int64) = number |> int |> this.CalcSigma1 |> int64

    // from https://en.wikipedia.org/wiki/Divisor_function
    member public this.CalcSigma1(number: bigint) = number |> int |> this.CalcSigma1 |> bigint

    // from https://en.wikipedia.org/wiki/Euler%27s_totient_function
    member public this.CalcEulerTotientFunction(number: int): int =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | _ -> number |> this.CalcEulerTotientFunctionImpl

    // from https://en.wikipedia.org/wiki/Euler%27s_totient_function
    member public this.CalcEulerTotientFunction(number: int64) = number |> int |> this.CalcEulerTotientFunction |> int64

    // from https://en.wikipedia.org/wiki/Euler%27s_totient_function
    member public this.CalcEulerTotientFunction(number: bigint) = number |> int |> this.CalcEulerTotientFunction |> bigint

    // from https://en.wikipedia.org/wiki/Divisor_function
    member private this.CalcSigma0Impl(number: int) =
        let mutable rest = number
        let mutable result = 1
        if number % 2 = 0 then
            let numberRest, count = number |> eraseFactor 2
            result <- result * (1 + count)
            rest <- numberRest
        while (rest > 1) do
            let index = rest |> calcIndex
            let factor = data.[index]
            let numberRest, count = rest |> eraseFactor factor
            result <- result * (1 + count)
            rest <- numberRest
        result

    // from https://en.wikipedia.org/wiki/Divisor_function
    member private this.CalcSigma1Impl(number: int) =
        let calcSigma1Factor (factor: int) (count: int) =
            let mutable result = 1
            let mutable memberValue = 1
            for _ in {1 .. count} do
                memberValue <- memberValue * factor
                result <- result + memberValue
            result

        let mutable rest = number
        let mutable result = 1
        if number % 2 = 0 then
            let numberRest, count = number |> eraseFactor 2
            result <- result * (count |> calcSigma1Factor 2)
            rest <- numberRest
        while (rest > 1) do
            let index = rest |> calcIndex
            let factor = data.[index]
            let numberRest, count = rest |> eraseFactor factor
            result <- result * (count |> calcSigma1Factor factor)
            rest <- numberRest
        result

    member private this.CalcEulerTotientFunctionImpl(number: int) =
        let mutable rest = number
        let mutable result = number
        if number % 2 = 0 then
            let numberRest, _ = number |> eraseFactor 2
            result <- result / 2
            rest <- numberRest
        while (rest > 1) do
            let index = rest |> calcIndex
            let factor = data.[index]
            let numberRest, _ = rest |> eraseFactor factor
            result <- (result / factor) * (factor - 1)
            rest <- numberRest
        result

    static member public Create(maxNumber: int) =

        let erasePrimeFactors (prime: int64) (maxNumber: int64) (data: int[]) =
            let mutable number = prime * prime
            let delta =  2L * prime
            while number <= maxNumber do
                let index = number |> int |> calcIndex
                data.[index] <- prime |> int
                number <- number + delta

        let generate (maxNumber: int64) (data: int[]) =
            for index = 0 to data.Length - 1 do
            if data.[index] = 0 then
                let prime = index |> calcNumber
                data.[index] <- prime
                data |> erasePrimeFactors (prime |> int64) maxNumber

        let supportedMaxNumber = 500000000

        match maxNumber with
        | _ when (maxNumber < 2) || (maxNumber > supportedMaxNumber) -> raise (ArgumentOutOfRangeException("maxNumber"))
        | 2 -> EratosSieveWithSmallestPrimeFactors(maxNumber, Array.create 0 0)
        | _ ->
            let data = Array.create (calcSieveSize maxNumber) 0
            data |> generate (maxNumber |> int64)
            EratosSieveWithSmallestPrimeFactors(maxNumber, data)
