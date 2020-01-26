namespace CommonLib

open System

type EulerTotientFunction private (maxNumber: int, data: int[]) =

    member public this.MaxNumber = maxNumber

    member public this.GetValue(number: int) =
        match number with
        | _ when (number < 1) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | _ -> data.[number]

    member public this.GetValue(number: int64) = number |> int |> this.GetValue |> int64

    member public this.GetValue(number: bigint) = number |> int |> this.GetValue |> bigint

    member public this.IsPrime(number: int) =
        match number with
        | _ when (number < 2) || (number > maxNumber) -> raise (ArgumentOutOfRangeException("number"))
        | _ -> data.[number] = (number - 1)

    member public this.IsPrime(number: int64) = number |> int |> this.IsPrime

    member public this.IsPrime(number: bigint) = number |> int |> this.IsPrime

    static member public Create(maxNumber: int) =

        let supportedMaxNumber = 250000000

        let generate (data: int[]) =
            data.[1] <- 1
            for number in {2 .. maxNumber} do
                let isPrime = (data.[number] = 0)
                // phi(p) = p - 1
                if data.[number] = 0 then
                    data.[number] <- (number - 1)
                let mutable factor = 2
                let mutable product = factor * number
                while (factor <= number) && (product <= maxNumber) do
                    if data.[product] = 0 then
                        // phi(m * n) = phi(m) * phi(n), if gcd(m, n) = 1
                        if (isPrime) && (factor < number) then
                            data.[product] <- data.[number] * data.[factor]
                        // phi(n^m) = (n^(m - 1)) * phi(n)
                        else if (factor = number) then
                            data.[product] <- data.[number] * number
                        // phi(2 * m) = 2 * phi(m), if m = 2 * k; phi(2 * m) = phi(m), if m = 2 * k - 1
                        else if product % 2 = 0 then
                            let m = product / 2
                            data.[product] <- (if m % 2 = 0 then 2 else 1) * data.[m]
                        // phi(m * n) = phi(m) * phi(n), if gcd(m, n) = 1, phi(m * n) = phi(m) * phi(n) * d / phi(d), where d = gcd(m, n), if gcd(m, n) != 1
                        else
                            let gcd = CommonLib.NumbersRelation.CalcGCD(number, factor)
                            if (gcd = 1) then
                                data.[product] <- data.[number] * data.[factor]
                            else
                                data.[product] <- gcd * (data.[number] * data.[factor] / data.[gcd])
                    factor <- factor + 1
                    product <- factor * number

        match maxNumber with
        | _ when (maxNumber < 2) || (maxNumber > supportedMaxNumber) -> raise (ArgumentOutOfRangeException("maxNumber"))
        | _ ->
            let data = Array.create (maxNumber + 1) 0
            data.[1] <- 1
            data |> generate
            EulerTotientFunction(maxNumber, data)