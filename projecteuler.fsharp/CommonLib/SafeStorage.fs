namespace CommonLib

open System

type SafeStorage<'TValue>(minNumber: int, maxNumber: int, defaultValue: 'TValue) =

    do if (minNumber > maxNumber) then
        raise (ArgumentOutOfRangeException("minNumber"))

    let minNumber = minNumber
    let maxNumber = maxNumber
    let defaultValue = defaultValue
    let storage = Array.create (maxNumber - minNumber + 1) defaultValue

    member this.MinNumber = minNumber

    member this.MaxNumber = maxNumber

    member this.DefaultValue = defaultValue

    member this.Storage = storage

    member public this.SetValue(number: int, value: 'TValue) =
        match number with
        | _ when (number < minNumber) || (number > maxNumber) -> ()
        | _ -> storage.[number - minNumber]<-value

    member public this.SetValue(number: int64, value: 'TValue) =
        let minNumber = minNumber |> int64
        let maxNumber = maxNumber |> int64
        match number with
        | _ when (number < minNumber) || (number > maxNumber) -> ()
        | _ -> storage.[number - minNumber |> int]<-value

    member public this.SetValue(number: bigint, value: 'TValue) =
        let minNumber = minNumber |> bigint
        let maxNumber = maxNumber |> bigint
        match number with
        | _ when (number < minNumber) || (number > maxNumber) -> ()
        | _ -> storage.[number - minNumber |> int]<-value

    member public this.GetValue(number: int) =
        match number with
        | _ when (number < minNumber) || (number > maxNumber) -> defaultValue
        | _ -> storage.[number - minNumber]

    member public this.GetValue(number: int64) =
        let minNumber = minNumber |> int64
        let maxNumber = maxNumber |> int64
        match number with
        | _ when (number < minNumber) || (number > maxNumber) -> defaultValue
        | _ -> storage.[number - minNumber |> int]

    member public this.GetValue(number: bigint) =
        let minNumber = minNumber |> bigint
        let maxNumber = maxNumber |> bigint
        match number with
        | _ when (number < minNumber) || (number > maxNumber) -> defaultValue
        | _ -> storage.[number - minNumber |> int]
