namespace AdventOfCode.FSharp.Y2020

// Day 4: Passport Processing
module Day04 =
    open AdventOfCode.FSharp.Util
    open System

    let parsePassportTuple input =
        match input |> split ":" with
        | [| field; value |] -> field, value
        | _ -> failwithf "Invalid passport tuple: %s" input

    let parsePassportBlock (input: string) =
        input.Split([| "\n"; " " |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parsePassportTuple
        |> Map.ofArray

    let reqField field v =
        match v |> Map.tryFind field with
        | Some s -> Ok s
        | _ -> sprintf "Missing field: %s" field |> Error

    let validNumber (v: string) =
        let mutable n = 0

        if Int32.TryParse(v, &n) then
            Ok n
        else
            sprintf "Invalid number: %s" v |> Error

    let validRange (minRange: int) (maxRange: int) v =
        if minRange <= v && v <= maxRange then
            Ok()
        else
            sprintf "Outside of range %i to %i: %i" minRange maxRange v |> Error

    let validRegex1 re v =
        match v with
        | Regex re [ p ] -> Ok p
        | _ -> sprintf "Match of '%s' failed: %s" re v |> Error

    let validAll validators v =
        let results = validators |> Seq.map (fun validator -> validator v)

        match results |> Seq.tryFind Result.isError with
        | Some(Error e) -> Error e
        | _ -> Ok()

    let validAny validators v =
        let results = validators |> Seq.map (fun validator -> validator v)

        match results |> Seq.tryFind Result.isOk with
        | Some(Ok s) -> Ok s
        | _ ->
            let errors =
                results
                |> Seq.map (function
                    | Error s -> s.ToString()
                    | _ -> raise Unreachable)
                |> Seq.toArray

            String.Join(',', errors) |> Error


    let validOption options v =
        if options |> List.contains v then
            Ok()
        else
            sprintf "Option not found: %A" v |> Error

    let ignoreResult v = Ok()

    let inline (@>>) x y = x >> Result.bind y

    let requiredFields: (Map<string, string> -> Result<unit, string>) list =
        [ "byr" // (Birth Year)
          "iyr" // (Issue Year)
          "eyr" // (Expiration Year)
          "hgt" // (Height)
          "hcl" // (Hair Color)
          "ecl" // (Eye Color)
          "pid" ] // (Passport ID)
        |> List.map (fun f -> reqField f @>> ignoreResult)

    let fieldValidPassport = validAll requiredFields

    let eyeColors = [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

    let valueValidators =
        [ reqField "byr" @>> validNumber @>> validRange 1920 2002
          reqField "iyr" @>> validNumber @>> validRange 2010 2020
          reqField "eyr" @>> validNumber @>> validRange 2020 2030
          reqField "hgt"
          @>> validAny
                  [ validRegex1 @"^(\d+)cm$" @>> validNumber @>> validRange 150 193
                    validRegex1 @"^(\d+)in$" @>> validNumber @>> validRange 59 76 ]
          reqField "hcl" @>> validRegex1 @"^#([0-9a-f]{6})$" @>> ignoreResult
          reqField "ecl" @>> validOption eyeColors
          reqField "pid" @>> validRegex1 @"^([0-9]{9})$" @>> ignoreResult ]

    let valueValidPassport = validAll valueValidators

    let run (input: byte array) output =
        let passports = input |> text |> splitDoubleLine |> Seq.map parsePassportBlock

        let hasFields, hasValues =
            passports
            |> Seq.fold
                (fun (f, v) p ->
                    let f' = f + if fieldValidPassport p |> Result.isOk then 1 else 0
                    let v' = v + if valueValidPassport p |> Result.isOk then 1 else 0
                    f', v')
                (0, 0)

        hasFields |> string |> output 1
        hasValues |> string |> output 2
