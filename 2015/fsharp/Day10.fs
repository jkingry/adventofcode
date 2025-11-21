namespace AdventOfCode.FSharp.Y2015

// Day 10: Elves Look, Elves Say
module Day10 =
    open AdventOfCode.FSharp.Util
    open Checked

    let digits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]

    let lookandsay input =
        // input |> List.toArray |> System.String |> printfn "%s"
        let mutable result = []
        let mutable previousDigit = None
        let mutable digitCount = 0

        let expand pv =
            result <- pv :: digits[digitCount] :: result

        for digit in input do
            match previousDigit with
            | None ->
                previousDigit <- Some digit
                digitCount <- 1
            | Some previousDigitValue when previousDigitValue = digit -> digitCount <- digitCount + 1
            | Some previousDigitValue ->
                expand previousDigitValue
                previousDigit <- Some digit
                digitCount <- 1

        match previousDigit with
        | Some previousDigitValue -> expand previousDigitValue
        | _ -> ()

        result |> List.rev

    let rec repeat f n i =
        if n > 0 then repeat f (n - 1) (f i) else i

    let trim (s: string) = s.Trim()

    let run (input: byte array) (output: int -> string -> unit) =
        let part1 = input |> text |> trim |> Seq.toList |> repeat lookandsay 40
        let part2 = part1 |> repeat lookandsay 10

        part1 |> List.length |> string |> output 1
        part2 |> List.length |> string |> output 2
