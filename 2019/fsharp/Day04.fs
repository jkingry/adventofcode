namespace AdventOfCode.FSharp.Y2019

// Day 4: Secure Container
module Day04 =
    open AdventOfCode.FSharp.Util

    let rec generate resMin resMax safeMin safeMax hasDupe prefix =
        seq {
            let digitMin, resMin' =
                match resMin with
                | x :: y -> x, y
                | _ -> raise Unreachable

            let digitMax, resMax' =
                match resMax with
                | x :: y -> x, y
                | _ -> raise Unreachable

            let startDigit =
                if not safeMin then
                    max digitMin (List.tryHead prefix |> Option.defaultValue 0)
                else
                    List.head prefix

            let endDigit = if not safeMax then digitMax else 9

            for d = startDigit to endDigit do
                let safeMin' = safeMin || d > digitMin
                let safeMax' = safeMax || d < digitMax
                let n = d :: prefix

                let hasDupe' =
                    hasDupe
                    || match prefix with
                       | [] -> false
                       | x :: _ -> x = d

                if resMin' |> List.length > 0 then
                    yield! generate resMin' resMax' safeMin' safeMax' hasDupe' n
                elif hasDupe' then
                    yield n
        }

    let hasOnlyOneDouble (n: int list) : bool =
        let mutable p = -1
        let mutable count = 1
        let mutable found = false

        for d in n do
            count <-
                if d = p then
                    count + 1
                else
                    if count = 2 then
                        found <- true

                    1

            p <- d

        found || count = 2

    let numberToDigits (n: int) : int list =
        let digits = n |> float |> log10 |> int
        let mutable result = []
        let mutable remainder = n

        for i = 0 to digits do
            let magnitude = 10.0 ** float (digits - i) |> int
            let d = remainder / magnitude
            result <- d :: result
            remainder <- remainder - d * magnitude

        result |> List.rev

    let digitsToNumber (n: int list) : int =
        n |> List.rev |> List.fold (fun a d -> a * 10 + d) 0

    let run (input: byte array) output =
        let rangeStart, rangeEnd =
            match input |> text with
            | Regex @"(\d+)-(\d+)" [ rangeStart; rangeEnd ] -> int rangeStart, int rangeEnd
            | s -> failwithf "Invalid input: %s" s

        let resMin = rangeStart |> numberToDigits
        let resMax = rangeEnd |> numberToDigits

        let numbers = generate resMin resMax false false false []

        let doubleRule, onlyOneDoubleRule =
            numbers
            |> Seq.fold
                (fun (dd, dd1) n ->
                    if n |> hasOnlyOneDouble then
                        dd + 1, dd1 + 1
                    else
                        dd + 1, dd1)
                (0, 0)

        doubleRule |> string |> output 1
        onlyOneDoubleRule |> string |> output 2
