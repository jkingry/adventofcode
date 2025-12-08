namespace AdventOfCode.FSharp.Y2025

// Day 2: Gift Shop
module Day02 =
    open AdventOfCode.FSharp.Util
    open System

    let inline countDigits (value: int64) = (value |> double |> log10 |> int) + 1

    let inline isRepeat patternLength (value: int64) : bool =
        let digits = countDigits value

        if digits % patternLength <> 0 then
            false
        else
            let scale = 10.0 ** (patternLength |> float) |> int64
            let kernel = value % scale

            if kernel = 0 then
                false
            else
                let mutable repeatValue = kernel
                let mutable repeatScale = scale

                let maxRepeats = digits / patternLength - 1
                let mutable repeat = 1

                while repeat <= maxRepeats do
                    repeatValue <- repeatValue + kernel * repeatScale
                    repeatScale <- repeatScale * scale
                    repeat <- repeat + 1

                // printfn "%d (%d) : k: %d r: %d = %b" value patternLength kernel repeatValue (value = repeatValue)
                value = repeatValue


    let inline findRepeats (value: int64) =
        let digits = countDigits value

        if digits = 1 then
            None
        else
            let maxPattern = digits / 2
            let mutable found = None
            let mutable patternLength = maxPattern

            while found.IsNone && patternLength >= 1 do
                if isRepeat patternLength value then
                    found <- digits / patternLength |> Some

                patternLength <- patternLength - 1

            found

    let runSpan (input: byte array) (output: int -> string -> unit) =
        let inputSpan = ReadOnlySpan<byte> input
        let mutable part1 = 0L
        let mutable part2 = 0L

        for r in inputSpan.Split ','B do
            let rangeText = trySlice r inputSpan

            if rangeText.Length > 0 then
                let trimIndex = rangeText.IndexOf '\n'B

                let rangeText =
                    if trimIndex > 0 then
                        rangeText.Slice(0, trimIndex)
                    else
                        rangeText

                let vi = rangeText.IndexOf '-'B
                let v1 = parseInt64Span (rangeText.Slice(0, vi))
                let v2 = parseInt64Span (rangeText.Slice(vi + 1))

                // printfn "%d-%d" v1 v2
                let mutable v = v1

                while v <= v2 do
                    let repeats = findRepeats v

                    if repeats.IsSome then
                        if repeats.Value > 0 then
                            part2 <- part2 + v

                        if repeats.Value = 2 then
                            part1 <- part1 + v

                    v <- v + 1L

        part1 |> string |> output 1
        part2 |> string |> output 2

    let parseRange (input: string) =
        let parts = input |> split "-"
        let v1 = parts[0] |> int64
        let v2 = parts[1] |> int64

        v1, v2

    let parseRanges (input: string) =
        input.Trim() |> split "," |> Seq.map parseRange

    let run (input: byte array) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> text
            |> parseRanges
            |> Seq.map (fun (v1, v2) -> seq { v1..v2 })
            |> Seq.concat
            |> Seq.choose (fun v ->
                v
                |> findRepeats
                |> Option.map (function
                    | 2 -> v, v
                    | _ -> 0L, v))
            |> Seq.fold (fun (part1, part2) (a, b) -> part1 + a, part2 + b) (0L, 0L)

        part1 |> string |> output 1
        part2 |> string |> output 2

    let checkRepeat value repeatCount =
        let digits = value |> double |> log10 |> floor
        let digits = digits + 1.0

        let repeatCount = repeatCount |> double

        if digits % repeatCount <> 0 then
            false
        else
            let scale = 10.0 ** (digits / repeatCount) |> int64
            let kernel = value % scale

            if kernel = 0 || value % kernel <> 0 then
                false
            else
                let dividend = value / kernel
                let actual = dividend * (scale - 1L) + 1L |> double |> log10
                actual = digits

    let findRepeat value =
        match countDigits value with
        | 1 -> None
        | n -> [ 2..n ] |> List.tryFind (checkRepeat value)

    let runNoLoop (input: byte array) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> text
            |> parseRanges
            |> Seq.map (fun (v1, v2) -> seq { v1..v2 })
            |> Seq.concat
            |> Seq.choose (fun v ->
                match findRepeat v with
                | Some 2 -> (v, v) |> Some
                | Some _ -> (0L, v) |> Some
                | _ -> None)
            |> Seq.fold (fun (part1, part2) (a, b) -> part1 + a, part2 + b) (0L, 0L)

        part1 |> string |> output 1
        part2 |> string |> output 2

    let findRepeatsOfSize digits a b patternSize =
        let patternSize = double patternSize

        let remainder = digits - patternSize
        let patternMagnitude = 10.0 ** patternSize
        let exponent = digits / patternSize

        let factor =
            (patternMagnitude ** exponent - 1.0) / (patternMagnitude - 1.0) |> int64

        let patternMagnitude = int64 patternMagnitude

        seq {
            let mutable kernel =
                a / (10.0 ** remainder |> int64) |> max (patternMagnitude / 10L)

            let mutable v = kernel * factor

            while v <= b && kernel < patternMagnitude do
                if v >= a then
                    yield v

                kernel <- kernel + 1L
                v <- kernel * factor
        }

    let findRepeatsFast a b =
        let adigits = countDigits a
        let bdigits = countDigits b

        [ adigits..bdigits ]
        |> Seq.map (fun digits ->
            [ digits / 2 .. -1 .. 1 ]
            |> Seq.filter (fun patternSize -> digits % patternSize = 0)
            |> Seq.map (fun patternSize ->
                let oneRepeat = digits = patternSize * 2
                findRepeatsOfSize digits a b patternSize |> Seq.map (fun v -> v, oneRepeat))
            |> Seq.concat)
        |> Seq.concat
        |> Seq.distinctBy fst

    let runPrefix (input: byte array) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> text
            |> parseRanges
            |> Seq.map (fun (v1, v2) -> findRepeatsFast v1 v2)
            |> Seq.concat
            |> Seq.fold
                (fun (part1, part2) (v, includePart1) ->
                    let part1 = if includePart1 then part1 + v else part1

                    let part2 = part2 + v

                    part1, part2)
                (0L, 0L)

        part1 |> string |> output 1

        part2 |> string |> output 2
