namespace AdventOfCode.FSharp.Y2024

// Day 3: Mull It Over
module Day03 =
    open AdventOfCode.FSharp.Util
    open System.Text
    open System.Text.RegularExpressions

    let MulInstructioRegex = new Regex(@"mul\((\d+),(\d+)\)", RegexOptions.Compiled)

    let DoMulInstructioRegex =
        new Regex(@"(?:mul\((\d+),(\d+)\))|do\(\)|don't\(\)", RegexOptions.Compiled)

    let run (input: byte[]) (output: int -> string -> unit) =
        let inputText = input |> text

        let executeMul (found: Match) =
            let a = found.Groups[1].Value |> int
            let b = found.Groups[2].Value |> int
            a * b

        inputText
        |> MulInstructioRegex.Matches
        |> Seq.map executeMul
        |> Seq.sum
        |> string
        |> output 1

        inputText
        |> DoMulInstructioRegex.Matches
        |> Seq.fold
            (fun (total, doState) found ->
                match found.Value[0], doState with
                | 'd', _ when found.Length = 4 -> total, true
                | 'd', _ -> total, false
                | _, false -> total, doState
                | _ -> total + (executeMul found), doState)
            (0, true)
        |> fst
        |> string
        |> output 2

    open System
    open System.Buffers

    let SimpleMulInstructioRegex =
        new Regex(@"mul\([1-9][0-9]{0,2},[1-9][0-9]{0,2}\)|do\(\)|don't\(\)", RegexOptions.Compiled)

    let runBuffers (input: byte[]) (output: int -> string -> unit) =
        let byteBuf = ReadOnlySequence<byte> input
        let charBufSource = input.Length |> Array.zeroCreate
        let charBuf = Span<char> charBufSource
        EncodingExtensions.GetChars(Encoding.UTF8, &byteBuf, charBuf) |> ignore

        let mutable e = SimpleMulInstructioRegex.EnumerateMatches charBuf

        let mutable total1 = 0

        let mutable doState = true
        let mutable total2 = 0

        while e.MoveNext() do
            let m = e.Current

            match charBuf[m.Index], m.Length with
            | 'd', 4 -> doState <- true
            | 'd', _ -> doState <- false
            | _ ->
                let ms = charBuf.Slice(m.Index + 4, m.Length - 5)
                let mutable res = 0
                let mutable arg = 0

                for i = 0 to ms.Length - 1 do
                    let c = ms[i]

                    if c = ',' then
                        res <- res + arg
                        arg <- 0
                    else
                        arg <- arg * 10 + int (c - '0')

                total1 <- total1 + (arg * res)

                if doState then
                    total2 <- total2 + (arg * res)

        total1 |> string |> output 1
        total2 |> string |> output 2
