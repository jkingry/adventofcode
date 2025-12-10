namespace AdventOfCode.FSharp.Y2025

// Day 6: Trash Compactor
module Day06 =
    open AdventOfCode.FSharp.Util

    let parseOps (opLine: byte[]) =
        let mutable ops = []

        for i = 0 to opLine.Length - 1 do
            if opLine[i] <> ' 'B then
                ops <- opLine[i] :: ops

        ops |> List.rev |> Array.ofList

    let parseInputPart1 (lines: byte array array) =
        let operands =
            lines |> Array.take (lines.Length - 1) |> Array.map parseInts |> Array.transpose

        let ops = lines |> Array.last |> parseOps

        Array.zip operands ops

    let inline arraySplit<'T when 'T: equality> (splitValue: 'T) (a: 'T[]) =
        let mutable chunk = []
        let mutable result = []

        for x in a do
            if x = splitValue then
                if chunk <> [] then
                    result <- (chunk |> Array.ofList) :: result

                chunk <- []
            else
                chunk <- x :: chunk

        if chunk <> [] then
            result <- (chunk |> Array.ofList) :: result

        result |> Array.ofList


    let parseInputPart2 (lines: byte array array) =
        let operands =
            lines
            |> Array.take (lines.Length - 1)
            |> Array.transpose
            |> Array.map (fun input -> parseIntToAny input 0 |> snd)
            |> arraySplit 0

        let ops = lines |> Array.last |> parseOps |> Array.rev

        Array.zip operands ops

    let doMathHomework homework =
        homework
        |> Array.map (fun (values, op) ->
            match op with
            | '+'B -> values |> Array.sum |> int64
            | '*'B -> values |> Array.map int64 |> Array.reduce (fun a b -> a * b)
            | _ -> failwith "invalid operation")
        |> Array.sum

    let run (input: byte array) (output: int -> string -> unit) =
        let lines = input |> bsplit '\n'B

        lines |> parseInputPart1 |> doMathHomework |> string |> output 1

        lines |> parseInputPart2 |> doMathHomework |> string |> output 2
