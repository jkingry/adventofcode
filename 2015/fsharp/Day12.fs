namespace AdventOfCode.FSharp.Y2015

// Day 12
module Day12 =
    open AdventOfCode.FSharp.Util
    open System
    open System.Text.Json

    let runJson (input: byte array) (output: int -> string -> unit) =
        let inputSpan = ReadOnlySpan<byte> input

        let reader = new Utf8JsonReader(inputSpan)

        let mutable part1 = 0

        let mutable part2 = 0

        let mutable objTotals = []
        let mutable objValid = []
        let mutable readSkip = false

        while readSkip || reader.Read() do
            readSkip <- false

            match reader.TokenType with
            | JsonTokenType.Number ->
                let v = reader.GetInt32()
                part1 <- part1 + v

                match objTotals with
                | [] -> part2 <- part2 + v
                | x :: xs -> objTotals <- x + v :: xs
            | JsonTokenType.StartObject ->
                objTotals <- 0 :: objTotals
                objValid <- true :: objValid
            | JsonTokenType.EndObject ->
                match objTotals, objValid with
                | x :: y :: xs, true :: vs ->
                    objTotals <- x + y :: xs
                    objValid <- vs
                | [ x ], [ true ] ->
                    objTotals <- []
                    objValid <- []
                    part2 <- part2 + x
                | _ :: xs, false :: vs ->
                    objTotals <- xs
                    objValid <- vs
                | _, _ -> failwith "invalid json"
            | JsonTokenType.PropertyName ->
                if reader.Read() then
                    match reader.TokenType with
                    | JsonTokenType.String when reader.ValueTextEquals "red"B ->
                        match objValid with
                        | _ :: vs -> objValid <- false :: vs
                        | [] -> failwith "invalid"
                    | _ -> readSkip <- true
            | _ -> ()

        part1 |> string |> output 1
        part2 |> string |> output 2

(*
    let runSimple (input: byte array) (output: int -> string -> unit) =
        let mutable p = 0

        let mutable part1 = 0

        while p < input.Length do
            let np, v = parseIntToAny input p

            part1 <- part1 + v

            p <- np + 1

        part1 |> string |> output 1
*)
