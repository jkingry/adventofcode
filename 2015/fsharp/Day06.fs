namespace AdventOfCode.FSharp.Y2015

// Day 6: Probably a Fire Hazard
module Day06 =
    open AdventOfCode.FSharp.Util

    let mask = 1000 * 1000 |> bits

    let turnOff (lights: bits) (top, left: int) (bottom, right: int) =
        let row: bits = bits (1 + (right - left), true)
        row.Length <- 1000 * 1000
        left + (top * 1000) |> row.LeftShift |> ignore

        for _ = 0 to bottom - top do
            row.Not() |> ignore
            lights.And(row) |> ignore
            row.Not() |> ignore
            row.LeftShift(1000) |> ignore

    let turnOn (lights: bits) (top, left: int) (bottom, right: int) =
        let row: bits = bits (1 + (right - left), true)
        row.Length <- 1000 * 1000
        left + (top * 1000) |> row.LeftShift |> ignore

        for _ = 0 to bottom - top do
            lights.Or(row) |> ignore
            row.LeftShift(1000) |> ignore

    let toggle (lights: bits) (top, left: int) (bottom, right: int) =
        let row: bits = bits (1 + (right - left), true)
        row.Length <- 1000 * 1000
        left + (top * 1000) |> row.LeftShift |> ignore

        for _ = 0 to bottom - top do
            lights.Xor(row) |> ignore
            row.LeftShift(1000) |> ignore

    type Pos = int * int

    type Instr =
        | TurnOff of (Pos * Pos)
        | TurnOn of (Pos * Pos)
        | Toggle of (Pos * Pos)

    let parseLine (line: string) =
        match line with
        | Regex "(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)" [ txt; a; b; c; d ] ->
            let topLeft = (int a), (int b)
            let bottomRight = (int c), (int d)
            let coords = topLeft, bottomRight

            match txt with
            | "turn off" -> TurnOff coords
            | "turn on" -> TurnOn coords
            | "toggle" -> Toggle coords
            | s -> failwithf "Invalid instruction: %s" s
        | _ -> failwithf "Invalid line: %s" line

    let run (input: byte array) (output: int -> string -> unit) =
        let lights = 1000 * 1000 |> bits

        input
        |> text
        |> splitLine
        |> Array.map parseLine
        |> Array.iter (function
            | TurnOff(topLeft, bottomRight) -> turnOff lights topLeft bottomRight
            | TurnOn(topLeft, bottomRight) -> turnOn lights topLeft bottomRight
            | Toggle(topLeft, bottomRight) -> toggle lights topLeft bottomRight)

        lights |> Bits.popCount |> string |> output 1
