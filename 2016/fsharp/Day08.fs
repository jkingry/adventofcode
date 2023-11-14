namespace AdventOfCode.FSharp.Y2016

// Day 8: Two-Factor Authentication
module Day08 =
    open AdventOfCode.FSharp.Util

    let screenWidth = 50
    let screenHeight = 6

    type Instr =
        | Rect of a: int * b: int
        | RotateRow of row: int * right: int
        | RotateColumn of col: int * down: int

    let parseInstruction (line: string) =
        let parts = line.Split ' '

        match parts[0] with
        | "rect" ->
            let ab = parts[ 1 ].Split("x")
            Rect(ab[0] |> int, ab[1] |> int)
        | "rotate" ->
            let x = parts[ 2 ].Split("=")[1] |> int
            let y = parts[4] |> int

            match parts[1] with
            | "row" -> RotateRow(x, y)
            | "column" -> RotateColumn(x, y)
            | x -> failwithf "Invalid rotate instuction: %s" x
        | x -> failwithf "Invalid instuction: %s" x

    let applyInstruction (screen: char[,]) (i: Instr) =
        match i with
        | Rect(a, b) ->
            for row = 0 to (b - 1) do
                for col = 0 to (a - 1) do
                    screen[row, col] <- '#'
        | RotateRow(row, right) ->
            let tempRow = screen[row, *]

            for col = 0 to (screenWidth - 1) do
                let newCol = (col + right) % screenWidth
                screen[row, newCol] <- tempRow[col]
        | RotateColumn(col, down) ->
            let tempCol = screen[*, col]

            for row = 0 to (screenHeight - 1) do
                let newRow = (row + down) % screenHeight
                screen[newRow, col] <- tempCol[row]

        screen

    let run (input: byte array) output =
        let screen = Array2D.create screenHeight screenWidth '.'

        let instructions = input |> text |> splitLine |> Array.map parseInstruction

        let finalScreen = instructions |> Array.fold applyInstruction screen

        let mutable pixels = 0

        finalScreen
        |> Array2D.iter (fun v ->
            if v = '#' then
                pixels <- pixels + 1)

        pixels |> string |> output 1

        let mutable part2 = "\n"

        for row = 0 to (screenHeight - 1) do
            let tempRow = System.String screen[row, *]

            part2 <- part2 + tempRow + "\n"

        part2 |> output 2
