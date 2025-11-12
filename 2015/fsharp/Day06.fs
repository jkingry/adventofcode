namespace AdventOfCode.FSharp.Y2015

// Day 6: Probably a Fire Hazard
module Day06 =
    open AdventOfCode.FSharp.Util

    type Pos = int * int

    type InstructionCommand =
        | TurnOff
        | TurnOn
        | Toggle

    type Instruction = InstructionCommand * (Pos * Pos)

    let parseLine (line: string) =
        match line with
        | Regex "(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)" [ txt; a; b; c; d ] ->
            let topLeft = int a, int b
            let bottomRight = int c, int d
            let coords = topLeft, bottomRight

            match txt with
            | "turn off" -> TurnOff, coords
            | "turn on" -> TurnOn, coords
            | "toggle" -> Toggle, coords
            | s -> failwithf "Invalid instruction: %s" s
        | _ -> failwithf "Invalid line: %s" line

    type Rectangle = Pos * Pos

    type RectangleList<'a> = (Rectangle * 'a) list

    module Rect =
        let getPoints (r: Rectangle) =
            let (x1, y1), (x2, y2) = r
            [| x1, y1; x1, y2; x2, y1; x2, y2 |]

        let contains (p: Pos) (r: Rectangle) =
            let x, y = p
            let (x1, y1), (x2, y2) = r
            x1 <= x && x <= x2 && y1 <= y && y <= y2

        let area (r: Rectangle) =
            let (x1, y1), (x2, y2) = r
            (1 + x2 - x1) * (1 + y2 - y1)

        let overlap (a: Rectangle) (b: Rectangle) : Rectangle option =
            let (x1a, y1a), (x2a, y2a) = a
            let (x1b, y1b), (x2b, y2b) = b

            if x1a > x2b || x1b > x2a || y1a > y2b || y1b > y2a then
                None
            else
                let x1r = max x1a x1b
                let y1r = max y1a y1b
                let x2r = min x2a x2b
                let y2r = min y2a y2b
                ((x1r, y1r), (x2r, y2r)) |> Some

        let remove (source: Rectangle) (section: Rectangle) : Rectangle seq =
            // Returns the remaining pieces of `source` after removing `section`
            // `section` is guaranteed to be contained in `source`
            let (x1s, y1s), (x2s, y2s) = source
            let (x1r, y1r), (x2r, y2r) = section

            seq {
                if x1s < x1r then
                    yield (x1s, y1s), (x1r - 1, y2s) // left piece

                if x2s > x2r then
                    yield (x2r + 1, y1s), (x2s, y2s) // right piece

                if y1s < y1r then
                    yield (max x1s x1r, y1s), (min x2s x2r, y1r - 1) // top piece

                if y2s > y2r then
                    yield (max x1s x1r, y2r + 1), (min x2s x2r, y2s) // bottom piece
            }

    module RectList =
        let change
            (addRect: Rectangle)
            (changer: Option<'a> -> Option<'a>)
            (rects: RectangleList<'a>)
            : RectangleList<'a> =
            let mutable result = []

            for entry in rects do
                let entryRect, oldValue = entry

                match Rect.overlap addRect entryRect with
                | Some overlap ->
                    match changer (Some oldValue) with
                    | Some newValue -> result <- (overlap, newValue) :: result
                    | None -> ()

                    if entryRect <> overlap then
                        // add remaining pieces
                        for remainder in Rect.remove entryRect overlap do
                            result <- (remainder, oldValue) :: result
                | None -> result <- entry :: result

            result


    let runRects (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> text |> splitLine |> Seq.map parseLine

        let field = ((0, 0), (999, 999)), (false, 0)

        let applyInstruction cmd (switchState, lightTotal) =
            match cmd with
            | TurnOff -> false, lightTotal - 1 |> max 0
            | TurnOn -> true, lightTotal + 1
            | Toggle -> not switchState, lightTotal + 2

        let part1, part2 =
            instructions
            |> Seq.fold
                (fun field (cmd, coords) -> field |> RectList.change coords (applyInstruction cmd |> Option.map))
                [ field ]
            |> Seq.fold
                (fun (part1, part2) (r, (switchState, lightTotal)) ->
                    let rectangleArea = r |> Rect.area
                    let part1' = part1 + if switchState then rectangleArea else 0
                    let part2' = part2 + rectangleArea * lightTotal
                    part1', part2')
                (0, 0)

        part1 |> string |> output 1
        part2 |> string |> output 2
