namespace AdventOfCode.FSharp.Y2019

// Day 3: Crossed Wires
module Day03 =
    open AdventOfCode.FSharp.Util
    open System.IO

    let parseLine (input: Stream) =
        seq {
            let mutable eol = false
            let mutable eof = false
            let mutable x = 0
            let mutable y = 0
            let mutable d = 0

            while not (eol || eof) do
                let res = input.ReadByte()

                if res < 0 then
                    eof <- true
                else
                    let delta = input |> readIntToAny
                    d <- d + delta

                    match byte res with
                    | 'R'B ->
                        yield false, (y, x, x + delta, d)
                        x <- x + delta
                    | 'L'B ->
                        yield false, (y, x, x - delta, d)
                        x <- x - delta
                    | 'U'B ->
                        yield true, (x, y, y + delta, d)
                        y <- y + delta
                    | 'D'B ->
                        yield true, (x, y, y - delta, d)
                        y <- y - delta
                    | _ -> failwithf "Invalid direction: %A" (char res)

                    let res = input.ReadByte()

                    if res < 0 then
                        eof <- true
                    else
                        match byte res with
                        | ','B -> ()
                        | '\n'B -> eol <- true
                        | c -> failwithf "Expected comma or eol: %A" c
        }

    let parse (input: byte[]) =
        let s = new MemoryStream(input)

        seq {
            while s.ReadByte() >= 0 do
                s.Seek(-1, SeekOrigin.Current) |> ignore

                yield parseLine s
        }


    let run (input: byte array) output =

        let mutable horizontal = Set.empty
        let mutable vertical = Set.empty
        let mutable first = true

        let mutable firstCrossManhatten = System.Int32.MaxValue
        let mutable minCrossDistanceTravelled = System.Int32.MaxValue

        for wire in parse input do
            if first then
                for segment in wire do
                    match segment with
                    | true, w -> vertical <- vertical |> Set.add w
                    | false, w -> horizontal <- horizontal |> Set.add w

                first <- false
            else
                for dir, (lvl2, f2, t2, d2) in wire do
                    let segments = if dir then horizontal else vertical
                    let min2 = min t2 f2
                    let max2 = max t2 f2

                    for lvl1, f1, t1, d1 in segments do
                        let min1 = min t1 f1
                        let max1 = max t1 f1

                        if
                            min2 <= lvl1
                            && lvl1 <= max2
                            && min1 <= lvl2
                            && lvl2 <= max1
                            && (lvl1 <> 0 || lvl2 <> 0)
                        then
                            let manhatten = abs lvl1 + abs lvl2
                            let d = d1 + d2 - (abs (t2 - lvl1) + abs (t1 - lvl2))
                            firstCrossManhatten <- min firstCrossManhatten manhatten
                            minCrossDistanceTravelled <- min minCrossDistanceTravelled d

        firstCrossManhatten |> string |> output 1
        minCrossDistanceTravelled |> string |> output 2
