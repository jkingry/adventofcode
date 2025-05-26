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

            while not (eol || eof) do
                let res = input.ReadByte()

                if res < 0 then
                    eof <- true
                else
                    let delta = input |> readIntToAny

                    match byte res with
                    | 'R'B ->
                        yield false, (y, x, x + delta)
                        x <- x + delta
                    | 'L'B ->
                        yield false, (y, x - delta, x)
                        x <- x - delta
                    | 'U'B ->
                        yield true, (x, y, y + delta)
                        y <- y + delta
                    | 'D'B ->
                        yield true, (x, y - delta, y)
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

        let mutable crossDistance = System.Int32.MaxValue

        for wire in parse input do
            if first then
                for segment in wire do
                    match segment with
                    | true, w -> vertical <- vertical |> Set.add w
                    | false, w -> horizontal <- horizontal |> Set.add w

                first <- false
            else
                for dir, (lvl2, f2, t2) in wire do
                    let segments = if dir then horizontal else vertical

                    for lvl1, f1, t1 in segments do
                        if f2 <= lvl1 && lvl1 <= t2 && f1 <= lvl2 && lvl2 <= t1 && (lvl1 <> 0 || lvl2 <> 0) then
                            let dist = abs lvl1 + abs lvl2
                            crossDistance <- min crossDistance dist

        crossDistance |> string |> output 1
        0 |> string |> output 2
