namespace AdventOfCode.FSharp.Y2015

// Day 18: Like a GIF For Your Yard

module Day18 =
    open AdventOfCode.FSharp.Util


    let movesToSeq (mx, my) (x, y) =
        seq {
            if x > 0 then
                if y > 0 then
                    yield (x - 1, y - 1)

                yield (x - 1, y)

                if y < (my - 1) then
                    yield (x - 1, y + 1)


            if y > 0 then
                yield (x, y - 1)

            if y < (my - 1) then
                yield (x, y + 1)

            if x < (mx - 1) then
                if y > 0 then
                    yield (x + 1, y - 1)

                yield (x + 1, y)

                if y < (my - 1) then
                    yield (x + 1, y + 1)
        }

    let run (input: byte array) (output: int -> string -> unit) =
        let lines = input |> bsplit '\n'B

        let width = lines |> Array.map (fun a -> a |> Array.length) |> Array.min
        let height = lines.Length

        let mutable yard1 =
            input
            |> Array.filter (fun c -> c <> '\n'B)
            |> Array.map (function
                | '#'B -> true
                | '.'B -> false
                | _ -> failwith "Unreachable")
            |> bits

        let mutable yard2 = bits yard1.Count

        let bounds = movesToSeq (width, height)

        for step = 1 to 100 do
            for x = 0 to width - 1 do
                for y = 0 to height - 1 do
                    let light = yard1[y * width + x]

                    let neighbors =
                        (x, y)
                        |> bounds
                        |> Seq.map (fun (px, py) -> if yard1[py * width + px] then 1 else 0)
                        |> Seq.sum

                    let newLight =
                        if light then
                            if 2 <= neighbors && neighbors <= 3 then true else false
                        else
                            neighbors = 3

                    yard2.Set(y * width + x, newLight)

            let temp = yard1
            yard1 <- yard2
            yard2 <- temp

        yard1 |> Bits.popCount |> string |> output 1
