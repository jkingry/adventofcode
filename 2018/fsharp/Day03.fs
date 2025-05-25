namespace AdventOfCode.FSharp.Y2018

// Day 3: No Matter How You Slice It
module Day03 =
    open AdventOfCode.FSharp.Util

    let parseLine line =
        match line with
        // #25 @ 410,958: 11x12
        | Regex @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" [ claimId; x; y; w; h ] ->
            int16 claimId, int x, int y, int w, int h
        | _ -> line |> failwithf "Invalid claim: %s"

    let parse input =
        input |> text |> splitLine |> Seq.map parseLine

    let run (input: byte array) output =
        let claims = parse input
        let field = Array2D.zeroCreate 1000 1000
        let ids = Array2D.zeroCreate 1000 1000

        let mutable availableClaims = Set.empty

        for id, x, y, w, h in claims do
            let mutable intersected = false

            for i = x to x + w - 1 do
                for j = y to y + h - 1 do
                    if field[i, j] < 2uy then
                        field[i, j] <- field[i, j] + 1uy

                    if field[i, j] = 1uy then
                        ids[i, j] <- id
                    else
                        availableClaims <- availableClaims |> Set.remove ids[i, j]
                        intersected <- true

            if not intersected then
                availableClaims <- availableClaims |> Set.add id

        field
        |> Array2D.fold (fun s v -> if v > 1uy then s + 1 else s) 0
        |> string
        |> output 1

        availableClaims |> Seq.head |> string |> output 2
