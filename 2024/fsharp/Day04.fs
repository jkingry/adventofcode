namespace AdventOfCode.FSharp.Y2024

// Day 4: Ceres Search
module Day04 =
    open AdventOfCode.FSharp.Util

    let XMAS = "XMAS"B

    let dirs =
        [| (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1); (1, 0) |]

    let diags = [| (1, 1); (-1, 1); (-1, -1); (1, -1) |]

    let good = [ "SSMM"B; "MSSM"B; "SMMS"B; "MMSS"B ]

    let readPos (grid: byte[][]) pos =
        let (r, c) = pos

        if r >= 0 && c >= 0 && r < grid.Length && c < grid[r].Length then
            Some(grid[r][c])
        else
            None

    let findXmas (grid: byte[][]) pos (dr, dc) =
        let found =
            pos
            |> Seq.unfold (fun cur ->
                let r, c = cur
                let next = r + dr, c + dc
                Some(cur, next))
            |> Seq.map (readPos grid)
            |> Seq.truncate XMAS.Length
            |> Seq.choose id
            |> Seq.compareWith (fun a b -> int (a - b)) XMAS

        if found = 0 then 1 else 0

    let find2Mas (grid: byte[][]) (r, c) =
        let letters =
            diags
            |> Array.map (fun (dr, dc) -> r + dr, c + dc)
            |> Array.choose (readPos grid)

        if good |> List.contains letters then 1 else 0

    let run (input: byte array) (output: int -> string -> unit) =
        let lines = input |> bsplit '\n'B

        let mutable found1 = 0
        let mutable found2 = 0

        for row = 0 to lines.Length - 1 do
            for col = 0 to lines[row].Length - 1 do
                let c = lines[row][col]

                if c = 'X'B then
                    found1 <- found1 + (dirs |> Array.map (findXmas lines (row, col)) |> Array.sum)

                if c = 'A'B then
                    found2 <- found2 + find2Mas lines (row, col)

        found1 |> string |> output 1
        found2 |> string |> output 2
