namespace AdventOfCode.FSharp.Y2024

// Day 4: Ceres Search
module Day04 =
    open AdventOfCode.FSharp.Util

    let XMAS = "XMAS"B

    let dirs =
        [| (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1); (1, 0) |]

    let diags = [| (1, 1); (-1, 1); (-1, -1); (1, -1) |]

    let good = [ "SSMM"B; "MSSM"B; "SMMS"B; "MMSS"B ]

    let readPos (grid: byte[][]) (r, c) =
        if r >= 0 && c >= 0 && r < grid.Length && c < grid[r].Length then
            Some(grid[r][c])
        else
            None

    let search (grid: byte[][]) pos (dr, dc) =
        Seq.unfold (fun (r, c) -> readPos grid (r, c) |> Option.map (fun next -> next, (r + dr, c + dc))) pos
        |> Seq.truncate XMAS.Length
        |> Seq.compareWith (fun a b -> int (a - b)) XMAS

    let search2 (grid: byte[][]) (r, c) =
        let letters =
            diags
            |> Array.map (fun (dr, dc) -> r + dr, c + dc)
            |> Array.choose (readPos grid)

        if good |> List.contains letters then 1 else 0

    let run (input: byte array) (output: int -> string -> unit) =
        let lines = input |> bsplit '\n'B

        let mutable found = 0

        for row = 0 to lines.Length - 1 do
            for col = 0 to lines[row].Length - 1 do
                let c = lines[row][col]

                if c = 'X'B then
                    found <-
                        dirs
                        |> Array.map (search lines (row, col))
                        |> Array.fold (fun a v -> a + if v = 0 then 1 else 0) found

        found |> string |> output 1

        let mutable found = 0

        for row = 0 to lines.Length - 1 do
            for col = 0 to lines[row].Length - 1 do
                let c = lines[row][col]

                if c = 'A'B then
                    found <- found + search2 lines (row, col)

        found |> string |> output 2
