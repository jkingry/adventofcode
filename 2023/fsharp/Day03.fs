namespace AdventOfCode.FSharp.Y2023

// Day 3: Gear Ratios
module Day03 =
    open AdventOfCode.FSharp.Util

    type Entry =
        | Symbol of int * int
        | Part of (int * int) * (int * int) * int

    let parse (input: byte[]) =
        seq {
            let mutable row = 0
            let mutable col = 0
            let mutable v = 0

            for b in input do

                if '0'B <= b && b <= '9'B then
                    v <- (v * 10) + int (b - '0'B)
                else
                    if b <> '.'B && b <> '\n'B then
                        yield Symbol(row, col)

                    if v > 0 then
                        let len = v |> float |> log10 |> ceil |> int
                        let br = row, col - 1
                        let tl = row, (col - len)
                        yield Part(tl, br, v)
                        v <- 0

                if b = '\n'B then
                    row <- row + 1
                    col <- 0
                else
                    col <- col + 1

        }

    let run (input: byte[]) (output: int -> string -> unit) =
        let isAdj symbol part =
            match symbol, part with
            | Symbol(x, y), Part((tx, ty), (bx, by), v) ->
                if (tx - 1) <= x && x <= (bx + 1) && (ty - 1) <= y && y <= (by + 1) then
                    Some v
                else
                    None
            | _ -> None

        let sumParts (total, (parts, symbols)) entry =
            match entry with
            | Symbol _ ->
                parts
                |> List.fold
                    (fun (t, (plist, s)) p ->
                        match isAdj entry p with
                        | Some v -> t + v, (plist, s)
                        | None -> t, (p :: plist, s))
                    (total, ([], entry :: symbols))
            | Part _ ->
                symbols
                |> List.tryPick (fun s -> isAdj s entry)
                |> function
                    | Some v -> total + v, (parts, symbols)
                    | None -> total, (entry :: parts, symbols)

        input |> parse |> Seq.fold sumParts (0, ([], [])) |> fst |> string |> output 1
