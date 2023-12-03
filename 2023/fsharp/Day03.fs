namespace AdventOfCode.FSharp.Y2023

// Day 3: Gear Ratios
module Day03 =
    open AdventOfCode.FSharp.Util

    type Entry =
        | Symbol of int * int * byte
        | Part of (int * int) * (int * int) * int
        | Row

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
                        yield Symbol(row, col, b)

                    if v > 0 then
                        let len = v |> float |> log10 |> ceil |> int
                        let br = row, col - 1
                        let tl = row, (col - len)
                        yield Part(tl, br, v)
                        v <- 0

                if b = '\n'B then
                    row <- row + 1
                    col <- 0
                    yield Row
                else
                    col <- col + 1

        }

    let isAdj symbol part =
        match symbol, part with
        | Symbol(x, y, b), Part((tx, ty), (bx, by), v) ->
            if (tx - 1) <= x && x <= (bx + 1) && (ty - 1) <= y && y <= (by + 1) then
                Some(v, (x, y))
            else
                None
        | _ -> None

    let matchPartsSymbols matchFunc values =
        let mutable prevParts = List.empty
        let mutable currParts = List.empty
        let mutable prevSymbs = List.empty
        let mutable currSymbs = List.empty

        for entry in values do
            match entry with
            | Row ->
                // Only keep previous row of part map to match against
                prevParts <- currParts
                currParts <- List.empty
                prevSymbs <- currSymbs
                currSymbs <- List.empty
            | Symbol _ ->
                // See if the previous entry in this row was an adj. part
                let removePart =
                    currParts
                    |> List.tryHead
                    |> Option.bind (isAdj entry)
                    |> Option.bind (fun (v, b) -> matchFunc b v |> Some)
                    |> Option.defaultValue false

                if removePart then
                    currParts <- currParts |> List.tail

                let mutable prevParts' = []

                // check row above for parts
                prevParts <-
                    prevParts
                    |> List.fold
                        (fun newPrev p ->
                            let removePart =
                                isAdj entry p
                                |> Option.bind (fun (v, b) -> matchFunc b v |> Some)
                                |> Option.defaultValue false

                            if removePart then newPrev else p :: newPrev)
                        List.empty

                currSymbs <- entry :: currSymbs
            | Part _ ->
                // Add the previous symbol in this row if available
                let s =
                    currSymbs
                    |> List.tryHead
                    |> function
                        | Some s -> s :: prevSymbs
                        | None -> prevSymbs

                let removePart =
                    s
                    |> List.tryPick (fun s -> isAdj s entry)
                    |> Option.bind (fun (v, b) -> matchFunc b v |> Some)
                    |> Option.defaultValue false

                if not removePart then
                    currParts <- entry :: currParts

    let run (input: byte[]) (output: int -> string -> unit) =
        let parsed = input |> parse |> Seq.cache

        let mutable partNumberTotal = 0

        parsed
        |> matchPartsSymbols (fun _ v ->
            partNumberTotal <- partNumberTotal + v
            true)

        partNumberTotal |> string |> output 1

        let gearsOnly =
            parsed
            |> Seq.filter (function
                | Symbol(_, _, b) when b <> '*'B -> false
                | _ -> true)

        let mutable gears = Map.empty

        gearsOnly
        |> matchPartsSymbols (fun b v ->
            gears <-
                gears
                |> Map.change b (function
                    | Some s -> v :: s |> Some
                    | None -> [ v ] |> Some)

            false)

        gears
        |> Map.values
        |> Seq.choose (function
            | [ a; b ] -> a * b |> Some
            | _ -> None)
        |> Seq.sum
        |> string
        |> output 2
