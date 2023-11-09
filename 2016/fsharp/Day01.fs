namespace AdventOfCode.FSharp.Y2016

// Day 1: No Time for a Taxicab
module Day01 =
    open AdventOfCode.FSharp.Util

    type Turn =
        | Left
        | Right
        | Straight

    let run (input: byte array) output =
        let instructions =
            seq {
                let mutable pos = 0

                while pos < input.Length do
                    let (pos', steps) = parseIntToDelim input (pos + 1) ','B

                    let turn =
                        match input[pos] with
                        | 'L'B -> Left
                        | 'R'B -> Right
                        | c -> failwithf "bad format, invalid turn: %A" (char c)

                    pos <- pos' + 1

                    yield turn, steps
            }
            |> List.ofSeq

        let followDirection (facing, x, y) (turn, steps) =
            let facing' =
                match turn with
                | Right -> (4 + (facing + 1)) % 4
                | Left -> (4 + (facing - 1)) % 4
                | Straight -> facing

            let (x', y') =
                match facing' with
                | 0 -> x, y + steps
                | 1 -> x + steps, y
                | 2 -> x, y - steps
                | 3 -> x - steps, y
                | f -> failwithf "impossible: %A" f

            facing', x', y'

        let (_, x, y) = instructions |> List.fold followDirection (0, 0, 0)

        x + y |> string |> output 1

        let rec findVisitedTwice visited pos instructions =
            match instructions with
            | (turn, steps) :: xs ->
                let (facing', x', y') = followDirection pos (turn, 1)

                if visited |> Set.contains (x', y') then
                    Some(x', y')
                else
                    let rest = if steps > 1 then (Straight, steps - 1) :: xs else xs
                    let visited' = visited |> Set.add (x', y')
                    rest |> findVisitedTwice visited' (facing', x', y')
            | _ -> None

        let (vx, vy) = instructions |> findVisitedTwice Set.empty (0, 0, 0) |> Option.get

        vx + vy |> string |> output 2
