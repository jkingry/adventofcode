namespace AdventOfCode.FSharp.Y2016

// Day 15: Timing is Everything
module Day15 =
    open AdventOfCode.FSharp.Util

    let parseLine (line: string) =
        match line with
        | Regex @"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)." [ disc; n; offset ] ->
            (disc |> int, n |> int, offset |> int)
        | _ -> failwithf "Bad format: %s" line

    let solve parsed =
        let M = parsed |> Array.fold (fun a (_, p, _) -> a * p) 1

        let x =
            parsed
            |> Array.fold
                (fun x (d, p, o) ->
                    let Mi = M / p
                    let mi = p

                    // Ni = Mi^(mi - 2)
                    let mutable Ni = 1

                    for _ = 1 to (mi - 2) do
                        Ni <- (Ni * Mi) % mi

                    let offsetAtTime = (d + o) % mi
                    let offsetNeeded = mi - offsetAtTime
                    x + (offsetNeeded * Ni * Mi))
                0

        x % M

    let run (input: byte array) (output: int -> string -> unit) =
        let parsed = input |> text |> splitLine |> Array.map parseLine

        parsed |> solve |> string |> output 1

        let newdisk = parsed.Length + 1, 11, 0
        let part2 = parsed |> Array.append [| newdisk |]
        part2 |> solve |> string |> output 2
