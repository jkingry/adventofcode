namespace AdventOfCode.FSharp.Y2016

// Day 20: Firewall Rules
module Day20 =
    open AdventOfCode.FSharp.Util

    let parseInput (input: byte array) =
        seq {
            let mutable pos = 0

            while pos < input.Length do
                let p0 = pos
                let p1, startValue = parseIntToDelim input p0 '-'B
                let p2, endValue = parseIntToAny input p1
                pos <- p2 + 1

                yield (startValue |> uint, endValue |> uint)
        }

    let run (input: byte array) (output: int -> string -> unit) =
        let ranges =
            input
            |> parseInput
            |> Seq.fold
                (fun a (bs, be) ->
                    a
                    |> Map.keys
                    |> Seq.filter (fun (goodStart, goodEnd) -> intersects bs be goodStart goodEnd)
                    |> Seq.fold
                        (fun a' (gs, ge) ->
                            a'
                            |> Map.remove (gs, ge)
                            |> if gs < bs then Map.add (gs, bs - 1u) true else id
                            |> if ge > be then Map.add (be + 1u, ge) true else id)
                        a)
                (Map [ (0u, System.UInt32.MaxValue), true ])

        ranges |> Map.minKeyValue |> fst |> fst |> string |> output 1

        ranges
        |> Map.keys
        |> Seq.map (fun (a, b) -> (b - a) + 1u)
        |> Seq.sum
        |> string
        |> output 2
