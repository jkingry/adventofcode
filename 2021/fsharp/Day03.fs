namespace AdventOfCode.FSharp.Y2021

// Day 3: Binary Diagnostic
module Day03 =
    open AdventOfCode.FSharp.Util

    let fromBinary (s: string) = int ("0b" + s)

    let toBinary (input: int) =
        System.Convert.ToString(input, 2).ToCharArray() |> Array.rev |> System.String

    let run input (output: int -> string -> unit) =
        // Part 1
        let input = input |> text

        let countDigits (counts: Map<int, int>) (line: string) =
            line
            |> Seq.indexed
            |> Seq.fold (fun a (i, c) -> if c = '1' then (mapIncr i a) else (mapDecr i a)) counts

        let gammaCounts = input |> splitLine |> Seq.fold countDigits Map.empty

        let gammaText =
            gammaCounts
            |> Map.toSeq
            |> Seq.sortBy fst
            |> Seq.map (fun (_, v) -> if v > 0 then '1' else '0')
            |> Seq.toArray
            |> System.String

        let gamma = gammaText |> fromBinary

        let epsilon = ~~~gamma

        // trim to digit length
        let epsilon = epsilon &&& ((1 <<< gammaText.Length) - 1)

        gamma * epsilon |> string |> output 1

        // Part 2

        let filterReport (dir: bool) (input: string list) (p: int) =
            let m = input |> List.groupBy (fun s -> s.[p]) |> Map.ofList

            if (m.['0'].Length > m.['1'].Length) = dir then
                m.['0']
            else
                m.['1']

        let foldReport (input: string list) (dir: bool) =
            let n = (List.head input).Length

            [ 0 .. n - 1 ]
            |> Seq.scan (filterReport dir) input
            |> Seq.find (fun l -> l.Length = 1)
            |> List.head
            |> fromBinary

        let cache = input |> splitLine |> List.ofArray
        let oxygenGenerator = foldReport cache true
        let co2Scrubber = foldReport cache false
        oxygenGenerator * co2Scrubber |> string |> output 2
