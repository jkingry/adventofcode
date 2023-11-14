namespace AdventOfCode.FSharp.Y2016

// Day 9: Explosives in Cyberspace
module Day09 =
    open AdventOfCode.FSharp.Util

    let rec decompressLength (recurse: bool) (line: char array) =
        match line |> Array.tryFindIndex (fun c -> c = '(') with
        | Some startMarkerPos ->
            let prefixLen = startMarkerPos |> max 0 |> int64

            let endMarkerPos = line |> Array.findIndex (fun c -> c = ')')

            let marker = (System.String line[startMarkerPos + 1 .. endMarkerPos - 1]).Split('x')

            let dataLen = marker[0] |> int
            let dataRep = marker[1] |> int64

            let dataStart = endMarkerPos + 1
            let dataEnd = dataStart + dataLen - 1

            let patternLen =
                if recurse then
                    decompressLength recurse line[dataStart..dataEnd]
                else
                    dataLen |> int64

            let suffixLen = decompressLength recurse line[dataEnd + 1 ..]
            prefixLen + (dataRep * patternLen) + suffixLen
        | None -> line |> Array.length |> int64

    let run (input: byte array) output =
        let lines = input |> text |> splitLine

        let part1Decompress = decompressLength false

        lines
        |> Array.map (fun s -> part1Decompress (s.ToCharArray()))
        |> Array.sum
        |> string
        |> output 1

        let part2Decompress = decompressLength true

        lines
        |> Array.map (fun s -> part2Decompress (s.ToCharArray()))
        |> Array.sum
        |> string
        |> output 2
