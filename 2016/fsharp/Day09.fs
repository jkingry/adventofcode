namespace AdventOfCode.FSharp.Y2016

// Day 9: Explosives in Cyberspace
module Day09 =
    open AdventOfCode.FSharp.Util

    let decompress (line: string) =
        let mutable buf = System.Text.StringBuilder()
        let mutable pos = 0

        while pos < line.Length do
            let startMarkerPos = line.IndexOf('(', pos)

            if startMarkerPos > pos then
                buf <- buf.Append(line.Substring(pos, startMarkerPos - pos))

            if startMarkerPos >= 0 then
                let endMarkerPos = line.IndexOf(')', startMarkerPos)

                let marker =
                    line.Substring(startMarkerPos + 1, endMarkerPos - startMarkerPos - 1).Split('x')

                let dataLen = marker[0] |> int
                let dataRep = marker[1] |> int
                let data = line.Substring(endMarkerPos + 1, dataLen)

                for _ = 1 to dataRep do
                    buf <- buf.Append(data)

                pos <- endMarkerPos + dataLen + 1
            else
                buf <- buf.Append(line.Substring(pos, line.Length - pos))
                pos <- line.Length

        buf.ToString()

    let run (input: byte array) output =

        input
        |> text
        |> splitLine
        |> Array.map (decompress >> String.length)
        |> Array.sum
        |> string
        |> output 1

        0 |> string |> output 2
