namespace AdventOfCode.FSharp.Y2016

// Day 4
module Day04 =
    open AdventOfCode.FSharp.Util

    type LineParts =
        | EncryptedName
        | SectorID
        | Checksum

    let run (input: byte array) output =

        let lines = input |> bsplit '\n'B

        let parseEncryptedName (line: byte[]) =
            let mutable pos = 0
            let mutable counts = Map.empty
            let mutable parsed = false

            while not parsed do
                match line[pos] with
                | c when 'a'B <= c && c <= 'z'B ->
                    counts <- counts |> Counter.incr c
                    pos <- pos + 1
                | '-'B -> pos <- pos + 1
                | _ -> parsed <- true

            pos, counts

        let hasValidChecksum (counts: Map<byte, int64>) (pos: int) (line: byte[]) =
            let checksum =
                counts
                |> Map.toArray
                |> Array.sortBy (fun (k, v) -> (-v, k))
                |> Array.map fst
                |> Array.take 5

            let mutable validPos = 0
            let mutable checkPos = pos
            let mutable valid = true

            while valid && validPos < 5 do
                if line[checkPos] = checksum[validPos] then
                    checkPos <- checkPos + 1
                    validPos <- validPos + 1
                else
                    valid <- false

            valid

        let part1 (line: byte array) =
            let (sectorPos, counts) = parseEncryptedName line
            let (checksumPos, sectorId) = parseIntToDelim line sectorPos '['B

            if hasValidChecksum counts checksumPos line then
                sectorId
            else
                0

        lines |> Array.fold (fun sum line -> sum + (part1 line)) 0 |> string |> output 1

        let decryptName (line: byte array) (sectorId: int) =
            let buf = System.Text.StringBuilder line.Length
            let mutable parsed = false
            let mutable pos = 0

            while not parsed do
                match line[pos] with
                | c when 'a'B <= c && c <= 'z'B ->
                    let ecAlphaPos = (c - 'a'B) |> int
                    let deAlphaPos = (ecAlphaPos + sectorId) % 26 |> byte
                    let d = (deAlphaPos + 'a'B) |> char
                    buf.Append(d) |> ignore
                | '-'B -> buf.Append(' ') |> ignore
                | _ -> parsed <- true

                pos <- pos + 1

            buf.ToString()

        let part2 (objectiveRoom: string) (line: byte array) =
            let (sectorPos, counts) = parseEncryptedName line
            let (checksumPos, sectorId) = parseIntToDelim line sectorPos '['B

            if hasValidChecksum counts checksumPos line then
                let name = decryptName line sectorId
                if name = objectiveRoom then sectorId |> Some else None
            else
                None

        lines |> Seq.pick (part2 "northpole object storage ") |> string |> output 2
