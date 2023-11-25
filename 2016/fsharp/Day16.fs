namespace AdventOfCode.FSharp.Y2016

// Day 16: Dragon Checksum
module Day16 =
    open AdventOfCode.FSharp.Util

    let dragonCurve (a: System.Collections.BitArray) =
        let mutable b = Bits.rev a
        b <- b.Not()
        let originalLength = a.Length
        let newLength = (originalLength * 2) + 1
        a.Length <- newLength
        b.Length <- newLength
        let a' = a.LeftShift(originalLength + 1)
        a'.Or(b)

    let runBits (input: byte array) (output: int -> string -> unit) =

        let fill (seed: System.Collections.BitArray) targetLength =
            let mutable a = System.Collections.BitArray seed

            while a.Count < targetLength do
                a <- dragonCurve a

            a <- a.RightShift(a.Count - targetLength)
            a.Length <- targetLength
            a

        let rec checksum (b: bits) =
            if b.Count % 2 = 1 then
                b
            else
                let checksumLen = b.Count / 2

                let result: System.Collections.BitArray =
                    [ 0 .. checksumLen - 1 ]
                    |> List.fold
                        (fun c ci ->
                            let ai = ci * 2
                            let bi = ai + 1
                            c[ci] <- b[ai] = b[bi]
                            c)
                        (System.Collections.BitArray checksumLen)

                checksum result

        let initial = input |> text |> splitLine |> Array.head |> Bits.fromString

        fill initial 272 |> checksum |> Bits.toString |> output 1
        fill initial 35651584 |> checksum |> Bits.toString |> output 2

    let runSeq (input: byte array) (output: int -> string -> unit) =
        let dragonFill (seed: System.Collections.BitArray) =
            let generate (i, spacers: System.Collections.BitArray) =
                let spacePos = i / (seed.Count + 1)
                let patPos = (i + 1) % (seed.Count + 1)

                if patPos = 0 then
                    let spacers' =
                        if spacePos >= spacers.Count then
                            dragonCurve spacers
                        else
                            spacers

                    let state' = (i + 1, spacers')
                    let value = spacers'[spacers'.Count - (spacePos + 1)]
                    Some(value, state')
                else
                    let state' = (i + 1, spacers)

                    if spacePos % 2 = 0 then
                        let value = seed[seed.Count - patPos]
                        Some(value, state')
                    else
                        let value = not seed[patPos - 1]
                        Some(value, state')

            Seq.unfold generate (0, System.Collections.BitArray 1)

        let rec checksumSeq (length: int) (s: bool seq) =
            if length % 2 = 1 then
                s |> Seq.take length
            else
                let checksumLen = length / 2

                seq {
                    let mutable a = false
                    let mutable b = false
                    let mutable i = 0

                    for v in s do
                        if i % 2 = 0 then
                            a <- v
                        else
                            b <- v
                            yield a = b

                        i <- i + 1
                }
                |> checksumSeq checksumLen

        let boolSeqToString (s: bool seq) =
            let mutable buf = System.Text.StringBuilder()

            for v in s do
                buf <- buf.Append(if v then '1' else '0')

            buf.ToString()

        let initial = input |> text |> splitLine |> Array.head |> Bits.fromString

        dragonFill initial |> checksumSeq 272 |> boolSeqToString |> output 1
        dragonFill initial |> checksumSeq 35651584 |> boolSeqToString |> output 2
