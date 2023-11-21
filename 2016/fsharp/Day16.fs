namespace AdventOfCode.FSharp.Y2016

// Day 16: Dragon Checksum
module Day16 =
    open AdventOfCode.FSharp.Util

    module Bits =
        let toString (bits: System.Collections.BitArray) =
            let mutable buf = System.Text.StringBuilder bits.Count

            for i = 1 to bits.Count do
                let p = bits.Count - i
                buf <- buf.Append(if bits[p] then '1' else '0')

            buf.ToString()

        let reverse =
            [| 0x00uy
               0x80uy
               0x40uy
               0xc0uy
               0x20uy
               0xa0uy
               0x60uy
               0xe0uy
               0x10uy
               0x90uy
               0x50uy
               0xd0uy
               0x30uy
               0xb0uy
               0x70uy
               0xf0uy
               0x08uy
               0x88uy
               0x48uy
               0xc8uy
               0x28uy
               0xa8uy
               0x68uy
               0xe8uy
               0x18uy
               0x98uy
               0x58uy
               0xd8uy
               0x38uy
               0xb8uy
               0x78uy
               0xf8uy
               0x04uy
               0x84uy
               0x44uy
               0xc4uy
               0x24uy
               0xa4uy
               0x64uy
               0xe4uy
               0x14uy
               0x94uy
               0x54uy
               0xd4uy
               0x34uy
               0xb4uy
               0x74uy
               0xf4uy
               0x0cuy
               0x8cuy
               0x4cuy
               0xccuy
               0x2cuy
               0xacuy
               0x6cuy
               0xecuy
               0x1cuy
               0x9cuy
               0x5cuy
               0xdcuy
               0x3cuy
               0xbcuy
               0x7cuy
               0xfcuy
               0x02uy
               0x82uy
               0x42uy
               0xc2uy
               0x22uy
               0xa2uy
               0x62uy
               0xe2uy
               0x12uy
               0x92uy
               0x52uy
               0xd2uy
               0x32uy
               0xb2uy
               0x72uy
               0xf2uy
               0x0auy
               0x8auy
               0x4auy
               0xcauy
               0x2auy
               0xaauy
               0x6auy
               0xeauy
               0x1auy
               0x9auy
               0x5auy
               0xdauy
               0x3auy
               0xbauy
               0x7auy
               0xfauy
               0x06uy
               0x86uy
               0x46uy
               0xc6uy
               0x26uy
               0xa6uy
               0x66uy
               0xe6uy
               0x16uy
               0x96uy
               0x56uy
               0xd6uy
               0x36uy
               0xb6uy
               0x76uy
               0xf6uy
               0x0euy
               0x8euy
               0x4euy
               0xceuy
               0x2euy
               0xaeuy
               0x6euy
               0xeeuy
               0x1euy
               0x9euy
               0x5euy
               0xdeuy
               0x3euy
               0xbeuy
               0x7euy
               0xfeuy
               0x01uy
               0x81uy
               0x41uy
               0xc1uy
               0x21uy
               0xa1uy
               0x61uy
               0xe1uy
               0x11uy
               0x91uy
               0x51uy
               0xd1uy
               0x31uy
               0xb1uy
               0x71uy
               0xf1uy
               0x09uy
               0x89uy
               0x49uy
               0xc9uy
               0x29uy
               0xa9uy
               0x69uy
               0xe9uy
               0x19uy
               0x99uy
               0x59uy
               0xd9uy
               0x39uy
               0xb9uy
               0x79uy
               0xf9uy
               0x05uy
               0x85uy
               0x45uy
               0xc5uy
               0x25uy
               0xa5uy
               0x65uy
               0xe5uy
               0x15uy
               0x95uy
               0x55uy
               0xd5uy
               0x35uy
               0xb5uy
               0x75uy
               0xf5uy
               0x0duy
               0x8duy
               0x4duy
               0xcduy
               0x2duy
               0xaduy
               0x6duy
               0xeduy
               0x1duy
               0x9duy
               0x5duy
               0xdduy
               0x3duy
               0xbduy
               0x7duy
               0xfduy
               0x03uy
               0x83uy
               0x43uy
               0xc3uy
               0x23uy
               0xa3uy
               0x63uy
               0xe3uy
               0x13uy
               0x93uy
               0x53uy
               0xd3uy
               0x33uy
               0xb3uy
               0x73uy
               0xf3uy
               0x0buy
               0x8buy
               0x4buy
               0xcbuy
               0x2buy
               0xabuy
               0x6buy
               0xebuy
               0x1buy
               0x9buy
               0x5buy
               0xdbuy
               0x3buy
               0xbbuy
               0x7buy
               0xfbuy
               0x07uy
               0x87uy
               0x47uy
               0xc7uy
               0x27uy
               0xa7uy
               0x67uy
               0xe7uy
               0x17uy
               0x97uy
               0x57uy
               0xd7uy
               0x37uy
               0xb7uy
               0x77uy
               0xf7uy
               0x0fuy
               0x8fuy
               0x4fuy
               0xcfuy
               0x2fuy
               0xafuy
               0x6fuy
               0xefuy
               0x1fuy
               0x9fuy
               0x5fuy
               0xdfuy
               0x3fuy
               0xbfuy
               0x7fuy
               0xffuy |]

        let rev (ba: System.Collections.BitArray) =
            let bytesCount = (ba.Count |> float) / 8.0 |> ceil |> int
            let bytes: byte array = Array.zeroCreate bytesCount
            ba.CopyTo(bytes, 0)
            let reverseBytes = bytes |> Array.map (fun i -> reverse[i |> int]) |> Array.rev
            let reverseBa = System.Collections.BitArray reverseBytes
            reverseBa.RightShift(reverseBa.Count - ba.Count) |> ignore
            reverseBa.Length <- ba.Count
            reverseBa

        let fromString (inputText: string) =
            let parsed = int ("0b" + inputText)
            let initial = System.Collections.BitArray [| parsed |]
            initial.Length <- inputText.Length
            initial

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

        let rec checksum (bits: System.Collections.BitArray) =
            if bits.Count % 2 = 1 then
                bits
            else
                let checksumLen = bits.Count / 2

                let result: System.Collections.BitArray =
                    [ 0 .. checksumLen - 1 ]
                    |> List.fold
                        (fun c ci ->
                            let ai = ci * 2
                            let bi = ai + 1
                            c[ci] <- bits[ai] = bits[bi]
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
