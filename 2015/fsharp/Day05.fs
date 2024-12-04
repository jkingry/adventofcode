namespace AdventOfCode.FSharp.Y2015

// Day 5: Doesn't He Have Intern-Elves For This?  https://adventofcode.com/2015/day/5
module Day05 =
    open AdventOfCode.FSharp.Util

    let IsNiceString (input: byte array) =
        let (_, vowelCount, hasDouble, hasBad) =
            input
            |> Seq.scan
                (fun (prevChar, vowelCount, hasDouble, hasBad) a ->
                    // vowel check
                    let vowelCount' =
                        match a with
                        | 'a'B
                        | 'e'B
                        | 'i'B
                        | 'o'B
                        | 'u'B -> vowelCount + 1
                        | _ -> vowelCount

                    let hasDouble' = hasDouble || prevChar = a

                    let hasBad' =
                        if hasBad > 0 then
                            hasBad + 1
                        else
                            match prevChar, a with
                            | 'a'B, 'b'B
                            | 'c'B, 'd'B
                            | 'p'B, 'q'B
                            | 'x'B, 'y'B -> 1
                            | _ -> 0

                    a, vowelCount', hasDouble', hasBad')
                ('!'B, 0, false, 0)
            |> Seq.takeWhile (function
                | (_, _, _, x) -> x < 2)
            |> Seq.last

        hasBad = 0 && hasDouble && vowelCount >= 3

    let IsNiceString2 (input: byte array) =
        let mutable p0 = '!'B
        let mutable p1 = '!'B

        let mutable foundPair = false
        let mutable foundSep = false

        let mutable index = 0

        while not (foundPair && foundSep) && index < input.Length do
            let c = input[index]
            foundSep <- foundSep || p0 = c

            if not foundPair && index >= 3 then
                foundPair <- input |> Array.take (index - 1) |> Seq.pairwise |> Seq.contains (p1, c)

            p0 <- p1
            p1 <- c
            index <- index + 1

        foundPair && foundSep


    let run (input: byte array) (output: int -> string -> unit) =
        let lines = input |> bsplit '\n'B

        lines |> Seq.filter IsNiceString |> Seq.length |> string |> output 1

        lines |> Seq.filter IsNiceString2 |> Seq.length |> string |> output 2
