namespace AdventOfCode.FSharp.Y2015

// Day 5: Doesn't He Have Intern-Elves For This?  https://adventofcode.com/2015/day/5
module Day05 =
    open AdventOfCode.FSharp.Util

    let INVALID = '!'B

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
                (INVALID, 0, false, 0)
            |> Seq.takeWhile (function
                | (_, _, _, x) -> x < 2)
            |> Seq.last

        hasBad = 0 && hasDouble && vowelCount >= 3

    let IsNiceString2 (input: byte array) =
        let mutable p0 = INVALID
        let mutable p1 = INVALID

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


    let combineFolder3 (fa, fb, fc) (a, b, c) x =
        (fa a x), (fb b x), (fc c x)

    let VOWELS = "aeiou"B

    let BAD_STRINGS = [|
        "ab"B
        "cd"B
        "pq"B
        "xy"B
    |]

    let IsNiceString3 (input: byte array) =
        let vowelCountScan vowelCount a =
            if VOWELS |> Array.contains a then vowelCount + 1
            else vowelCount

        let hasDoubleScan (found, prevChar) a =
            let found = found || prevChar = a
            found, a

        let hasBad (found, prevChar) a =
            let pair = [| prevChar; a |]
            let found = 
                if found > 0 then found + 1
                elif BAD_STRINGS |> Array.contains pair then 1
                else 0
            found, a

        let folder = combineFolder3 (vowelCountScan, hasDoubleScan, hasBad)
 
        let (vowelCount, hasDouble, hasBad) =
            input
            |> Seq.scan folder
                (0, (false, INVALID), (0, INVALID))
            |> Seq.takeWhile (fun (_, _, (x, _)) -> x < 2)
            |> Seq.last

        if (fst hasBad) = 0 && (fst hasDouble) && vowelCount >= 3 then 1 else 0

    let combineFolder2 (fa, fb) (a, b) x =
        (fa a x), (fb b x)

    let IsNiceString4 (input: byte array) =
        let hasPair (found, p) (index, c) =
            let found =
                found || (index >= 3 && input |> Array.take (index - 1) |> Seq.pairwise |> Seq.contains (p, c))
            found, c
        
        let hasSep (found, p0, p1) (_, c) =
            let found = found || p0 = c
            found, p1, c

        let folder = combineFolder2 (hasPair, hasSep)

        let nice = 
            input
            |> Seq.indexed
            |> Seq.scan folder ((false, INVALID), (false, INVALID, INVALID))
            |> Seq.exists (fun ((foundPair, _),(foundSep, _, _)) -> foundPair && foundSep)
        
        if nice then 1 else 0 

    let runNoLoops (input: byte array) (output: int -> string -> unit) =
        let lines = input |> bsplit '\n'B

        lines |> Seq.map IsNiceString3 |> Seq.sum |> string |> output 1
        lines |> Seq.map IsNiceString4 |> Seq.sum |> string |> output 2
