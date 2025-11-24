namespace AdventOfCode.FSharp.Y2015

// Day 11
module Day11 =
    open AdventOfCode.FSharp.Util

    let OVERFLOW = 'z'B + 1uy

    let run (input: byte array) (output: int -> string -> unit) =
        let passwd = input |> Array.truncate 8

        let hasIllegalLetters (s: byte array) =
            let mutable p = 0
            let mutable illegal = false

            while not illegal && p < s.Length do
                illegal <-
                    match s[p] with
                    | 'i'B
                    | 'o'B
                    | 'l'B -> true
                    | _ -> false

                p <- p + 1

            illegal

        let hasStraight (s: byte array) =
            let mutable p = 0
            let mutable straightFound = false

            while not straightFound && p < s.Length - 2 do
                let a = s[p]
                let b = s[p + 1]
                let c = s[p + 2]
                straightFound <- a + 1uy = b && b + 1uy = c
                p <- p + 1

            straightFound

        let hasDouble (s: byte array) =
            let mutable p = 0
            let mutable firstDouble = '*'B
            let mutable doubleCount = 0

            while doubleCount <> 2 && p < s.Length - 1 do
                let c = s[p]

                match doubleCount with
                | 0 when c = s[p + 1] ->
                    doubleCount <- 1
                    firstDouble <- c
                | 1 when c <> firstDouble && c = s[p + 1] -> doubleCount <- 2
                | _ -> ()

                p <- p + 1

            doubleCount = 2

        let isLegal (s: byte array) =
            hasStraight s && hasIllegalLetters s |> not && hasDouble s

        let incr (s: byte array) =
            let mutable p = s.Length - 1
            let mutable overflowed = true

            while overflowed do
                s[p] <- s[p] + 1uy

                if s[p] <> OVERFLOW then
                    overflowed <- false
                else
                    s[p] <- 'a'B
                    p <- p - 1

        while passwd |> isLegal |> not do
            incr passwd

        passwd |> text |> output 1

        incr passwd

        while passwd |> isLegal |> not do
            incr passwd

        passwd |> text |> output 2
