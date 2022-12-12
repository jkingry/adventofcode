namespace AdventOfCode.FSharp.Y2022

// Day 10
module Day10 =
    open Checked
    open AdventOfCode.FSharp.Util

    type Op =
    | AddX of int
    | Noop

    let run (input: byte array) (output: int -> string -> unit) =
        let parse (line: string) = 
            let p = line.Split(' ')
            match p[0] with
            | "addx" -> AddX (int p[1])
            | _ -> Noop

        let instrs = input |> text |> splitLine |> Array.map parse

        let cyclesOfInterest = Set [20;60;100;140;180;220]

        let mutable cycle = 0
        let mutable x = 1
        
        let mutable crtOutput = ""
        let mutable signalStrengthTotal = 0

        for op in instrs do
            let (cycleLen, amt) = 
                match op with
                | AddX amt -> (2, amt)
                | _ -> (1, 0) 

            for _ = 1 to cycleLen do
                let position = cycle % 40

                if position = 0 then crtOutput <- crtOutput + "\n"

                if (x - 1) <= position && position <= (x + 1) then
                    crtOutput <- crtOutput + "#"
                else
                    crtOutput <- crtOutput + "."

                cycle <- cycle + 1

                if cyclesOfInterest |> Set.contains cycle then
                    signalStrengthTotal <- signalStrengthTotal + (cycle * x)

            x <- x + amt
        
        signalStrengthTotal |> string |> output 1 
        crtOutput |> output 2 

    let runFast (input: byte array) (output: int -> string -> unit) =
        let charA = byte 'a'
        let newline = byte '\n'

        let parse (s: byte array) (pos: int) =
            let (nextPos, amt) = if s[pos] = charA then parseIntToDelim s (pos+5) newline else (pos+5, 0)

            let cycleLen = if amt = 0 then 1 else 2

            (nextPos, amt, cycleLen)

        let mutable cycle = 0
        let mutable x = 1

        let mutable crt = new System.Text.StringBuilder ()
        let mutable signalStrengthTotal = 0

        let mutable i = 0
        while i < (input.Length - 1) do        
            let (nextPos, amt, cycleLen) = parse input i
            i <- nextPos

            for _ = 1 to cycleLen do
                let position = cycle % 40

                if position = 0 then
                    crt <- crt.Append '\n'

                let pixel = if (x - 1) <= position && position <= (x + 1) then '#' else '.'
                crt <- crt.Append pixel

                cycle <- cycle + 1

                if cycle = 20 || ((cycle - 20) % 40 = 0 && cycle <= 220) then
                    signalStrengthTotal <- signalStrengthTotal + (cycle * x)

            x <- x + amt

        signalStrengthTotal |> string |> output 1
        crt.ToString () |> output 2 
