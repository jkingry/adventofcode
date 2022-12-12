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
