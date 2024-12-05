namespace AdventOfCode.FSharp.Y2024

// Day 5
module Day05 =
    open Checked
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let parts =
            input
            |> text
            |> splitDoubleLine
        let orderRules = 
            parts[0]
            |> splitLine
            |> Array.map (fun s -> 
                let x = s.Split('|')
                let b = int x[0]
                let a = int x[1]
                b, a)
        let updates =
            parts[1]
            |> splitLine
            |> Array.map ints
        
        let updateOrderCorrect (update: int[]) =
            let mutable printed = []
            let mutable valid = true
            for u in update do
                for (b,a) in orderRules do
                    if (update |> Array.contains a) && (update |> Array.contains b) then
                        if u = b then
                            if printed |> List.contains a then
                                valid <- false
                        elif u = a then
                            if not (printed |> List.contains b) then
                                valid <- false
                    printed <- u::printed
            valid
        let validUpdates = updates |> Array.filter updateOrderCorrect

        validUpdates 
        |> Array.fold (fun a update -> 
            let midIndex = (update.Length / 2) 
            let mid = update[midIndex]
            printfn "%A %d" update mid
            a + mid) 0
        |> string
        |> output 1

        ()

