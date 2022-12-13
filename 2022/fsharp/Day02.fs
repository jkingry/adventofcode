namespace AdventOfCode.FSharp.Y2022

// Day 2: Rock Paper Scissors
module Day02 =
    open AdventOfCode.FSharp.Util

    let run input (output: int -> string -> unit) =        
        let values = 
            input 
            |> text
            |> splitLine
            |> Seq.map(fun s -> 
                let them = (int s[0]) - (int 'A')
                let you = (int s[2]) - (int 'X')
                (them, you))

        let score (them, you) =
            let result = 
                if you = them then 3
                elif you = (them + 1) % 3 then 6
                else 0
            1 + you + result           

        values 
            |> Seq.map score
            |> Seq.sum
            |> string
            |> output 1      

        let strategyOutput = 
            values 
                |> Seq.map(fun (them, result) ->  
                        let you = 
                            match result with
                            | 0 -> (them + 2) % 3  
                            | 1 -> them
                            | 2 -> (them + 1) % 3 
                            | x -> failwithf "Unexpected: %i" x
                        (them, you))
        
        let part2 = strategyOutput |> Seq.map score |> Seq.sum 

        part2 |> string |> output 2     

        // did we win?
        let elfScore = strategyOutput |> Seq.map (fun (a,b) -> (b,a)) |> Seq.map score |> Seq.sum
        let part3 = part2 - elfScore
        part3 |> string |> output 3

    let runFast input output =
        let s = new System.ReadOnlySpan<byte>(input)

        let mutable part1 = 0
        let mutable part2 = 0

        let mutable lastLinePosition = 0

        for i in 0 .. s.Length - 1 do
            if (i - lastLinePosition) = 2 then
                lastLinePosition <- lastLinePosition + 4
                let them = int (s[i-2] - 'A'B)
                let you = int (s[i] - 'X'B)
                part1 <- part1 + 
                    if you = them then 1 + 3 + you
                    elif you = (them + 1) % 3 then 1 + 6 + you
                    else 1 + 0 + you
                part2 <- part2 + 
                    if you = 0 then 1 + 0 + ((them + 2) % 3)
                    elif you = 1 then 1 + 3 + them
                    else 1 + 6 + ((them + 1) % 3) 
        part1 |> string |> output 1
        part2 |> string |> output 2