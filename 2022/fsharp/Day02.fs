namespace AdventOfCode.FSharp.Y2022

// Day 2: Rock Paper Scissors
module Day02 =
    open AdventOfCode.FSharp.Util

    let run input (output: int -> string -> unit) =        
        let values = 
            input 
            |> splitLine
            |> Seq.map(fun s -> 
                let them = (int s[0]) - (int 'A')
                let you = (int s[2]) - (int 'X')
                (them, you))

        values 
            |> Seq.map(fun (them, you) -> 
                let result = 
                    if you = them then 3
                    elif you = (them + 1) % 3 then 6
                    else 0
                1 + you + result)
            |> Seq.sum
            |> string
            |> output 1      

        values 
            |> Seq.map(fun (them, result) -> 
                1 + 
                    match result with
                    | 0 -> 0 + (them + 2) % 3  
                    | 1 -> 3 + them
                    | 2 -> 6 + (them + 1) % 3
                    | x -> failwithf "Unexpected: %i" x)
            |> Seq.sum
            |> string
            |> output 2

