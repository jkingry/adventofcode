namespace AdventOfCode.FSharp.Y2022

// Day 2: Rock Paper Scissors
module Day02 =
    open AdventOfCode.FSharp.Util

    let run input (output: int -> string -> unit) =
        let losesVs = [| 2; 0; 1 |]
        
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
                    elif you = losesVs[them] then 0
                    else 6
                1 + you + result)
            |> Seq.sum
            |> string
            |> output 1      

        values 
            |> Seq.map(fun (them, result) -> 
                1 + 
                    match result with
                    | 0 -> losesVs[them] 
                    | 1 -> 3 + them
                    | 2 -> 
                        let winsVs = 3 - (them + losesVs[them])
                        6 + winsVs
                    | x -> failwithf "Unexpected: %i" x)
            |> Seq.sum
            |> string
            |> output 2       

