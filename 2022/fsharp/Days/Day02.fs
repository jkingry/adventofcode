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
