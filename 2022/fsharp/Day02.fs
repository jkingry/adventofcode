namespace AdventOfCode.FSharp.Y2022

// Day 2: ????
module Day02 =
    open AdventOfCode.FSharp.Util

    let run input (output: int -> string -> unit) =
        let commands = 
            input 
            |> splitLine
            |> Seq.map(fun s -> 
                let you = (int s[2]) - (int 'X')
                let them = (int s[0]) - (int 'A')
                let score = 
                    if you = 0 && them = 2 then 1 + 6 + you
                    elif you = 1 && them = 0 then 1 + 6 + you
                    elif you = 2 && them = 1 then 1 + 6 + you
                    elif you = them then 1 + 3 + you
                    else 1 + you
                printfn "%i %i = %i" them you score
                
                score)
            |> Seq.sum
        
              
        commands |> string |> output 1 

        let commands2 = 
            input 
            |> splitLine
            |> Seq.map(fun s -> 
                let obj = (int s[2]) - (int 'X')
                let them = (int s[0]) - (int 'A')

                let winner = [| 2; 3; 1 |]
                let loser = [| 3; 1; 2 |]
                let tie = [| 1; 2; 3 |]

                let result = [| 0;3;6|]

                let score = 
                    result[obj] +
                    match obj with
                        | 0 -> loser[them]
                        | 1 -> tie[them]
                        | 2 -> winner[them]
                        | _ -> failwith "wtf"
                
                score)
            |> Seq.sum

        commands2 |> string |> output 2
