namespace AdventOfCode.FSharp.Y2021

// Day 21: Dirac Dice
module Day21 =
    open AdventOfCode.FSharp.Util

    let game (rolls,players) =
        let ((pos, score), next) = players
        let die = (rolls % 100) + 1
        let dieTotal =
            match die with
            | 99 -> 200
            | 100 -> 103
            | _ -> (3*die) + 3

        let newPos = (pos + dieTotal) % 10
        let newScore = score + (newPos+1)
        let newRolls = rolls + 3

        newRolls, (next, (newPos, newScore))

    let winValue (rolls,((_,p1score),(_,p2score))) =
        if p1score >= 1000 then rolls * p2score |> Some
        elif p2score >= 1000 then rolls * p1score |> Some
        else None 
    
    let run (input: string) (output: int -> string -> unit) =
        let mutable g = (0, ((1, 0), (0,0)))

        let mutable w = None
        while Option.isNone w do
            g <- game g
            w <- winValue g
        let (r,_) = g
        w.Value |> string |> output 1 
