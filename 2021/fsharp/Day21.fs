namespace AdventOfCode.FSharp.Y2021

// Day 21: Dirac Dice
module Day21 =
    open AdventOfCode.FSharp.Util

    let runDeterministicRound (rolls,players) =
        let ((pos, score), next) = players
        let die = (rolls % 100) + 1
        let dieTotal = (3*die) + 3 

        let newPos = (pos + dieTotal) % 10
        let newScore = score + (newPos+1)
        let newRolls = rolls + 3

        newRolls, (next, (newPos, newScore))

    let calculateDeterministicResult (rolls,((_,p1score),(_,p2score))) =
        if p1score >= 1000 then rolls * p2score |> Some
        elif p2score >= 1000 then rolls * p1score |> Some
        else None 

    


    let rec runDiracGame (cache: Ref<Map<(int*int*int*int),(int64*int64)>>) gameState =
        match cache.Value |> Map.tryFind gameState with
        | Some (currentWins, otherWins) -> currentWins, otherWins
        | None ->
            let (currentPosition, currentScore, otherPosition, otherScore) = gameState

            let mutable currentWins = 0L
            let mutable otherWins = 0L
            for roll1 = 1 to 3 do
                for roll2 = 1 to 3 do
                    for roll3 = 1 to 3 do
                        let nextPosition = (currentPosition + roll1 + roll2 + roll3) % 10
                        let nextScore = currentScore + (nextPosition+1)
                        if nextScore >= 21 then
                            currentWins <- currentWins + 1L
                        else
                            // keep playing
                            let (universeCopyOtherWins, universeCopyCurrentWins) = runDiracGame cache (otherPosition, otherScore, nextPosition, nextScore)
                            currentWins <- currentWins + universeCopyCurrentWins
                            otherWins <- otherWins + universeCopyOtherWins
            cache.Value <- Map.add gameState (currentWins, otherWins) cache.Value 
            currentWins, otherWins

    let run (input: string) (output: int -> string -> unit) =
        let positions = 
            input 
            |> splitLine
            |> Seq.map (splitSpace >> Array.last >> int)
            |> List.ofSeq
        
        let p1 = positions.[0] - 1
        let p2 = positions.[1] - 1

        // part 1

        let mutable gameState = (0, ((p1, 0), (p2,0)))

        let mutable w = None
        while Option.isNone w do
            gameState <- runDeterministicRound gameState
            w <- calculateDeterministicResult gameState

        w.Value |> string |> output 1

        // part 2
        let (w1, w2) = runDiracGame (ref Map.empty) (p1, 0, p2, 0)

        max w1 w2 |> string |> output 2





