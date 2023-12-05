namespace AdventOfCode.FSharp.Y2023

// Day 4: Scratchcards
module Day04 =
    open AdventOfCode.FSharp.Util

    // Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53

    type Game = (int list) * (int list)

    let parse (input: byte[]) =
        seq {
            let mutable winning = List.empty
            let mutable yours = List.empty
            let mutable state = 0
            let mutable v = 0

            for b in input do
                match state, b with
                | 0, ':'B -> state <- 1
                | 3, c
                | 1, c when '0'B <= c && c <= '9'B ->
                    v <- c - '0'B |> int
                    state <- state + 1
                | 1, '|'B -> state <- 3
                | 4, c
                | 2, c when '0'B <= c && c <= '9'B -> v <- (v * 10) + (c - '0'B |> int)
                | 2, _ ->
                    winning <- v :: winning
                    state <- 1
                    v <- 0
                | 4, '\n'B ->
                    yours <- v :: yours
                    state <- 0
                    v <- 0
                    yield winning, yours
                    winning <- List.empty
                    yours <- List.empty
                | 4, c ->
                    yours <- v :: yours
                    state <- 3
                    v <- 0
                | _ -> ()
        }

    let scoreGamePart1 (winning, yours) =
        let w = winning |> Set.ofList

        yours
        |> List.fold (fun score you -> score + if w |> Set.contains you then 1 else 0) 0

    let rec scoreGamePart2 (games: Game[]) (cache: int[]) index =
        if cache[index] >= 0 then
            cache[index]
        else
            let cards = scoreGamePart1 (games[index])

            if cards = 0 then
                cache[index] <- 1
                1
            else
                let totalScore =
                    [ index + 1 .. index + cards ]
                    |> List.fold (fun count copyIndex -> count + scoreGamePart2 games cache copyIndex) 1

                cache[index] <- totalScore
                totalScore

    let run (input: byte[]) (output: int -> string -> unit) =
        let games = input |> parse |> Seq.toArray

        games
        |> Seq.map scoreGamePart1
        |> Seq.map (fun s -> if s > 0 then 1 <<< (s - 1) else 0)
        |> Seq.sum
        |> string
        |> output 1

        let cache = Array.create (games.Length) (-1)

        [ 0 .. games.Length - 1 ]
        |> Seq.map (scoreGamePart2 games cache)
        |> Seq.sum
        |> string
        |> output 2
