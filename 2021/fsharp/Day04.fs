namespace AdventOfCode.FSharp.Y2021

// Day 4: Giant Squid
module Day04 =
    open AdventOfCode.FSharp.Util

    let parse (input: string) =
        match input |> splitDoubleLine |> Array.toList with
        | callsText :: boardsText ->
            let calls = callsText |> ints

            let boards =
                boardsText
                |> List.map (fun lines -> lines |> splitLine |> Array.map ints |> array2D)

            calls, boards
        | _ -> failwith "Invalid"

    let isWin calls board =
        seq {
            for i in 1 .. Array2D.length1 board do
                yield board.[i - 1, *]
                yield board.[*, i - 1]
        }
        |> Seq.exists (Seq.forall (fun v -> calls |> List.contains v))

    let bingo calls boards =
        calls
        |> Seq.scan
            (fun (called, winners) call ->
                let called' = call :: called

                let winners' =
                    (boards |> List.except winners)
                    |> List.choose (fun b -> if isWin called' b then Some b else None)
                    |> List.append winners

                (called', winners'))
            ([], [])
        // filter out all rounds where there was no new winner
        |> Seq.pairwise
        |> Seq.choose
            (fun ((c, w), (c', w')) ->
                if w'.Length > w.Length then
                    Some(c', w')
                else
                    None)

    let unpickedSum called b =
        let mutable sum = 0

        b
        |> Array2D.iter
            (fun v ->
                if not (called |> List.contains v) then
                    sum <- sum + v)

        sum

    let run (input: string) (output: int -> string -> unit) = 

        let (calls, boards) = parse input

        let results = bingo calls boards |> Seq.cache

        let getOutput called winner =
            let unpicked = unpickedSum called winner
            let lastCall = called |> List.head

            lastCall * unpicked |> string    

        // part 1
        let (called, firstWinners) = results |> Seq.head

        let firstWinner = List.head firstWinners
        getOutput called firstWinner |> output 1

        // part 2
        let (called, lastWinners) = results |> Seq.last

        let lastWinner = List.last lastWinners
        getOutput called lastWinner |> output 2
