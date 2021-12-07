namespace AdventOfCode.FSharp.Y2021

open System
open AdventOfCode.FSharp.Util

// Day 4: Giant Squid
module Day04 =    
    let parse (input : string) =
        match input |> dblLineSplit |> Array.toList with
        | callsText::boardsText ->        
            let calls = callsText |> ints
            let boards = boardsText |> List.map (fun lines -> lines |> lineSplit |> Array.map ints |> array2D)
            calls, boards
        | _ -> failwith "Invalid"

    let isWin calls board =
        seq { for i in 1..Array2D.length1 board do 
                yield board[i-1,*]
                yield board[*,i-1] 
            }  
        |> Seq.exists (Seq.forall (fun v -> calls |> List.contains v))

    let bingo calls boards =
        calls 
        |> Seq.scan (fun (called, winners) call -> 
            let called' = call::called
            let winners' =
                (boards |> List.except winners) 
                |> List.choose (fun b -> if isWin called' b then Some b else None)
                |> List.append winners
            (called', winners')) ([], [])
        // filter out all rounds where there was no new winner
        |> Seq.pairwise 
        |> Seq.choose (fun ((c,w), (c',w')) -> if w'.Length > w.Length then Some (c', w') else None)

    let unpickedSum called b =
            let mutable sum = 0 
            b |> Array2D.iter (fun v -> if not (called |> List.contains v) then sum <- sum + v)
            sum
            
    let part1 (input : string) =  
        let (calls, boards) = parse input
        
        let (called, firstWinners) =
            bingo calls boards
            |> Seq.head

        let firstWinner = List.head firstWinners
        let unpicked = unpickedSum called firstWinner
        let lastCall = called |> List.head

        lastCall * unpicked

    let part2 (input : string) : int =     
        let (calls, boards) = parse input
        
        let (called, lastWinners) =
            bingo calls boards
            |> Seq.last

        let lastWinner = List.last lastWinners
        let unpicked = unpickedSum called lastWinner
        let lastCall = called |> List.head

        lastCall * unpicked