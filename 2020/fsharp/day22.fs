namespace AdventOfCode.FSharp.Y2020

open AdventOfCode.FSharp.Util
open System
open System.Collections.Generic

// Crab Combat
module Day22 =
    type Decks = Map<int, int list>
    
    let parse (input : string seq) : Decks =
        let parsePlayer (e: IEnumerator<string>) =
            if e.MoveNext() = false then None else

            let p = Int32.Parse(e.Current.Split(' ').[1].Trim(':'))
            let cards = e |> takeWhile |> Seq.map Int32.Parse |> Seq.toList
            
            Some ((p, cards), e)

        let e = input.GetEnumerator ()
        Seq.unfold parsePlayer e |> Map.ofSeq

    let combatGame (round : Decks -> int) (mp : Decks) : int * int list =
        let mutable state = mp
        
        let mutable gameWinner = None

        let mutable pastStates = Set.empty
        
        let generateState (state : Decks) : Decks =
            let winner = round state
            let loser = if winner = 1 then 2 else 1

            let tails = state |> Map.map (fun _ v -> v.Tail)
            let heads = state |> Map.map (fun _ v -> v.Head)
            let winnings = [ heads[winner] ; heads[loser] ]
            
            tails |> Map.add winner (tails[winner] @ winnings)

        while Option.isNone gameWinner do 
            if pastStates |> Set.contains state then gameWinner <- Some 1 else
            pastStates <- Set.add state pastStates

            let winner = round state
            let loser = if winner = 1 then 2 else 1

            let tails = state |> Map.map (fun _ v -> v.Tail)
            let heads = state |> Map.map (fun _ v -> v.Head)

            let winnings = [ heads[winner] ; heads[loser] ]
            state <- tails |> Map.add winner (tails[winner] @ winnings)

            if state[loser].Length = 0 then gameWinner <- Some winner

        (gameWinner.Value, state[gameWinner.Value])
        
    let combatRound (mp : Decks) =
        if mp[1].Head > mp[2].Head then 1 else 2
        
    let part1 (input : string seq) =
        let mp = parse input
        
        let (_, winnerDeck) = mp |> combatGame combatRound

        let n = winnerDeck.Length
        winnerDeck |> List.mapi (fun i v -> (n - i) * v ) |> List.sum

    let rec recCombatRound (mp : Decks) =
        let tails = mp |> Map.map (fun _ v -> v.Tail)
        let heads = mp |> Map.map (fun _ v -> v.Head)
        
        if Seq.forall2 (fun h t -> h <= List.length t) heads.Values tails.Values then
            let recDeck = tails |> Map.map (fun p v -> v |> List.take heads[p])
            recDeck |> combatGame recCombatRound |> fst
        else
            if heads[1] > heads[2] then 1 else 2
    
    let part2 (input : string seq) =
        let mp = parse input
        
        let (_, winnerDeck) = mp |> combatGame recCombatRound

        let n = winnerDeck.Length
        winnerDeck |> List.mapi (fun i v -> (n - i) * v ) |> List.sum

