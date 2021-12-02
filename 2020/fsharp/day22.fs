namespace AdventOfCode.FSharp.Y2020

open AdventOfCode.FSharp.Util
open System
open System.Text
open System.Text.RegularExpressions
open FParsec
open System.Collections.Generic
open AdventOfCode.FSharp.Util

// Crab Combat
module Day22 =
    let parse (input : string seq) =
        let parsePlayer (e: IEnumerator<string>) =
            if e.MoveNext() = false then None else

            let p = Int32.Parse(e.Current.Split(' ').[1].Trim(':'))
            let cards = e |> takeWhile |> Seq.map Int32.Parse |> Seq.toList
            
            Some ((p, cards), e)

        let e = input.GetEnumerator ()
        Seq.unfold parsePlayer e |> Map.ofSeq


    let round (mp : Map<int, int list>) =
        let nmp = mp |> Map.map (fun _ v -> v.Tail)

        let winner = if mp[1].Head > mp[2].Head then 1 else 2
        let loser = if winner = 1 then 2 else 1
        let winnings = [ mp[winner].Head ; mp[loser].Head ]

        let fmp = nmp |> Map.add winner (nmp[winner] @ winnings)

        let res = if fmp[loser].Length = 0 then Some winner else None
        (res, fmp)


    let part1 (input : string seq) =
        let mutable mp = parse input
        
        let mutable winner = None

        while Option.isNone winner do 
            let results = round mp
            winner <- fst results
            mp <- snd results

        let d = mp[winner.Value]

        let n = d.Length
        d |> List.mapi (fun i v -> (n - i) * v ) |> List.sum
