namespace AdventOfCode.FSharp.Y2020

open AdventOfCode.FSharp.Util
open System
open System.Text
open System.Text.RegularExpressions
open FParsec
open System.Collections.Generic

// Crab Combat
module Day22 =
    let parse (input : string seq) =

        let parsePlayer (e: IEnumerator<string>) =
            if e.MoveNext() = false then None else

            let p = Int32.Parse(e.Current.Split(' ').[1].Trim (':'))

            let mutable cards = []
            while (e.MoveNext() && String.IsNullOrWhiteSpace(e.Current) = false) do
                cards <- Int32.Parse(e.Current)::cards
            
            Some ((p, cards |> List.rev), e)
        
        Seq.unfold parsePlayer (input.GetEnumerator ()) |> Map.ofSeq

    let round (p1, p2) =
        match p1  with
        | [] -> (Some 2, (p1, p2))
        | p1h::p1d -> 
            match p2 with
            | [] -> (Some 1, (p1, p2))
            | p2h::p2d -> 
                if p1h > p2h then
                    let np1d = p1d @ [ p1h; p2h ]
                    let np2d = p2d
                    (None, (np1d, np2d))
                else
                    let np1d = p1d
                    let np2d = p2d @ [ p2h; p1h ]
                    (None, (np1d, np2d))

    let part1 (input : string seq) =
        let sp = parse input
        
        let mutable r = (sp[1], sp[2])
        let mutable winner = None

        while Option.isNone winner do 
            let z = round r
            winner <- fst z
            r <- snd z

        let d = 
            match winner with
            | Some 1 -> fst r
            | Some 2 -> snd r

        let n = d.Length
        let score = d |> List.mapi (fun i v -> (n - i) * v ) |> List.sum
        printfn "%A" r
        score
