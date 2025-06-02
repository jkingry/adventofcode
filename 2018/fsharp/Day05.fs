namespace AdventOfCode.FSharp.Y2018

// Day 5: Alchemical Reduction
module Day05 =
    open System
    open AdventOfCode.FSharp.Util

    let runReaction polymer =
        let mutable polymer' = []
        let mutable p = None

        for c in polymer do
            match p with
            | Some s ->
                if
                    Char.ToUpperInvariant c <> Char.ToUpperInvariant s
                    || Char.IsUpper c = Char.IsUpper s
                then
                    polymer' <- s :: polymer'
                    p <- Some c
                else
                    p <- None
            | _ -> p <- Some c

        match p with
        | Some s -> polymer' <- s :: polymer'
        | _ -> ()

        polymer' |> List.rev

    let fullyReact polymer =
        let mutable p = polymer
        let mutable length = p |> List.length

        let mutable p' = p
        let mutable length' = -1

        let ss x = new System.String(x |> List.toArray)

        while length <> length' do
            // printfn "%A -> %A" (ss p) (ss p')
            // printfn "%i -> %i" length length'

            length <- length'
            p <- p'

            p' <- p |> runReaction
            length' <- p' |> List.length

        p'


    let run (input: byte array) output =
        let polymer = input |> text |> splitLine |> Array.head |> Seq.toList

        let finalPolymer = polymer |> fullyReact

        finalPolymer |> List.length |> string |> output 1

        // find most occurent letter
        finalPolymer
        |> List.map Char.ToUpperInvariant
        |> List.distinct
        |> List.map (fun c ->
            let newLength =
                finalPolymer
                |> List.filter (fun p -> Char.ToUpperInvariant p <> c)
                |> fullyReact
                |> List.length

            newLength)
        |> List.min
        |> string
        |> output 2

    let runReactionBytes (polymer: byte list) =
        let mutable polymer' = []
        let mutable p = None

        for c in polymer do
            match p with
            | Some s ->
                if s - c <> 32uy && c - s <> 32uy then
                    polymer' <- s :: polymer'
                    p <- Some c
                else
                    p <- None
            | _ -> p <- Some c

        match p with
        | Some s -> polymer' <- s :: polymer'
        | _ -> ()

        polymer'

    let fullyReactBytes polymer =
        let mutable p = polymer
        let mutable length = p |> List.length

        let mutable p' = p
        let mutable length' = -1

        while length <> length' do
            length <- length'
            p <- p'

            p' <- p |> runReactionBytes
            length' <- p' |> List.length

        p'


    let runBytes (input: byte array) output =
        let finalPolymer = input |> Array.toList |> fullyReactBytes

        finalPolymer |> List.length |> string |> output 1

        finalPolymer
        |> Seq.map (fun c -> if c >= 'a'B then c - 32uy else c)
        |> Seq.distinct
        |> Seq.map (fun c ->
            let newLength =
                finalPolymer
                |> List.filter (fun p -> p <> c && p <> c + 32uy)
                |> fullyReactBytes
                |> List.length

            newLength)
        |> Seq.min
        |> string
        |> output 2
