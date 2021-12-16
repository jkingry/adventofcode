namespace AdventOfCode.FSharp.Y2021

// Day 14: Extended Polymerization
module Day14 =
    open AdventOfCode.FSharp.Util

    let parse text =
        let sections = text |> splitDoubleLine

        let parseRule (line: string) =
            let p = line.Split("->")
            let ab = p.[0].Trim()
            let c = p.[1].Trim().[0]
            let a = ab.[0]
            let b = ab.[1]
            ab, (c, sprintf "%c%c" a c, sprintf "%c%c" c b)

        let rules =
            sections.[1]
            |> splitLine
            |> Array.map parseRule
            |> Map.ofArray

        (rules, sections.[0])

    let pairCounts text =
        text
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> sprintf "%c%c" a b)
        |> Counter.create

    let inline react rules (polymer, atoms) =
        polymer
        |> Map.fold
            (fun (poly', atoms') ab count ->
                match rules |> Map.tryFind ab with
                | Some (c, ac, cb) ->
                    poly'
                    |> Counter.remove ab count
                    |> Counter.add ac count
                    |> Counter.add cb count,
                    atoms' |> Counter.add c count
                | _ -> poly', atoms')
            (polymer, atoms)

    let atomicValue atoms =
        let (most, least) =
            atoms 
            |> Map.values
            |> Seq.fold (fun (mv, lv) v -> (max v mv), (min v lv)) (System.Int64.MinValue, System.Int64.MaxValue)
        most - least

    let run (text: string) output =
        let (rules, polymerString) = parse text
        let reactor = react rules

        let mutable polymer = pairCounts polymerString
        let mutable atoms = polymerString |> Counter.create

        for _ = 1 to 10 do
            let (npolymer, natoms) = (polymer, atoms) |> reactor
            polymer <- npolymer
            atoms <- natoms            
        atomicValue atoms |> string |> output 1     
            
        for _ = 11 to 40 do
            let (npolymer, natoms) = (polymer, atoms) |> reactor
            polymer <- npolymer
            atoms <- natoms            
        atomicValue atoms |> string |> output 2            
