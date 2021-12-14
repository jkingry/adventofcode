namespace AdventOfCode.FSharp.Y2021

// Day 14: Extended Polymerization
module Day14 =
    open AdventOfCode.FSharp.Util
    open Checked
    let parse text =
        let sections = text |> splitDoubleLine
        let parseRule (line: string) =
            let p = line.Split("->")
            let ab = p[0].Trim() 
            let c = p[1].Trim()[0]
            let a = ab[0]
            let b = ab[1]
            ab, (c, sprintf "%c%c" a c, sprintf "%c%c" c b) 

        let rules = 
            sections.[1]
            |> splitLine
            |> Array.map parseRule
            |> Map.ofArray

        (rules, sections[0])

    let pairCounts text =
        text 
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> sprintf "%c%c" a b)
        |> Counter.create

    let react rules (polymer, atoms) =
        polymer |> Map.fold (fun (poly',atoms') ab count ->
            match rules |> Map.tryFind ab with
            | Some (c,ac,cb) -> 
                poly' |> Counter.remove ab count |> Counter.add ac count |> Counter.add cb count,
                atoms' |> Counter.add c count
            | _ -> poly',atoms') (polymer,atoms)

    let run reactor polymerString n =
        let mutable polymer = pairCounts polymerString
        let mutable atoms = polymerString |> Counter.create
        for i=1 to n do
            let (npolymer,natoms) = (polymer, atoms) |> reactor
            polymer <- npolymer
            atoms <- natoms
        let weights = atoms |> Map.toList |> List.map snd |> List.sort
        
        let least = weights |> List.head
        let most = weights |> List.last        
        most - least           

    let part1 (text: string) =
        let (rules, polymerString) = parse text
        let reactor = react rules
        run reactor polymerString 10 |> string

    let part2 (text: string) =
        let (rules, polymerString) = parse text
        let reactor = react rules
        run reactor polymerString 40 |> string