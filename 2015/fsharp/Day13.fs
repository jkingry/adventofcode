namespace AdventOfCode.FSharp.Y2015

// Day 13
module Day13 =
    open AdventOfCode.FSharp.Util
    open FSharpx.Collections

    let linePattern =
        "^(.+) would (lose|gain) ([0-9]+) happiness units by sitting next to (.+)\.$"

    let parseLine (line: string) =
        match line with
        | Regex linePattern [ source; "gain"; amount; target ] -> source, (int amount, target)
        | Regex linePattern [ source; "lose"; amount; target ] -> source, (-1 * int amount, target)
        | _ -> failwithf "Invalid line: %s" line

    let optionAdd amount vopt =
        amount + (vopt |> Option.defaultValue 0) |> Some

    let findHappiness costTable people =
        let firstPerson = people |> List.head

        let mutable q = Heap.empty true |> Heap.insert (0, [ firstPerson ])
        let mutable maxHappiness = 0

        while Heap.isEmpty q |> not do
            let (cost, table), nq = Heap.uncons q
            q <- nq

            for newPerson in people |> Seq.except table do
                let neighbor = List.head table

                let happiness =
                    costTable |> Map.tryFind (newPerson, neighbor) |> Option.defaultValue 0

                let newTable = newPerson :: table
                let newCost = happiness + cost

                // Account for the additional neighbor as
                // this is final person at table
                if newTable.Length = people.Length then
                    let happiness =
                        costTable |> Map.tryFind (newPerson, firstPerson) |> Option.defaultValue 0

                    let finalCost = newCost + happiness

                    if finalCost > maxHappiness then
                        maxHappiness <- finalCost
                else
                    q <- q |> Heap.insert (newCost, newTable)

        maxHappiness

    let run (input: byte array) (output: int -> string -> unit) =
        let rules = input |> text |> splitLine |> Array.map parseLine

        let addRule costs (source, (amount, target)) =
            costs
            |> Map.change (source, target) (optionAdd amount)
            |> Map.change (target, source) (optionAdd amount)

        let costTable = rules |> Array.fold addRule Map.empty

        let people = rules |> Seq.map fst |> Seq.distinct |> List.ofSeq

        findHappiness costTable people |> string |> output 1

        findHappiness costTable ("me" :: people) |> string |> output 2
