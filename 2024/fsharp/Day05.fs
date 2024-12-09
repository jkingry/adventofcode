namespace AdventOfCode.FSharp.Y2024

// Day 5
module Day05 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let parts = input |> text |> splitDoubleLine

        let orderRules =
            parts[0]
            |> splitLine
            |> Array.map (fun s ->
                let x = s.Split('|')
                let b = int x[0]
                let a = int x[1]
                b, a)

        let updates = parts[1] |> splitLine |> Array.map ints

        let updateOrderCorrect (update: int[]) =
            let mutable printed = []
            let mutable valid = true

            for u in update do
                for (b, a) in orderRules do
                    if (update |> Array.contains a) && (update |> Array.contains b) then
                        if u = b then
                            if printed |> List.contains a then
                                valid <- false
                        elif u = a then
                            if not (printed |> List.contains b) then
                                valid <- false

                    printed <- u :: printed

            valid

        let validUpdates, invalidUpdates = updates |> Array.partition updateOrderCorrect

        let midSum (orderings: int[][]) =
            orderings
            |> Array.fold
                (fun a update ->
                    let midIndex = (update.Length / 2)
                    let mid = update[midIndex]
                    a + mid)
                0

        midSum validUpdates |> string |> output 1



        let orderUpdate (update: int[]) =
            let urules =
                orderRules
                |> Array.filter (fun (b, a) -> (update |> Array.contains a) && (update |> Array.contains b))

            let nextpage =
                urules
                |> Array.fold
                    (fun d (b, a) ->
                        d
                        |> Map.change b (function
                            | None -> [ a ] |> Some
                            | Some xs -> a :: xs |> Some))
                    Map.empty

            let mutable remain = update |> Array.toList
            let mutable order = []

            while remain.Length > 0 do
                let next =
                    remain
                    |> List.find (fun p ->
                        remain
                        |> List.forall (fun op ->
                            match nextpage |> Map.tryFind op with
                            | Some x when x |> List.contains p -> false
                            | _ -> true))

                remain <- remain |> List.filter (fun p -> p <> next)
                order <- next :: order

            order |> List.rev |> List.toArray

        invalidUpdates |> Array.map orderUpdate |> midSum |> string |> output 2
