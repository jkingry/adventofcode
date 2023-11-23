namespace AdventOfCode.FSharp.Y2016

// Day 17
module Day17 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =

        let goal =
            function
            | (3, 3), _ -> true
            | _ -> false

        let hasher = System.Security.Cryptography.MD5.Create()
        let encoding = System.Text.Encoding.UTF8
        
        let moves passcode state =
            if goal state then Seq.empty else

            let (x,y), path = state

            let target: string = passcode + path
            let targetBytes = encoding.GetBytes target
            let hash = targetBytes |> hasher.ComputeHash

            seq {
                if y > 0 && hash[0] >= 0xB0uy then
                    yield ((x, y - 1), path + "U"), 1

                if y < 3 && (hash[0] % 16uy) >= 0x0Buy then
                    yield ((x, y + 1), path + "D"), 1

                if x > 0 && hash[1] >= 0xB0uy then
                    yield ((x - 1, y), path + "L"), 1

                if x < 3 && (hash[1] % 16uy) >= 0x0Buy then
                    yield ((x + 1, y), path + "R"), 1
            }

        let infinite = System.Int32.MaxValue
        let passcode = input |> text |> splitLine |> Array.head

        DijkstraMap.empty
        |> DijkstraMap.add ((0, 0), "") 0
        |> DijkstraMap.run infinite (moves passcode) goal
        |> fst
        |> Map.findKey (fun k _ -> goal k)
        |> snd
        |> output 1

        let impossibleGoal _ = false

        DijkstraMap.emptyMax
        |> DijkstraMap.add ((0, 0), "") 0
        |> DijkstraMap.runMax 0 (moves passcode) impossibleGoal
        |> fst
        |> Map.toSeq
        |> Seq.filter (fst >> goal)
        |> Seq.map snd
        |> Seq.max
        |> string
        |> output 2
