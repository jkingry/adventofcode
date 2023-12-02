namespace AdventOfCode.FSharp.Y2016

// Day 14: One-Time Pad
module Day14 =
    open AdventOfCode.FSharp.Util

    let findThreeAndFive (hash: string) =
        let mutable lc = ' '
        let mutable cc = 0
        let mutable threeRes = None
        let mutable fiveRes = []

        for c in hash do
            if c = lc then
                cc <- cc + 1
            else
                if cc >= 3 && threeRes.IsNone then
                    threeRes <- Some lc

                if cc >= 5 then
                    fiveRes <- lc :: fiveRes

                cc <- 1
                lc <- c

        if cc >= 3 && threeRes.IsNone then
            threeRes <- Some lc

        if cc >= 5 then
            fiveRes <- lc :: fiveRes

        threeRes, fiveRes

    let findKeys (hashes: byte[] seq) =
        seq {
            let mutable waiting = Map.empty
            let mutable found = Set.empty

            for index, hash in hashes |> Seq.indexed do
                let hashText = hash |> System.Convert.ToHexString
                let threeRes, fiveRes = findThreeAndFive hashText

                found <-
                    fiveRes
                    |> List.choose (fun c -> waiting |> Map.tryFind c)
                    |> List.concat
                    |> List.fold (fun f i -> f |> Set.add i) found

                let possibleIndex = index - 1000

                if found |> Set.contains possibleIndex then
                    yield possibleIndex
                    found <- found |> Set.remove possibleIndex

                waiting <- fiveRes |> List.fold (fun f c -> f |> Map.remove c) waiting

                waiting <- waiting |> Map.map (fun c x -> x |> List.filter (fun y -> (index - y) < 1000))

                waiting <-
                    match threeRes with
                    | Some c ->
                        waiting
                        |> Map.change c (function
                            | Some x -> (index :: x) |> Some
                            | None -> ([ index ]) |> Some)
                    | None -> waiting
        }

    let encoding = System.Text.Encoding.UTF8
    let hasher = System.Security.Cryptography.MD5.Create()

    let part1 prefix index =
        let targetText = sprintf "%s%i" prefix index
        let targetBytes = encoding.GetBytes targetText
        hasher.ComputeHash targetBytes

    let part2 prefix n index =
        let mutable h = part1 prefix index

        for _ = 0 to (n - 1) do
            let targetText = (h |> System.Convert.ToHexString).ToLower()
            let target = targetText |> encoding.GetBytes
            h <- hasher.ComputeHash target

        h

    let run (input: byte array) (output: int -> string -> unit) =
        let salt = input |> text |> splitLine |> Array.head

        let naturals = Seq.unfold (fun state -> Some(state, state + 1)) 0

        let part1cache = salt

        naturals
        |> Seq.map (part1 salt)
        |> cacheSequence part1cache
        |> findKeys
        |> Seq.take 64
        |> Seq.last
        |> string
        |> output 1

        let part2cache = sprintf "%s.2016" salt

        naturals
        |> Seq.map (part2 salt 2016)
        |> cacheSequence part2cache
        |> findKeys
        |> Seq.take 64
        |> Seq.last
        |> string
        |> output 2
