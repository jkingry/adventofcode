namespace AdventOfCode.FSharp.Y2016

// Day 4: Security Through Obscurity
module Day05 =
    open AdventOfCode.FSharp.Util
    open System.Security.Cryptography
    open System.Text

    let runThreaded (input: byte array) output =
        let doorId = input |> text |> splitLine |> Array.head

        let workerCount = System.Environment.ProcessorCount - 1
        let bag = System.Collections.Concurrent.ConcurrentBag()
        use finished = new System.Threading.CancellationTokenSource()
        let finishedToken = finished.Token

        let workers =
            [| 0..workerCount |]
            |> Array.map (fun offset ->
                System.Threading.Tasks.Task.Run(fun () ->
                    let mutable index = offset
                    let hasher = MD5.Create()

                    while not finishedToken.IsCancellationRequested do
                        let targetText = doorId + (string index)
                        let targetBytes = Encoding.Default.GetBytes targetText
                        let hash = hasher.ComputeHash targetBytes |> System.Convert.ToHexString


                        if hash.StartsWith("00000") then
                            bag.Add(index, hash[5], hash[6])

                        index <- index + workerCount))

        let mutable part1 = ""
        let mutable part1need = 8
        let mutable part2 = Array.create 8 ' '
        let mutable part2need = 8

        while part1need > 0 || part2need > 0 do

            let mutable results = []

            while results.Length < (part1need + part2need) do
                let mutable result = (0, ' ', ' ')

                if bag.TryTake &result then
                    results <- result :: results

            for (_, a, b) in results |> List.sortBy (fun (i, _, _) -> i) do
                if part1need > 0 then
                    part1 <- part1 + (a |> string)
                    part1need <- part1need - 1

                if part2need > 0 then
                    if '0' <= a && a < '9' then
                        let pos = a |> string |> int

                        if 0 <= pos && pos < 8 && part2[pos] = ' ' then
                            part2[pos] <- b
                            part2need <- part2need - 1

        finished.Cancel()

        System.Threading.Tasks.Task.WaitAll workers

        (System.String part1).ToLowerInvariant() |> output 1
        (System.String part2).ToLowerInvariant() |> output 2

    let run (input: byte array) output =
        let updatePasswordCharFromHash (part1: char array, part2: char array) (hash: string) =
            if hash.StartsWith("00000") then
                match part1 |> Array.tryFindIndex (fun c -> c = ' ') with
                | Some pos -> part1[pos] <- hash[5]
                | _ -> ()

                if '0' <= hash[5] && hash[5] <= '9' then
                    let pos = (int hash[5]) - (int '0')

                    if 0 <= pos && pos < 8 && part2[pos] = ' ' then
                        part2[pos] <- hash[6]

            part1, part2

        let doorId = input |> text |> splitLine |> Array.head

        let encoding = System.Text.Encoding.UTF8
        let hasher = MD5.Create()

        let computeHash index =
            let targetText = sprintf "%s%i" doorId index
            let targetBytes = encoding.GetBytes targetText
            hasher.ComputeHash targetBytes

        let naturals = Seq.unfold (fun state -> Some(state, state + 1)) 0

        let (part1, part2) =
            naturals
            |> Seq.map computeHash
            |> Seq.map System.Convert.ToHexString
            |> Seq.filter (fun s -> s.StartsWith("00000"))
            |> Seq.map System.Convert.FromHexString
            |> cacheSequence 5 doorId
            |> Seq.map System.Convert.ToHexString
            |> Seq.scan updatePasswordCharFromHash ((Array.create 8 ' '), (Array.create 8 ' '))
            |> Seq.skipWhile (fun (p1, p2) -> (p1 |> Array.contains ' ') || (p2 |> Array.contains ' '))
            |> Seq.head

        (System.String part1).ToLowerInvariant() |> output 1
        (System.String part2).ToLowerInvariant() |> output 2
