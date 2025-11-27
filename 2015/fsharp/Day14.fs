namespace AdventOfCode.FSharp.Y2015

// Day 14: Reindeer Olympics
module Day14 =
    open AdventOfCode.FSharp.Util

    let raceDurationSeconds = 2503

    let linePattern =
        "^(.+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds\.$"

    let parseLine (line: string) =
        match line with
        | Regex linePattern [ _; speedText; speedDuration; restDuration ] ->
            int speedText, int speedDuration, int restDuration
        | _ -> failwithf "invalid line: %s" line

    let calculateDistance time speed flyTime restTime =
        let travelBlocksTime = flyTime + restTime
        let travelBlocks = time / travelBlocksTime
        let blockDistance = speed * travelBlocks * flyTime
        let remainderFlyTime = time - travelBlocks * travelBlocksTime |> min flyTime
        let finalDistance = blockDistance + remainderFlyTime * speed

        finalDistance


    let run (input: byte array) (output: int -> string -> unit) =
        let reindeer = input |> text |> splitLine |> Array.map parseLine

        let raceDistance = calculateDistance raceDurationSeconds

        reindeer
        |> Array.map (fun (speed, flyTime, restTime) -> raceDistance speed flyTime restTime)
        |> Array.max
        |> string
        |> output 1

        let distances = Array.zeroCreate reindeer.Length
        let points = Array.zeroCreate reindeer.Length

        for time = 1 to raceDurationSeconds do
            for r = 0 to distances.Length - 1 do
                let speed, flyTime, restTime = reindeer[r]
                let travelBlocksTime = flyTime + restTime
                let travelBlockPos = time % travelBlocksTime

                if 0 < travelBlockPos && travelBlockPos <= flyTime then
                    distances[r] <- distances[r] + speed

            let maxDistance = Array.max distances

            for r = 0 to points.Length - 1 do
                if distances[r] = maxDistance then
                    points[r] <- points[r] + 1

        points |> Array.max |> string |> output 2
