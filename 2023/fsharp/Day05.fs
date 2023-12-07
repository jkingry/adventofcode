namespace AdventOfCode.FSharp.Y2023

// Day 5: If You Give A Seed A Fertilizer
module Day05 =
    open AdventOfCode.FSharp.Util

    let parse (input: byte[]) =
        let sections = input |> text |> splitDoubleLine

        let seeds = sections[ 0 ].Split(' ') |> Array.tail |> Array.map int64

        let maps =
            sections
            |> Array.tail
            |> Array.map (fun s ->
                s
                |> splitLine
                |> Array.tail
                |> Array.map (fun l ->
                    match l.Split(' ') with
                    | [| a; b; c |] -> (int64 a), (int64 b), (int64 c)
                    | x -> failwithf "Invalid line: %A" x))

        seeds, maps

    let getNextSection almanac value =
        almanac
        |> Array.tryPick (function
            | (a, b, c) when b <= value && value <= (b + c) - 1L -> a + (value - b) |> Some
            | _ -> None)
        |> Option.defaultValue value

    let rec followSection almanac level value =
        if level >= (Array.length almanac) then
            value
        else

            let value' = getNextSection almanac[level] value
            followSection almanac (level + 1) value'

    let rec followSections almanac level ranges =
        if level >= (Array.length almanac) then
            ranges
        else

            let section = almanac[level]
            let mutable result = []

            // printfn "=== %d" level

            for (inputSrc, inputRange) in ranges do
                let intersecting =
                    section
                    |> Array.filter (fun (_, src, rng) ->
                        intersects inputSrc (inputSrc + inputRange - 1L) src (src + rng - 1L))
                    |> Array.sortBy (fun (_, src, _) -> src)

                let mutable remSrc = inputSrc
                let mutable remRng = inputRange

                for (des, src, rng) in intersecting do
                    // before any rules
                    if remSrc < src then
                        let d = remSrc
                        let r = src - remSrc
                        result <- (d, r) :: result
                        remRng <- remRng - r
                        remSrc <- remSrc + r

                    let translate = des - src
                    let d = remSrc + translate

                    let offset = remSrc - src
                    let r = min remRng (rng - offset)
                    result <- (d, r) :: result
                    remRng <- remRng - r
                    remSrc <- remSrc + r

                // after any rules
                if remRng > 0 then
                    result <- (remSrc, remRng) :: result

            result |> List.toArray |> followSections almanac (level + 1)

    let run (input: byte[]) (output: int -> string -> unit) =
        let seeds, sections = input |> parse

        seeds |> Array.map (followSection sections 0) |> Array.min |> string |> output 1

        let seedRanges =
            [| 0..2 .. seeds.Length - 1 |] |> Array.map (fun i -> seeds[i], seeds[i + 1])

        seedRanges
        |> followSections sections 0
        |> Array.map fst
        |> Array.min
        |> string
        |> output 2
