namespace AdventOfCode.FSharp.Y2023

// Day 2: Cube Conundrum http://adventofcode.com/2023/day/2
module Day02 =
    open AdventOfCode.FSharp.Util

    let parseItem (item: string) =
        let parts = item.Trim().Split ' '
        let count = int parts[0]
        parts[1], count

    let parsePull (pull: string) =
        let items = pull.Split ','

        items
        |> Array.map parseItem
        |> Array.fold
            (fun (r, g, b) (s, c) ->
                match s with
                | "red" -> (c, g, b)
                | "green" -> (r, c, b)
                | "blue" -> (r, g, c)
                | _ -> failwithf "invalid color: %s" s)
            (0, 0, 0)

    let parseGame (line: string) =
        let pulls = (line.Split ':' |> Array.last).Split ';'
        pulls |> Array.map parsePull

    let run (input: byte[]) (output: int -> string -> unit) =
        let games =
            input
            |> text
            |> splitLine
            |> Array.map parseGame
            |> Array.mapi (fun i g -> (i + 1), g)

        games
        |> Array.filter (snd >> (Array.forall (fun (r, g, b) -> r <= 12 && g <= 13 && b <= 14)))
        |> Array.map fst
        |> Array.sum
        |> string
        |> output 1

        let minCubePower (game: (int * int * int) array) =
            let r, g, b =
                game
                |> Array.fold (fun (rm, gm, bm) (r, g, b) -> (max rm r), (max gm g), (max bm b)) (0, 0, 0)

            r * g * b

        games |> Array.map (snd >> minCubePower) |> Array.sum |> string |> output 2
