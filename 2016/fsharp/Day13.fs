namespace AdventOfCode.FSharp.Y2016

// Day 13: A Maze of Twisty Little Cubicles
module Day13 =
    open AdventOfCode.FSharp.Util

    let isWall (fav: uint) (x: uint) (y: uint) =
        let n = (x * x + 3u * x + 2u * x * y + y + y * y) + fav
        let bits = System.Numerics.BitOperations.PopCount n
        bits % 2 = 1

    let print fav maxX maxY =
        for y = 0 to maxY do
            for x = 0 to maxX do
                if isWall fav (x |> uint) (y |> uint) then
                    printf "#"
                else
                    printf "."

            printf "\n"

    let run (input: byte array) (output: int -> string -> unit) =
        let _, favInt = parseIntToAny input 0

        let fav = favInt |> uint

        let cost fx fy tx ty =
            if tx >= 0 && ty >= 0 && (isWall fav (tx |> uint) (ty |> uint) |> not) then
                Some 1
            else
                None

        let moves = OrthoGrid.movesToBuffers cost
        let gx, gy = 31, 39

        let goal x y = x = gx && y = gy

        let (costs, _) =
            Dijkstra2D.init 50 50 System.Int32.MaxValue
            |> Dijkstra2D.add 1 1 0
            |> Dijkstra2D.run 4 0 moves goal

        costs[gx, gy] |> string |> output 1
        let mutable count = 0

        costs
        |> Array2D.iter (fun c ->
            if c <= 50 then
                count <- count + 1)

        count |> string |> output 2
