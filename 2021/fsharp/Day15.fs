namespace AdventOfCode.FSharp.Y2021

// Day 15
module Day15 =
    open AdventOfCode.FSharp.Util
    open FSharpx.Collections

    let neighbors (x, y) width height =
        let mutable n = []
        if x > 0 then n <- (x - 1, y) :: n
        if y > 0 then n <- (x, y - 1) :: n

        if x < (width - 1) then
            n <- (x + 1, y) :: n

        if y < (height - 1) then
            n <- (x, y + 1) :: n
        n

    let dijkstra start goal width height (weight: int -> int -> int) =
        let costs =
            Array2D.create width height System.Int32.MaxValue

        let visited = Array2D.create width height false

        let mutable q =
            Heap.empty false |> Heap.insert (0, start)

        let mutable found = None

        while Option.isNone found && not (Heap.isEmpty q) do
            let ((cost, current), nq) = Heap.uncons q

            if current = goal then found <- Some cost else
            
            q <- nq
            let (x,y) = current

            if visited.[x, y] then () else

            visited.[x, y] <- true

            for (nx, ny) in (neighbors current width height) do
                if visited.[nx, ny] then () else

                let newCost = cost + weight nx ny

                if newCost < costs.[nx, ny] then
                    costs.[nx, ny] <- newCost
                    q <- q |> Heap.insert (newCost, (nx, ny))

        match found with
        | Some cost -> cost
        | _ -> failwith "INFINITY"

    let run (text: string) output =
        let grid =
            text
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        let width = grid |> Array2D.length1
        let height = grid |> Array2D.length2

        let risk x y =
            let baseRisk = grid.[x % width, y % height]
            let tileFactor = (x / width) + (y / height)
            let risk = (baseRisk + tileFactor) - 1
            (risk % 9) + 1

        let part1 =
            dijkstra (0, 0) (width - 1, height - 1) width height risk
        output 1 (part1 |> string)

        let width = width * 5
        let height = height * 5

        let part2 =
            dijkstra (0, 0) (width - 1, height - 1) width height risk
        output 2 (part2 |> string)
