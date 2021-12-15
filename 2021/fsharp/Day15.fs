namespace AdventOfCode.FSharp.Y2021

// Day 15
module Day15 =
    open AdventOfCode.FSharp.Util
    open FSharpx.Collections

    let neighbors (x, y) (mx, my) =
        let mutable n = []
        if x > 0 then n <- (x - 1, y) :: n
        if y > 0 then n <- (x, y - 1) :: n

        if x < (mx - 1) then
            n <- (x + 1, y) :: n

        if y < (my - 1) then
            n <- (x, y + 1) :: n
        n

    let dijkstra (sx, sy) (gx, gy) (mx, my) (w: int -> int -> int) =
        let costs =
            Array2D.create mx my System.Int32.MaxValue

        let visited = Array2D.create mx my false

        let mutable q =
            Heap.empty false |> Heap.insert (0, sx, sy)

        let mutable found = None

        while Option.isNone found && not (Heap.isEmpty q) do
            let ((cost, x, y), nq) = Heap.uncons q

            if x = gx && y = gy then found <- Some cost else
                q <- nq

            if visited.[x, y] then () else

            visited.[x, y] <- true

            for (nx, ny) in (neighbors (x, y) (mx, my)) do
                if visited.[nx, ny] then () else

                let newCost = cost + w nx ny

                if newCost < costs.[nx, ny] then
                    costs.[nx, ny] <- newCost
                    q <- q |> Heap.insert (newCost, nx, ny)

        match found with
        | Some cost -> cost
        | _ -> failwith "INFINITY"

    let run (text: string) : string * string =
        let grid =
            text
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        let mx = grid |> Array2D.length1
        let my = grid |> Array2D.length2

        let w x y =
            let v = grid.[x % mx, y % my]
            let r = (x / mx) + (y / my)
            let vv = (v + r) - 1
            (vv % 9) + 1

        let part1 =
            dijkstra (0, 0) (mx - 1, my - 1) (mx, my) w

        let part2 =
            dijkstra (0, 0) ((mx * 5) - 1, (my * 5) - 1) (mx * 5, my * 5) w

        (part1 |> string, part2 |> string)
