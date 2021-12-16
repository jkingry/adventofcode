namespace AdventOfCode.FSharp.Y2021

// Day 15
module Day16 =
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
   
        output 1 (-1 |> string)
        //output 2 (-1 |> string)
