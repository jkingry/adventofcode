namespace AdventOfCode.FSharp.Y2022

// Day 12: Hill Climbing Algorithm
module Day12 =
    open Checked
    open FSharpx.Collections
    open AdventOfCode.FSharp.Util

    let run (input: byte[]) (output: int -> string -> unit) =
        let lines = input |> text |> splitLine
        let a = lines |> Array.map (fun s -> s.ToCharArray()) |> array2D

        let mutable start = (0, 0)
        let mutable goal = (0, 0)

        let a =
            a
            |> Array2D.mapi (fun x y v ->
                match v with
                | 'S' ->
                    start <- (x, y)
                    0uy
                | 'E' ->
                    goal <- (x, y)
                    25uy
                | _ -> (byte v) - 'a'B)

        let mx = Array2D.length1 a
        let my = Array2D.length2 a

        let canMoveDown (tx, ty) (fx, fy) =
            let fh = a[fx, fy]
            let th = a[tx, ty]
            (fh + 1uy) >= th

        let moves (x, y) =
            (x, y)
            |> OrthoGrid.movesToSeq (mx, my)
            |> Seq.filter (canMoveDown (x, y))
            |> Seq.map (fun n -> (n, 1))

        let mutable firstA = None

        let stopSearching (nx, ny) =
            if firstA.IsNone && a[nx, ny] = 0uy then
                firstA <- Some(nx, ny)

            (nx, ny) = start

        let (costs, _) =
            DijkstraMap.empty
            |> DijkstraMap.add goal 0
            |> DijkstraMap.run System.Int32.MaxValue moves stopSearching

        costs[start] |> string |> output 1
        costs[firstA.Value] |> string |> output 2

    let runFast (input: byte[]) (output: int -> string -> unit) =
        let lines = input |> bsplit '\n'B
        let a = lines |> array2D

        let mutable (sx, sy) = (0, 0)
        let mutable (gx, gy) = (0, 0)

        let a =
            a
            |> Array2D.mapi (fun x y v ->
                match v with
                | 'S'B ->
                    sx <- x
                    sy <- y
                    0uy
                | 'E'B ->
                    gx <- x
                    gy <- y
                    25uy
                | _ -> (byte v) - 'a'B)

        let mx = Array2D.length1 a
        let my = Array2D.length2 a

        let canMoveDown tx ty fx fy =
            let fh = a[fx, fy]
            let th = a[tx, ty]
            (fh + 1uy) >= th

        let cost fx fy tx ty =
            if OrthoGrid.checkBounds a tx ty && canMoveDown fx fy tx ty then
                Some 1
            else
                None

        let moves = OrthoGrid.movesToBuffers cost

        let mutable firstA = None

        let findStart x y =
            if firstA.IsNone && a[x, y] = 0uy then
                firstA <- Some(x, y)

            (sx, sy) = (x, y)

        let (costs, _) =
            Dijkstra2D.init mx my System.Int32.MaxValue
            |> Dijkstra2D.add gx gy 0
            |> Dijkstra2D.run 4 0 moves findStart

        costs[sx, sy] |> string |> output 1

        let fx, fy = firstA.Value
        costs[fx, fy] |> string |> output 2
