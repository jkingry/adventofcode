namespace AdventOfCode.FSharp.Y2021

// Day 17: Trick Shot
module Day17 =
    open AdventOfCode.FSharp.Util

    let run data (output: int -> string -> unit) =
        let input = data |> text

        let (x1, x2, y1, y2) =
            match input with
            | Regex "target area: x=(\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)" [ a; b; c; d ] ->
                (int64 a, int64 b, int64 c, int64 d)
            | _ -> failwith "Bad input"

        let simulate (xv: int64) (yv: int64) =
            let mutable xvv = xv
            let mutable yvv = yv
            let mutable found = false
            let mutable maxy = 0L

            let mutable xp = 0L
            let mutable yp = 0L

            while xp < x2 && yp > y1 && not found do
                xp <- xp + xvv
                yp <- yp + yvv

                if yp > maxy then
                    maxy <- yp

                if x1 <= xp && xp <= x2 && y1 <= yp && yp <= y2 then
                    found <- true
                else
                    if xvv > 0L then
                        xvv <- xvv - 1L

                    yvv <- yvv - 1L

            if found then Some maxy else None

        let results =
            Seq.allPairs [ 1L .. x2 ] [ y1 .. 500L ]
            |> Seq.choose (fun (xv, yv) ->
                match simulate xv yv with
                | Some maxy -> Some(xv, yv, maxy)
                | _ -> None)
            |> Seq.toList

        results |> List.map (fun (_, _, maxy) -> maxy) |> List.max |> string |> output 1

        results
        |> List.map (fun (xv, yv, _) -> (xv, yv))
        |> List.distinct
        |> List.length
        |> string
        |> output 2
