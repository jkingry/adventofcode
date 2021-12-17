namespace AdventOfCode.FSharp.Y2021

// Day 17: Trick Shot
module Day17 =
    open AdventOfCode.FSharp.Util
    open Checked
    
    let run (input: string) (output: int -> string -> unit) =

        let simulate (xv:int64) (yv:int64) (x1,y1) (x2,y2) =
            let mutable xp = 0L
            let mutable yp = 0L
            let mutable xvv = xv
            let mutable yvv = yv
            let mutable found = false
            let mutable maxy = 0L

        
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
            if found then
                Some maxy
            else
                None
        
        // let foo = simulate 7 2 (20L, -10L) (30L, -5L)
        // printfn "%A" foo

        let v =
            Seq.allPairs [0L..5000L] [-5000L..5000L]
            |> Seq.choose (fun (x,y) -> match simulate x y (277L, -92L) (318L, -53L) with | Some _ -> Some (x,y) | _ -> None)
            //|> Seq.choose (fun (x,y) -> match simulate x y (20L, -10L) (30L, -5L) with | Some _ -> Some (x,y) | _ -> None)
            |> Seq.distinct
            |> Seq.length
        v |> string |> output 1