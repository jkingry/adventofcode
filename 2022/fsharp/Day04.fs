namespace AdventOfCode.FSharp.Y2022

// Day 4: Camp Cleanup
module Day04 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: byte array) (output: int -> string -> unit) =    
        let (part1, part2) = 
            input
            |> text 
            |> splitLine
            |> Array.fold(fun (p1, p2) line ->                 
                let v = line.Split([|',';'-'|]) |> Array.map int
                let (a1, a2, b1, b2) = v[0], v[1], v[2], v[3]

                // // sets solution
                // let a = [a1..a2] |> Set.ofList
                // let b = [b1..b2] |> Set.ofList
                // let p1' = p1 + if Set.isSubset a b || Set.isSubset b a then 1 else 0
                // let p2' = p2 + if (Set.intersect a b).Count > 0 then 1 else 0

                let contained = (a1 <= b1 && b2 <= a2) || (b1 <= a1 && a2 <= b2)
                let p1' = p1 + if contained then 1 else 0

                let intersects = a1 <= b2 && a2 >= b1
                let p2' = p2 + if intersects then 1 else 0

                p1', p2') (0, 0)
        
        part1 |> string |> output 1
        part2 |> string |> output 2
