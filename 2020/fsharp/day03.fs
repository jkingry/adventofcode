namespace AdventOfCode.FSharp.Y2020

open AdventOfCode.FSharp.Util
open System
open System.Text

module Day03=
    let findTrees down right input =
        let mutable r = 0
        let mutable trees = 0

        input
        |> Seq.skip 1
        |> if down > 1 then
                Seq.mapi (fun i e -> if i % down = (down - 1) then Some e else None)
                >> Seq.choose id
            else
                id 
        |> Seq.iter (fun (line : string) -> 
            r <- r + right
            r <- r % line.Length

            if line.[r] = '#' then trees <- trees + 1 )
        trees

    let part1 input =
        findTrees 1 3 input
        |> bigint

    let part2 input =
        let paths = [
            (1,1)
            (3,1)
            (5,1)
            (7,1)
            (1,2)
        ] 
        
        let x = paths |> List.map (fun (right, down) -> findTrees down right input)
        
        List.iter (printfn "%d") x

        x
        |> List.fold (fun a b -> a * b) 1
        |> bigint