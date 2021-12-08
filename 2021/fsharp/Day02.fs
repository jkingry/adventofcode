namespace AdventOfCode.FSharp.Y2021

// Day 2: Dive! 
module Day02 =
    type Pos = 
        {
            depth : int
            h : int
            aim : int
        }
        
    let executeInput exec (input : string seq) =
        let { depth = depth; h = h } = 
            input
            |> Seq.map (fun line -> let p = line.Split(' ') in (p[0], int p[1]))
            |> Seq.fold exec { depth = 0; h = 0; aim = 0 }
        depth * h    

    let part1 (input : string seq) =
        let executeCommand p (direction, x) =
            match direction with 
            | "down" -> { p with depth = p.depth + x }
            | "up" -> { p with depth = p.depth - x }
            | "forward" -> { p with h = p.h + x }
            | s -> failwith (sprintf "Unexpected command '%s'" s)

        input |> executeInput executeCommand
 
    let part2 input = 
        let executeCommand p (direction, x) =
            match direction with 
            | "down" -> { p with aim = p.aim + x }
            | "up" -> { p with aim = p.aim - x }
            | "forward" -> { p with h = p.h + x ; depth = p.depth + (p.aim * x) }
            | s -> failwith (sprintf "Unexpected command '%s'" s)

        input |> executeInput executeCommand