namespace AdventOfCode.FSharp.Y2024

// Day 7
module Day07 =
    open Checked
    open AdventOfCode.FSharp.Util

    let parseLine input =
        let mutable pos = 0
        let mutable values = []
        let (p, res) = parseInt64ToDelim input pos ':'B

        pos <- p + 1
        while pos < input.Length do
            let (p, next) = parseIntToAny input pos
            values <- (int64 next)::values
            pos <- p + 1
        
        res, (values |> List.rev)

    let rec canSolve rem v values =
        if v > rem then false
        else
            match values with
            | x::xs -> 
                (canSolve rem (v + x) xs) || (canSolve rem (v * x) xs)
            | [] -> rem = v

    let concat a b =
        // let mag = b |> double |> log10 |> ceil
        // let factor = (10.0 ** mag) |> int64
        // (factor * a) + b
        sprintf "%i%i" a b |> int64
    
    let rec canSolve2 rem v values =
        if v > rem then false
        else 
            match values with
            | x::xs -> 
                (canSolve2 rem (v + x) xs) 
                || (canSolve2 rem (v * x) xs)
                || (canSolve2 rem (concat v x) xs)
            | [] ->
                rem = v

    let solve2 (rem, values) =
        match values with
        | x::xs -> canSolve2 rem x xs
        | _ -> false

    let solve (rem, values) =
        match values with
        | x::xs -> canSolve rem x xs
        | _ -> false

    let run (input: byte[]) (output: int -> string -> unit) =
        let equations =
            input
            |> bsplit '\n'B
            |> Array.map parseLine

        equations
        |> Array.filter solve
        |> Array.map fst
        |> Array.sum
        |> string
        |> output 1     

        equations
        |> Array.filter solve2
        |> Array.map fst
        |> Array.map bigint
        |> Array.sum
        |> string
        |> output 2     
