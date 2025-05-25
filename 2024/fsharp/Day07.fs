namespace AdventOfCode.FSharp.Y2024

// Day 7
module Day07 =
    open AdventOfCode.FSharp.Util

    let parseLine input =
        let mutable pos = 0
        let mutable values = []
        let (p, res) = parseInt64ToDelim input pos ':'B

        pos <- p + 1

        while pos < input.Length do
            let (p, next) = parseIntToAny input pos
            values <- (int64 next) :: values
            pos <- p + 1

        res, (values |> List.rev)

    let rec canSolve ops rem v values =
        if v > rem then
            false
        else
            match values with
            | x :: xs -> ops |> Array.exists (fun op -> canSolve ops rem (op v x) xs)
            | [] -> rem = v

    let concat a b =
        let mag = b |> double |> log10 |> floor |> int
        let factor = pown 10L (mag + 1)
        (factor * a) + b

    let solve ops (rem, values) =
        match values with
        | x :: xs -> canSolve ops rem x xs
        | _ -> false

    let run (input: byte[]) (output: int -> string -> unit) =
        let equations = input |> bsplit '\n'B |> Array.map parseLine

        let part1Operations = [| (+); (*) |]

        equations
        |> Array.filter (solve part1Operations)
        |> Array.map fst
        |> Array.sum
        |> string
        |> output 1

        let part2Operations = [| (+); (*); concat |]

        equations
        |> Array.filter (solve part2Operations)
        |> Array.map fst
        |> Array.sum
        |> string
        |> output 2


    let rec canSolvePart1 rem v values =
        if v > rem then
            false
        else
            match values with
            | x :: xs -> (canSolvePart1 rem (v + x) xs) || (canSolvePart1 rem (v * x) xs)
            | [] -> rem = v

    let rec canSolvePart2 rem v values =
        if v > rem then
            false
        else
            match values with
            | x :: xs ->
                (canSolvePart2 rem (v + x) xs)
                || (canSolvePart2 rem (v * x) xs)
                || (canSolvePart2 rem (concat v x) xs)
            | [] -> rem = v

    let solveStaticOps f (rem, values) =
        match values with
        | x :: xs -> f rem x xs
        | _ -> false

    let runStaticOps (input: byte[]) (output: int -> string -> unit) =
        let equations = input |> bsplit '\n'B |> Array.map parseLine

        equations
        |> Array.filter (solveStaticOps canSolvePart1)
        |> Array.map fst
        |> Array.sum
        |> string
        |> output 1

        equations
        |> Array.filter (solveStaticOps canSolvePart2)
        |> Array.map fst
        |> Array.sum
        |> string
        |> output 2
