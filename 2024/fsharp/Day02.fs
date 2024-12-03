namespace AdventOfCode.FSharp.Y2024

// Day 2: Red-Nosed Reports
module Day02 =
    open AdventOfCode.FSharp.Util

    let findUnsafeLevel (report: int[]) =
        let levelDiffs = 
            report 
            |> Seq.pairwise
            |> Seq.map (fun (a,b) -> a - b)
        let firstDiffSign = levelDiffs |> Seq.head |> sign

        levelDiffs 
        |> Seq.tryFindIndex (fun d -> (sign d) <> firstDiffSign || (abs d) = 0 || (abs d) > 3)    

    let checkSafe (report: int[]) : bool =
        report |> findUnsafeLevel |> Option.isNone
        
    let checkSafeWithSafetyDampener (report: int[]) : bool =
        match (findUnsafeLevel report) with
        | None -> true
        | Some index -> 
            [-1; 0; 1] |> List.exists (fun delta -> 
                (index + delta) >= 0 && (report |> Array.removeAt (index + delta) |> checkSafe))

    let run (input: byte[]) (output: int -> string -> unit) =
        let reports = 
            input
            |> bsplit '\n'B
            |> Array.map parseInts

        reports 
        |> Seq.filter checkSafe
        |> Seq.length
        |> string
        |> output 1

        reports 
        |> Seq.filter checkSafeWithSafetyDampener
        |> Seq.length
        |> string
        |> output 2