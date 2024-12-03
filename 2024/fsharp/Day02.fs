namespace AdventOfCode.FSharp.Y2024

// Day 2: Red-Nosed Reports
module Day02 =
    open AdventOfCode.FSharp.Util

    let checkSafe (report: byte[]) =
        let levelDiffs = 
            report 
            |> parseInts
            |> Seq.pairwise
            |> Seq.map (fun (a,b) -> a - b)
        let firstDiffSign = levelDiffs |> Seq.head |> sign

        levelDiffs 
        |> Seq.forall (fun d -> (sign d) = firstDiffSign && 1 <= (abs d) && (abs d) <= 3)

    let run (input: byte[]) (output: int -> string -> unit) =
        input
        |> bsplit '\n'B
        |> Seq.filter checkSafe
        |> Seq.length
        |> string
        |> output 1
