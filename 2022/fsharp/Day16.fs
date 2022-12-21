namespace AdventOfCode.FSharp.Y2022

// Day 16
module Day16 =
    open Checked
    open AdventOfCode.FSharp.Util


    let LineRegex =
        @"^Valve (..) has flow rate=(\d+); tunnel(?:(?: leads to valve (..))|(?:s lead to valves (.+)))$"

    let parse (input: byte[]) =
        input
        |> text
        |> splitLine
        |> Array.map (function
            | Regex LineRegex [ src; rate; dest; dests ] ->
                src,
                (int rate),
                if dest.Length > 0 then
                    [| dest |]
                else
                    (dests.Split ',' |> Array.map (fun s -> s.Trim()))
            | line -> failwithf "Failed parsing: %s" line)

    let run (input: byte[]) (output: int -> string -> unit) =
        let valves = parse input

        printfn "%A" valves

        valves |> Array.length |> string |> output 1
