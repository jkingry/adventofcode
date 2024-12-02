namespace AdventOfCode.FSharp.Y2024

// Day 1: Historian Hysteria
module Day01 =
    open AdventOfCode.FSharp.Util

    let run (input: byte[]) (output: int -> string -> unit) =
        let aGroup, bGroup =
            input
            |> bsplit '\n'B
            |> Array.fold
                (fun (aGroup, bGroup) line ->
                    let (pos, aEntryId) = parseIntToAny line 0
                    let (_, bEntryId) = parseIntToAny line pos
                    aEntryId :: aGroup, bEntryId :: bGroup)
                ([], [])

        let aGroup = aGroup |> List.sort
        let bGroup = bGroup |> List.sort

        (aGroup, bGroup)
        ||> List.zip
        |> List.map (fun (aEntryId, bEntryId) -> aEntryId - bEntryId |> abs)
        |> List.sum
        |> string
        |> output 1

        aGroup 
        |> List.map (fun aEntryId -> 
            let countInOtherList = bGroup |> List.filter (fun bEntryId -> bEntryId = aEntryId) |> List.length
            aEntryId * countInOtherList)
        |> List.sum
        |> string
        |> output 2
