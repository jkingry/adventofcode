namespace AdventOfCode.FSharp.Y2017

// Day 4: High-Entropy Passphrases https://adventofcode.com/2017/day/4
module Day04 =
    open AdventOfCode.FSharp.Util

    let containsDuplicate (containsFun: 'a -> Set<'a> -> bool) (input: seq<'a>) : bool =
        let e = input.GetEnumerator()
        let mutable duplicateFound = false
        let mutable parts = Set.empty

        while (not duplicateFound) && e.MoveNext() do
            if parts |> containsFun e.Current then
                duplicateFound <- true
            else
                parts <- parts |> Set.add e.Current

        not duplicateFound

    let validatePassphrase (input: byte array) =
        input |> text |> split " " |> containsDuplicate Set.contains

    let isAnagram a b =
        if (Array.length a) <> (Array.length b) then
            false
        else
            let countValues =
                Array.fold
                    (fun m x ->
                        m
                        |> Map.change x (function
                            | Some v -> v + 1 |> Some
                            | _ -> Some 1))
                    Map.empty

            let ac = countValues a
            let bc = countValues b

            ac = bc

    let validatePassphrase2 (input: byte array) =
        input |> bsplit ' 'B |> containsDuplicate (fun a -> Set.exists (isAnagram a))

    let run (input: byte array) (output: int -> string -> unit) =
        let rows = input |> bsplit '\n'B

        rows |> Seq.filter validatePassphrase |> Seq.length |> string |> output 1

        rows |> Seq.filter validatePassphrase2 |> Seq.length |> string |> output 2
