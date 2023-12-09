namespace AdventOfCode.FSharp.Y2015

// Day 4: The Ideal Stocking Stuffer
module Day04 =
    open AdventOfCode.FSharp.Util
    open System.Security.Cryptography
    open System.Text

    let run (input: byte array) (output: int -> string -> unit) =
        let encoding = Encoding.UTF8
        let hasher = MD5.Create()

        let secretCode = input |> text |> splitLine |> Array.head

        let computeHash index =
            let targetText = sprintf "%s%i" secretCode index
            let targetBytes = encoding.GetBytes targetText
            hasher.ComputeHash targetBytes

        let naturals = Seq.unfold (fun state -> Some(state, state + 1)) 1

        let found =
            naturals
            |> Seq.map computeHash
            |> cacheSequence secretCode
            |> Seq.map System.Convert.ToHexString
            |> Seq.indexed
            |> Seq.choose (fun (i, s) ->
                if s.StartsWith("000000") then Some(6, (i + 1))
                elif s.StartsWith("00000") then Some(5, (i + 1))
                else None)
            |> Seq.distinctBy fst
            |> Seq.take 2
            |> Map.ofSeq

        found[5] |> string |> output 1
        found[6] |> string |> output 2
