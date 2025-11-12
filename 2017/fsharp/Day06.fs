namespace AdventOfCode.FSharp.Y2017

// Day 6: Memory Reallocation
module Day06 =
    open AdventOfCode.FSharp.Util

    let redistribute (banks: int array) : int array =
        let maxIndex, maxBlocks = banks |> Seq.indexed |> Seq.maxBy snd
        let newBlocksPerBank = maxBlocks / banks.Length
        let remainder = maxBlocks - newBlocksPerBank * banks.Length

        let banks' = banks |> Array.copy
        banks'[maxIndex] <- 0

        banks'
        |> Array.mapi (fun index blocks ->
            let distance = index - maxIndex

            let distance = if distance <= 0 then banks.Length + distance else distance

            let extraBlock = if distance > remainder then 0 else 1

            blocks + newBlocksPerBank + extraBlock)


    let run (input: byte array) (output: int -> string -> unit) =
        let banks = input |> parseInts

        let mutable bankConfigsSeen = Map.empty

        let mutable count = 0

        let mutable currentBanks = banks

        while bankConfigsSeen |> Map.containsKey currentBanks |> not do
            count <- count + 1
            bankConfigsSeen <- bankConfigsSeen |> Map.add currentBanks count
            currentBanks <- redistribute currentBanks

        count |> string |> output 1

        let originalCount = bankConfigsSeen |> Map.find currentBanks

        1 + (count - originalCount) |> string |> output 2
