namespace AdventOfCode.FSharp.Y2016

// Day 21: Scrambled Letters and Hash
module Day21 =
    open AdventOfCode.FSharp.Util

    type Instr =
        | Move of (int * int)
        | SwapPos of (int * int)
        | Reverse of (int * int)
        | RotateR of int
        | RotateL of int
        | Swap of (char * char)
        | RotateBasedR of char
        | RotateBasedL of char


    let parseInput (input: byte array) =
        input
        |> text
        |> splitLine
        |> Seq.map (function
            | Regex @"move position ([0-9]) to position ([0-9])" [ fromPos; toPos ] ->
                Move(fromPos |> int, toPos |> int)
            | Regex @"rotate right ([0-9]) steps?" [ steps ] -> RotateR(steps |> int)
            | Regex @"rotate left ([0-9]) steps?" [ steps ] -> RotateL(steps |> int)
            | Regex @"swap letter (.) with letter (.)" [ a; b ] -> Swap(a[0], b[0])
            | Regex @"reverse positions ([0-9]) through ([0-9])" [ fromPos; toPos ] ->
                Reverse(fromPos |> int, toPos |> int)
            | Regex @"swap position ([0-9]) with position ([0-9])" [ fromPos; toPos ] ->
                SwapPos(fromPos |> int, toPos |> int)
            | Regex @"rotate based on position of letter (.)" [ letter ] -> RotateBasedR letter[0]
            | line -> failwithf "Invalid line: %s" line)

    let mutable inverseIndexMap = Map.empty

    let getInverseRotateBasedR len index =
        if not (Map.containsKey len inverseIndexMap) then
            let newIndexLookup =
                [ 0 .. (len - 1) ]
                |> List.map (fun origIndex ->
                    let newIndex = (1 + 2 * origIndex + (if origIndex >= 4 then 1 else 0)) % len
                    let rotate = (len + newIndex - origIndex) % len
                    newIndex, rotate)
                |> Map.ofList

            inverseIndexMap <- inverseIndexMap |> Map.add len newIndexLookup

        inverseIndexMap |> Map.find len |> Map.find index

    let rec applyInstruction (word: char array) instr =
        match instr with
        | Move(a, b) ->
            let letter = word[a]
            word |> Array.removeAt a |> Array.insertAt b letter
        | RotateR a -> word |> Array.permute (fun p -> (p + a) % word.Length)
        | RotateL a -> word |> Array.permute (fun p -> (word.Length + p - a) % word.Length)
        | Swap(a, b) ->
            word
            |> Array.map (function
                | c when c = a -> b
                | c when c = b -> a
                | c -> c)
        | Reverse(a, b) -> word |> Array.permute (fun p -> if a <= p && p <= b then a + (b - p) else p)
        | SwapPos(a, b) ->
            let temp = word[a]
            word[a] <- word[b]
            word[b] <- temp
            word
        | RotateBasedR a ->
            let index = word |> Array.findIndex (fun v -> v = a)
            let rotate = 1 + index + (if index >= 4 then 1 else 0)
            applyInstruction word (RotateR rotate)
        | RotateBasedL a ->
            let index = word |> Array.findIndex (fun v -> v = a)
            let len = word.Length
            let rotate = getInverseRotateBasedR len index
            applyInstruction word (RotateL rotate)

    let log (word: char array) (instr: Instr) =
        let res = applyInstruction word instr
        printfn "%s + %A => %s" (System.String word) instr (System.String res)
        res

    let revInstr instr =
        match instr with
        | Move(a, b) -> Move(b, a)
        | RotateR a -> RotateL a
        | RotateL a -> RotateR a
        | RotateBasedR a -> RotateBasedL a
        | _ -> instr

    let run (input: byte array) (output: int -> string -> unit) =
        let part1 = "abcdefgh".ToCharArray()

        let instrs = input |> parseInput |> Seq.toArray

        instrs |> Seq.fold applyInstruction part1 |> System.String |> output 1

        let part2 = "fbgdceah".ToCharArray()

        part2
        |> Seq.foldBack (fun i a -> applyInstruction a i) (instrs |> Seq.map revInstr)
        |> System.String
        |> output 2
