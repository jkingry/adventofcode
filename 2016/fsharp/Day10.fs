namespace AdventOfCode.FSharp.Y2016

// Day 10: Balance Bots 
module Day10 =
    open AdventOfCode.FSharp.Util

    type Destination =
        | Bot of bot: int
        | Output of output: int

    type Instr =
        | Input of value: int * bot: int
        | Transfer of source: int * loTarget: Destination * hiTarget: Destination

    type Bot =
        { values: int list
          targets: (Destination * Destination) option }

    let EmptyBot = { values = []; targets = None }

    let parseLine (line: string) =
        let parts = line.Split(' ')

        match parts[0] with
        | "value" ->
            let value = parts[1] |> int
            let bot = parts[5] |> int
            Input(value, bot)
        | "bot" ->
            let source = parts[1] |> int
            let loTargetNumber = (parts[6] |> int)

            let loTarget =
                match parts[5] with
                | "output" -> Output loTargetNumber
                | "bot" -> Bot loTargetNumber
                | x -> failwithf "Invalid low target: %s" x

            let hiTargetNumber = (parts[11] |> int)

            let hiTarget =
                match parts[10] with
                | "output" -> Output hiTargetNumber
                | "bot" -> Bot hiTargetNumber
                | x -> failwithf "Invalid high target: %s" x

            Transfer(source, loTarget, hiTarget)
        | x -> failwithf "Invalid line: %s" x

    let rec add (target: Destination) (value: int) (bots: Map<int, Bot>, outputs: Map<int, int>) =
        match target with
        | Output n ->
            let outputs' = outputs |> Map.add n value
            bots, outputs'
        | Bot bot ->
            let bots' =
                bots
                |> Map.change bot (fun b ->
                    match b |> Option.defaultValue EmptyBot with
                    | { targets = _; values = [] } as x -> Some { x with values = [ value ] }
                    | { targets = _; values = [ a ] } as x -> Some { x with values = [ (min a value); (max a value) ] }
                    | _ -> failwith "Invalid instruction")

            let b = bots' |> Map.find bot
            (bots', outputs) |> tryActivate b

    and tryActivate (bot: Bot) (bots: Map<int, Bot>, outputs: Map<int, int>) =
        match bot with
        | { values = [ lo; hi ]
            targets = Some(loTarget, hiTarget) } -> (bots, outputs) |> add loTarget lo |> add hiTarget hi
        | _ -> bots, outputs

    let applyInstruction (bots: Map<int, Bot>, outputs: Map<int, int>) (instruction: Instr) =
        match instruction with
        | Input(value, bot) -> (bots, outputs) |> add (Bot bot) value
        | Transfer(source, loTarget, hiTarget) ->
            let bots' =
                bots
                |> Map.change source (fun b ->
                    let temp = b |> Option.defaultValue EmptyBot
                    Some { temp with targets = Some(loTarget, hiTarget) })

            let bot = bots' |> Map.find source
            (bots', outputs) |> tryActivate bot

    let run (input: byte array) output =
        let instructions = input |> text |> splitLine |> Array.map parseLine

        let (bots, outputs) =
            instructions |> Array.fold applyInstruction (Map.empty, Map.empty)

        bots
        |> Map.toSeq
        |> Seq.pick (fun (k, v) -> if v.values = [ 17; 61 ] then Some k else None)
        |> string
        |> output 1

        (outputs[0] * outputs[1] * outputs[2]) |> string |> output 2
