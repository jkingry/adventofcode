namespace AdventOfCode.FSharp.Y2015

// Day 7: Some Assembly Required
module Day07 =
    open AdventOfCode.FSharp.Util

    type LValue =
        | Constant of uint16
        | WireName of string

    type CircuitOp =
        | Set of LValue
        | And of (LValue * LValue)
        | Or of (LValue * LValue)
        | Not of LValue
        | LShift of (string * int)
        | RShift of (string * int)

    let parseLValue =
        function
        | Regex "^(\d+)$" [ a ] -> a |> uint16 |> Constant
        | Regex "^([a-z]+)$" [ a ] -> a |> WireName
        | x -> x |> failwithf "Invalid l-value format: %s"

    let parseOperation =
        function
        | Regex "^([a-z0-9]+)$" [ constantText ] -> constantText |> parseLValue |> Set
        | Regex "^([a-z0-9]+) AND ([a-z0-9]+)$" [ a; b ] -> (a |> parseLValue, b |> parseLValue) |> And
        | Regex "^([a-z0-9]+) OR ([a-z0-9]+)$" [ a; b ] -> (a |> parseLValue, b |> parseLValue) |> Or
        | Regex "^([a-z]+) LSHIFT (\d+)$" [ a; b ] -> (a, b |> int) |> LShift
        | Regex "^([a-z]+) RSHIFT (\d+)$" [ a; b ] -> (a, b |> int) |> RShift
        | Regex "^NOT ([a-z]+)$" [ a ] -> a |> parseLValue |> Not
        | x -> x |> failwithf "Invalid operation format: %s"

    let parseLine =
        function
        | Regex "(.+) -> ([a-z]{1,2})" [ operation; wireName ] -> (operation |> parseOperation), wireName
        | x -> x |> failwithf "Invalid line format: %s"

    type WireLayout =
        { mutable State: Map<string, uint16>
          Layout: Map<string, WireSpec> }

    and WireSpec = WireLayout -> uint16

    let getLValue =
        function
        | Constant x -> fun _ -> x
        | WireName a ->
            fun wires ->
                // Without this "caching" the actual solution runtime gets very, very long
                match wires.State |> Map.tryFind a with
                | Some v -> v
                | None ->
                    let v = wires |> wires.Layout[a]
                    wires.State <- wires.State |> Map.add a v
                    v

    let getOpOutput op =
        match op with
        | Set x -> getLValue x
        | And(a, b) ->
            let al = getLValue a
            let bl = getLValue b

            fun wires ->
                let av = al wires
                let bv = bl wires
                av &&& bv
        | Or(a, b) ->
            let al = getLValue a
            let bl = getLValue b

            fun wires ->
                let av = al wires
                let bv = bl wires
                av ||| bv
        | LShift(a, b) -> fun wires -> wires |> wires.Layout[a] <<< b
        | RShift(a, b) -> fun wires -> wires |> wires.Layout[a] >>> b
        | Not a ->
            let al = getLValue a
            fun wires -> ~~~(al wires)

    let applyLine wires (op, outputWire) =
        wires |> Map.add outputWire (getOpOutput op)

    let run (input: byte array) (output: int -> string -> unit) =
        let gates1 =
            input |> text |> splitLine |> Seq.map parseLine |> Seq.fold applyLine Map.empty

        let layout1 = { State = Map.empty; Layout = gates1 }

        let part1 = layout1 |> gates1["a"]

        part1 |> string |> output 1

        let setInstruction = Constant part1 |> Set

        let gates2 = gates1 |> Map.add "b" (getOpOutput setInstruction)

        let layout2 = { State = Map.empty; Layout = gates2 }

        let part2 = layout2 |> gates2["a"]

        part2 |> string |> output 2
