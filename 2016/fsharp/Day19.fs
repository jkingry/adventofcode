namespace AdventOfCode.FSharp.Y2016

// Day 19: An Elephant Named Joseph
module Day19 =
    open AdventOfCode.FSharp.Util

    let nextElf (circle: bits) elf =
        let len = circle.Count

        [ 1 .. (len - 1) ]
        |> List.tryPick (fun offset ->
            let nextElf = (elf + offset) % len
            if not circle[nextElf] then Some nextElf else None)

    let rec winner (circle: bits) elf =

        let targetElf = nextElf circle elf

        match targetElf with
        | None -> elf
        | Some n ->
            circle[n] <- true
            let next = nextElf circle n

            match next with
            | None -> elf
            | Some o -> winner circle o

    let acrossWinner circleSize =
        let mutable circle = [ 1..circleSize ]
        let mutable next = 1

        while circle.Length > 1 do
            let takerIndex = circle |> List.findIndex (fun i -> i = next)
            let victimIndex = (takerIndex + (circle.Length / 2)) % circle.Length
            let nextIndex = (takerIndex + 1) % circle.Length

            next <-
                if nextIndex = victimIndex then
                    circle[(nextIndex + 1) % circle.Length]
                else
                    circle[nextIndex]

            circle <- circle |> List.removeAt victimIndex

        circle[0]

    let run (input: byte array) (output: int -> string -> unit) =
        let (_, circleSize) = parseIntToAny input 0

        let bitWinner circleSize =
            let iu = circleSize |> uint
            let p = System.Numerics.BitOperations.Log2 iu
            let g = 2u * (iu - (1u <<< p))
            g + 1u

        circleSize |> bitWinner |> string |> output 1


        let bitAcrossWinner s =
            let log3 = (log (s |> float) / (log 3.0)) |> floor
            let pow3 = System.Math.Pow(3.0, log3) |> int
            let pow3_2 = pow3 * 2
            s - pow3 + (if s > pow3_2 then (s - pow3_2) else 0)

        circleSize |> bitAcrossWinner |> string |> output 2
