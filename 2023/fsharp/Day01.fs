namespace AdventOfCode.FSharp.Y2023

// Day 1: Trebuchet?!
module Day01 =
    open AdventOfCode.FSharp.Util

    let run (input: byte[]) (output: int -> string -> unit) =
        input
        |> bsplit '\n'B
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map (fun line ->
            let numbers =
                line
                |> Array.filter (fun c -> '0'B <= c && c <= '9'B)
                |> Array.map (fun c -> c - '0'B |> int)

            (10 * numbers[0]) + (numbers[numbers.Length - 1]))
        |> Array.sum
        |> string
        |> output 1

        let rev (s: string) =
            s.ToCharArray() |> Array.rev |> System.String

        let numbers =
            [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

        let revNumbers = numbers |> List.map rev

        let refirst =
            System.Text.RegularExpressions.Regex("[1-9]|" + (numbers |> String.concat "|"))

        let relast =
            System.Text.RegularExpressions.Regex("[1-9]|" + (revNumbers |> String.concat "|"))

        let matchToNumber (m: System.Text.RegularExpressions.Match) =
            numbers
            |> List.tryFindIndex (fun s -> s = m.Value)
            |> function
                | Some v -> v + 1
                | None -> m.Value |> int

        let matchToRevNumber (m: System.Text.RegularExpressions.Match) =
            revNumbers
            |> List.tryFindIndex (fun s -> s = m.Value)
            |> function
                | Some v -> v + 1
                | None -> m.Value |> int

        let res =
            input
            |> text
            |> splitLine
            |> Array.sort
            |> Array.map (fun line ->
                let first = line |> refirst.Match |> matchToNumber
                let last = line |> rev |> relast.Match |> matchToRevNumber
                let res = (first * 10) + last
                res)

        res |> Array.sum |> string |> output 2
