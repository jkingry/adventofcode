namespace AdventOfCode.FSharp.Y2023

// Day 1: Trebuchet?! http://adventofcode.com/2023/day/1
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

    let matchToNumber wordMap (m: System.Text.RegularExpressions.Match) =
        wordMap |> Map.tryFind m.Value |> Option.defaultWith (fun () -> m.Value |> int)

    let words =
        [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

    let wordMap =
        words |> Array.indexed |> Array.map (fun (a, b) -> (b, a + 1)) |> Map.ofArray

    let rePart2First =
        System.Text.RegularExpressions.Regex("[1-9]|" + (words |> String.concat "|"))

    let rev (s: string) =
        s.ToCharArray() |> Array.rev |> System.String

    let revWords = words |> Array.map rev

    let revWordMap =
        revWords |> Array.indexed |> Array.map (fun (a, b) -> (b, a + 1)) |> Map.ofArray

    let rePart2Last =
        System.Text.RegularExpressions.Regex("[1-9]|" + (revWords |> String.concat "|"))

    let runRegex (input: byte[]) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> text
            |> splitLine
            |> Array.fold
                (fun (p1, p2) line ->
                    let lineArray = line.ToCharArray()
                    let p1firstChar = lineArray |> Array.find (fun c -> '1' <= c && c <= '9')
                    let p1lastChar = lineArray |> Array.findBack (fun c -> '1' <= c && c <= '9')
                    let p1first = p1firstChar - '0' |> int
                    let p1last = p1lastChar - '0' |> int

                    let lineRev = line |> rev
                    let p2first = line |> rePart2First.Match |> (matchToNumber wordMap)
                    let p2last = lineRev |> rePart2Last.Match |> (matchToNumber revWordMap)

                    p1 + (p1first * 10) + p1last, p2 + (p2first * 10) + p2last)
                (0, 0)

        part1 |> string |> output 1
        part2 |> string |> output 2
