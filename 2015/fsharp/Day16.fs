namespace AdventOfCode.FSharp.Y2015

// Day 16: Aunt Sue

module Day16 =
    open AdventOfCode.FSharp.Util
    open System.Text.RegularExpressions

    type Detection =
        { children: int option
          cats: int option
          samoyeds: int option
          akitas: int option
          pomeranians: int option
          vizslas: int option
          goldfish: int option
          trees: int option
          cars: int option
          perfumes: int option }

    let EmptyDetection =
        { akitas = None
          cars = None
          cats = None
          children = None
          goldfish = None
          perfumes = None
          pomeranians = None
          samoyeds = None
          trees = None
          vizslas = None }

    let linePattern = Regex "Sue (\d+): (?:([a-z]+): (\d+)(?:, )?)+"

    let parseLine (line: string) =
        let m = linePattern.Match line

        if not m.Success then
            failwithf "invalid line: %s" line
        else
            let sue = m.Groups[1].Value |> int
            let kg = m.Groups[2]
            let vg = m.Groups[3]

            let mutable res = EmptyDetection

            for i = 0 to kg.Captures.Count - 1 do
                let v = vg.Captures[i].Value |> int |> Some

                match kg.Captures[i].Value with
                | "akitas" -> res <- { res with akitas = v }
                | "cars" -> res <- { res with cars = v }
                | "cats" -> res <- { res with cats = v }
                | "children" -> res <- { res with children = v }
                | "goldfish" -> res <- { res with goldfish = v }
                | "perfumes" -> res <- { res with perfumes = v }
                | "pomeranians" -> res <- { res with pomeranians = v }
                | "samoyeds" -> res <- { res with samoyeds = v }
                | "trees" -> res <- { res with trees = v }
                | "vizslas" -> res <- { res with vizslas = v }
                | k -> failwithf "invalid key: %s" k

            sue, res

    let matchOrNone e o =
        match o with
        | None -> true
        | Some v -> v = e

    let isExpectedPart1 (d: Detection) =
        d.akitas |> matchOrNone 0
        && d.cars |> matchOrNone 2
        && d.cats |> matchOrNone 7
        && d.children |> matchOrNone 3
        && d.goldfish |> matchOrNone 5
        && d.perfumes |> matchOrNone 1
        && d.pomeranians |> matchOrNone 3
        && d.samoyeds |> matchOrNone 2
        && d.trees |> matchOrNone 3
        && d.vizslas |> matchOrNone 0

    let lessOrNone e o =
        match o with
        | None -> true
        | Some v -> v < e

    let greaterOrNone e o =
        match o with
        | None -> true
        | Some v -> v > e

    let isExpectedPart2 (d: Detection) =
        d.akitas |> matchOrNone 0
        && d.cars |> matchOrNone 2
        && d.cats |> greaterOrNone 7
        && d.children |> matchOrNone 3
        && d.goldfish |> lessOrNone 5
        && d.perfumes |> matchOrNone 1
        && d.pomeranians |> lessOrNone 3
        && d.samoyeds |> matchOrNone 2
        && d.trees |> greaterOrNone 3
        && d.vizslas |> matchOrNone 0

    let run (input: byte array) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> text
            |> splitLine
            |> Seq.map parseLine
            |> Seq.scan
                (fun (part1, part2) (sue, info) ->
                    let part1 =
                        match part1 with
                        | Some _ -> part1
                        | _ when isExpectedPart1 info -> Some sue
                        | _ -> None

                    let part2 =
                        match part2 with
                        | Some _ -> part2
                        | _ when isExpectedPart2 info -> Some sue
                        | _ -> None

                    part1, part2)
                (None, None)
            |> Seq.skipWhile (fun (a, b) -> a.IsNone || b.IsNone)
            |> Seq.head

        part1.Value |> string |> output 1
        part2.Value |> string |> output 2
