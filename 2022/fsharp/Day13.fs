namespace AdventOfCode.FSharp.Y2022

// Day 13
module Day13 =
    open Checked
    open AdventOfCode.FSharp.Util

    type Packet =
        | PList of Packet list
        | PNum of int

    let rec tostr p =
        match p with
        | PList items -> sprintf "[%s]" (String.concat "," (items |> List.map tostr))
        | PNum n -> sprintf "%i" n

    let rec compare a b =
        match a, b with
        | PNum(aval), PNum(bval) -> if aval = bval then None else Some(aval < bval)
        | PList(alist), PList(blist) ->
            match (alist, blist) with
            | x :: xs, y :: ys ->
                match compare x y with
                | Some v -> Some v
                | None -> compare (xs |> PList) (ys |> PList)
            | [], [] -> None
            | [], _ -> Some true
            | _, [] -> Some false
        | PNum(_), PList(_) -> compare ([ a ] |> PList) b
        | PList(_), PNum(_) -> compare a ([ b ] |> PList)

    let divPack1 = PList([ PList([ PNum(2) ]) ])
    let divPack2 = PList([ PList([ PNum(6) ]) ])

    let runSort pairs (output: int -> string -> unit) =
        let mutable part1 = 0

        for (index, (a, b)) in pairs |> Array.indexed do
            if (compare a b).Value then
                part1 <- part1 + (index + 1)

        part1 |> string |> output 1

        let packets =
            pairs
            |> Array.map (fun (a, b) -> [| a; b |])
            |> Array.concat
            |> Array.append [| divPack1; divPack2 |]

        let packets =
            packets |> Array.sortWith (fun a b -> if (compare a b).Value then -1 else 1)

        let index1 = packets |> Array.findIndex (fun v -> v = divPack1)
        let index2 = packets |> Array.findIndex (fun v -> v = divPack2)

        let part2 = (index1 + 1) * (index2 + 1)
        part2 |> string |> output 2

    let runNoSort pairs (output: int -> string -> unit) =
        let mutable part1 = 0

        let mutable index1 = 1
        let mutable index2 = 2

        for (index, (a, b)) in pairs |> Array.indexed do
            if (compare a b).Value then
                part1 <- part1 + (index + 1)

            if (compare a divPack1).Value then
                index1 <- index1 + 1

            if (compare b divPack1).Value then
                index1 <- index1 + 1

            if (compare a divPack2).Value then
                index2 <- index2 + 1

            if (compare b divPack2).Value then
                index2 <- index2 + 1

        part1 |> string |> output 1
        (index1 * index2) |> string |> output 2

    (****************************************************************
    FParsec
    *****************************************************************)
    open FParsec

    let runParser p str =
        match run p str with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

    let runFParsec (input: byte[]) (output: int -> string -> unit) =
        let fvalue, fvalueRef = createParserForwardedToRef<Packet, unit> ()
        let pnumber = pint32 |>> PNum
        let plist = pstring "[" >>. sepBy fvalue (pstring ",") .>> pstring "]" |>> PList
        fvalueRef.Value <- choice [ pnumber; plist ]

        let pairs =
            input
            |> text
            |> splitDoubleLine
            |> Array.map (fun text ->
                let parts = text |> splitLine |> Array.map (runParser fvalue)
                parts[0], parts[1])

        runSort pairs output

    (****************************************************************
    JSON
    *****************************************************************)
    open System.Text.Json

    let runJson (input: byte[]) (output: int -> string -> unit) =
        let pairs = System.Collections.Generic.List<Packet * Packet>()

        let rec fromJson (e: JsonElement) =
            match e.ValueKind with
            | JsonValueKind.Array -> e.EnumerateArray() |> Seq.map fromJson |> List.ofSeq |> PList
            | JsonValueKind.Number -> e.GetInt32() |> PNum
            | _ -> failwithf "Unexpected element: %A" e

        let mutable s = 0
        let mutable i = 0
        let mutable firstPair = None

        while i < input.Length do
            let c = input[i]

            if c = '\n'B then
                if (i - s) = 0 then
                    ()
                else
                    let data = System.Buffers.ReadOnlySequence<byte>(input, s, i - s)
                    let packet = JsonDocument.Parse(data).RootElement |> fromJson

                    match firstPair with
                    | None -> firstPair <- packet |> Some
                    | Some p ->
                        pairs.Add(p, packet)
                        firstPair <- None

                s <- i + 1

            i <- i + 1

        let pairs = pairs.ToArray()

        runSort pairs output

    (****************************************************************
     Parse by hand
    *****************************************************************)

    // [1,[2,[3,[4,[5,6,7]]]],8,9]
    let rec parseList (input: byte[]) (offset: int) =
        let mutable stack = [ PList([]) ]
        let mutable i = offset
        let mutable p = '@'B

        while p <> '\n'B && i < input.Length do
            let c = input[i]

            stack <-
                match c, stack, p with
                | '['B, _, _ -> ([] |> PList) :: stack
                | ']'B, _, '['B -> stack
                | ']'B, x :: PList(y) :: ys, _ -> PList((x :: y) |> List.rev) :: ys
                | ','B, x :: PList(y) :: ys, _ -> PList(x :: y) :: ys
                | c, PNum(n) :: ys, _ when '0'B <= c && c <= '9'B -> PNum(((c - '0'B) |> int) + (10 * n)) :: ys
                | c, _, _ when '0'B <= c && c <= '9'B -> PNum(c - '0'B |> int) :: stack
                | '\n'B, _, _ -> stack // Terminated!
                | _ -> failwithf "Failed at '%c' : %s" (char c) (String.concat "\n  " (stack |> List.map tostr))

            p <- c
            i <- i + 1

        i, stack[0]

    let parse (input: byte[]) =
        let mutable i = 0
        let mutable pairs = []

        while i < input.Length do
            let (i1, packet1) = parseList input i
            let (i2, packet2) = parseList input i1
            i <- i2 + 1
            pairs <- (packet1, packet2) :: pairs

        pairs |> List.rev |> List.toArray

    let runHandParse (input: byte[]) (output: int -> string -> unit) =

        runSort (parse input) output

    let runHandParseNoSort (input: byte[]) (output: int -> string -> unit) =

        runNoSort (parse input) output
