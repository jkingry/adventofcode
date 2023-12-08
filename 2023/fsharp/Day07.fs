namespace AdventOfCode.FSharp.Y2023

// Day 7: Camel Cards
module Day07 =
    open AdventOfCode.FSharp.Util

    let handTypeValue cardCounts =
        match cardCounts with
        | [ 5 ] -> 6
        | 4 :: _ -> 5
        | [ 3; 2 ] -> 4
        | 3 :: _ -> 3
        | [ 2; 2; 1 ] -> 2
        | 2 :: _ -> 1
        | _ -> 0

    let JackValue = 11

    let jokerHandTypeValue jokerCount cardCounts =
        match cardCounts with
        | [] -> [ 5 ]
        | x :: xs -> (x + jokerCount) :: xs
        |> handTypeValue

    let parseHand pos (input: byte[]) =
        let mutable hand = 0
        let mutable jokerHand = 0
        let mutable cardCounts = Map.empty

        for i = 0 to 4 do
            let cardValue =
                match input[pos + i] with
                | 'A'B -> JackValue + 3
                | 'K'B -> JackValue + 2
                | 'Q'B -> JackValue + 1
                | 'J'B -> JackValue
                | 'T'B -> JackValue - 1
                | n when '2'B <= n && n <= '9'B -> int (n - '0'B)
                | c -> failwithf "Invalid input: %c" (char c)

            cardCounts <- cardCounts |> mapIncr cardValue
            hand <- (hand * 16) + cardValue
            jokerHand <- (jokerHand * 16) + (if cardValue = JackValue then 1 else cardValue)

        let handType =
            cardCounts |> Map.values |> List.ofSeq |> List.sortDescending |> handTypeValue

        let jokerCount = cardCounts |> Map.tryFind JackValue |> Option.defaultValue 0

        let jokerHandType =
            cardCounts
            |> Map.remove JackValue
            |> Map.values
            |> List.ofSeq
            |> List.sortDescending
            |> jokerHandTypeValue jokerCount

        let normalResult = (handType <<< (5 * 4)) + hand
        let jokerResult = (jokerHandType <<< (5 * 4)) + jokerHand

        normalResult, jokerResult

    let parse (input: byte[]) =
        let mutable result = []
        let mutable pos = 0

        while pos < input.Length do
            let hand = parseHand pos input
            let pos', bet = parseIntToAny input (pos + 6)
            pos <- pos' + 1
            result <- (hand, bet) :: result

        result

    let run (input: byte[]) (output: int -> string -> unit) =
        let parsed = input |> parse

        let gamesValue games =
            games
            |> List.sort
            |> List.map snd
            |> List.indexed
            |> List.fold (fun total (index, bet) -> total + (index + 1) * bet) 0

        parsed
        |> List.map (fun (hands, bet) -> (fst hands), bet)
        |> gamesValue
        |> string
        |> output 1

        parsed
        |> List.map (fun (hands, bet) -> (snd hands), bet)
        |> gamesValue
        |> string
        |> output 2
