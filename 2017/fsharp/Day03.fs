namespace AdventOfCode.FSharp.Y2017

// Day 3: Spiral Memory https://adventofcode.com/2017/day/3
module Day03 =
    open AdventOfCode.FSharp.Util

    let dirs = [| struct (1, 0); struct (0, 1); struct (-1, 0); struct (0, -1) |]

    let getSpiral () : seq<int * int * int> =
        seq {
            let mutable spiral = Map.empty

            let mutable v = 0
            let mutable x = 0
            let mutable y = 0
            let mutable dir = 3

            while true do
                v <- v + 1
                spiral <- spiral |> Map.add struct (x, y) v

                yield (v, x, y)

                let leftDir = (dir + 1) % 4
                let struct (leftDx, leftDy) = dirs[leftDir]

                if spiral |> Map.tryFind struct (x + leftDx, y + leftDy) |> Option.isNone then
                    dir <- (dir + 1) % 4

                let struct (dx, dy) = dirs[dir]
                x <- x + dx
                y <- y + dy
        }

    let getSumSpiral () : seq<int> =
        seq {
            let mutable spiral = Map.empty

            let mutable x = 0
            let mutable y = 0
            let mutable dir = 3

            while true do
                let sumValue =
                    Array.allPairs [| -1; 0; 1 |] [| -1; 0; 1 |]
                    |> Array.filter (fun (a, b) -> a <> 0 || b <> 0)
                    |> Array.choose (fun (dx, dy) -> spiral |> Map.tryFind struct (x + dx, y + dy))
                    |> Array.sum

                let sumValue = if sumValue = 0 then 1 else sumValue

                spiral <- spiral |> Map.add struct (x, y) sumValue

                yield sumValue

                let leftDir = (dir + 1) % 4
                let struct (leftDx, leftDy) = dirs[leftDir]

                if spiral |> Map.tryFind struct (x + leftDx, y + leftDy) |> Option.isNone then
                    dir <- (dir + 1) % 4

                let struct (dx, dy) = dirs[dir]
                x <- x + dx
                y <- y + dy
        }

    let run (input: byte array) (output: int -> string -> unit) =
        let _, value = parseIntToAny input 0

        let _, x, y = getSpiral () |> Seq.find (fun (v, _, _) -> v = value)

        let distance = (abs x) + (abs y)
        distance |> string |> output 1

        let largerValue = getSumSpiral () |> Seq.find (fun v -> v > value)
        largerValue |> string |> output 2

    let alldirs =
        Array.allPairs [| -1; 0; 1 |] [| -1; 0; 1 |]
        |> Array.filter (fun (a, b) -> a <> 0 || b <> 0)

    let getSpiralPos () : seq<struct (int * int)> =
        let mutable x = 0
        let mutable y = 0
        let mutable dir = 0
        let mutable side = 1
        let mutable distance = 0

        seq {
            while true do
                yield struct (x, y)

                if distance = side then
                    distance <- 0
                    dir <- (dir + 1) % 4

                    side <-
                        side
                        + match dir with
                          | 2
                          | 0 -> 1
                          | _ -> 0

                let struct (dx, dy) = dirs[dir]
                x <- x + dx
                y <- y + dy
                distance <- distance + 1
        }

    let computeSpiralSum (_, m) pos =
        let struct (x, y) = pos

        let v =
            alldirs
            |> Array.choose (fun (dx, dy) -> m |> Map.tryFind struct (x + dx, y + dy))
            |> Array.sum

        v, m |> Map.add pos (max v 1)

    let runPos (input: byte array) (output: int -> string -> unit) =
        let _, value = parseIntToAny input 0
        let struct (x, y) = getSpiralPos () |> Seq.skip (value - 1) |> Seq.head
        (abs x) + (abs y) |> string |> output 1

        getSpiralPos ()
        |> Seq.scan computeSpiralSum (0, Map.empty)
        |> Seq.map fst
        |> Seq.skipWhile (fun v -> v < value)
        |> Seq.head
        |> string
        |> output 2

    let getSpiralPositionFast (value: int) =
        let root = value |> float |> sqrt |> floor |> int

        if root * root = value && root % 2 = 1 then
            // lower-right corner
            let x = (root - 1) / 2
            let y = -x
            x, y
        elif (root * root) + 1 = value then
            // upper-left corner
            let y = root / 2
            let x = -y
            x, y
        else
            let root = if root * root = value then root - 1 else root

            if root % 2 = 0 then
                let a = (root * root) + 1
                let b = (root + 1) * (root + 1)
                let mid = a + ((b - a) / 2)

                if value < mid then
                    // left
                    let y = root / 2
                    let x = -y
                    let y = y + (value - a)
                    x, y
                else
                    // bottom
                    let y = -(root / 2)
                    let x = y
                    let x = x + (value - mid)
                    x, y
            else
                let a = root * root
                let b = (root + 1) * (root + 1) + 1
                let mid = a + ((b - a) / 2)

                if value > mid then
                    // top
                    let y = (root + 1) / 2
                    let x = -y
                    let x = x + (b - value)
                    x, y
                else
                    // right
                    let y = (root + 1) / 2
                    let x = y
                    let y = y - (mid - value)
                    x, y

    let runFast (input: byte array) (output: int -> string -> unit) =
        let _, value = parseIntToAny input 0

        let x, y = getSpiralPositionFast value

        (abs x) + (abs y) |> string |> output 1

        getSpiralPos ()
        |> Seq.scan computeSpiralSum (0, Map.empty)
        |> Seq.map fst
        |> Seq.skipWhile (fun v -> v < value)
        |> Seq.head
        |> string
        |> output 2
