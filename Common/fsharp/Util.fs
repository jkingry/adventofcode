namespace AdventOfCode.FSharp

module Util =
    open System
    open System.Text.RegularExpressions
    open System.Collections.Generic

    exception Unreachable

    module OrthoGrid =
        let movesToSeq (mx, my) (x, y) =
            seq {
                if x > 0 then
                    yield (x - 1, y)

                if y > 0 then
                    yield (x, y - 1)

                if x < (mx - 1) then
                    yield (x + 1, y)

                if y < (my - 1) then
                    yield (x, y + 1)
            }

        let checkBounds a cx cy =
            cx >= 0 && cy >= 0 && (cx < Array2D.length1 a) && (cy < Array2D.length2 a)

        let movesToBuffers costFunc cx cy (mxBuf: int[]) (myBuf: int[]) (mcBuf: 'a[]) =
            let mutable c = 0

            match costFunc cx cy (cx - 1) cy with
            | Some cost ->
                mxBuf[c] <- cx - 1
                myBuf[c] <- cy
                mcBuf[c] <- cost
                c <- c + 1
            | _ -> ()

            match costFunc cx cy cx (cy - 1) with
            | Some cost ->
                mxBuf[c] <- cx
                myBuf[c] <- cy - 1
                mcBuf[c] <- cost
                c <- c + 1
            | _ -> ()

            match costFunc cx cy (cx + 1) cy with
            | Some cost ->
                mxBuf[c] <- cx + 1
                myBuf[c] <- cy
                mcBuf[c] <- cost
                c <- c + 1
            | _ -> ()

            match costFunc cx cy cx (cy + 1) with
            | Some cost ->
                mxBuf[c] <- cx
                myBuf[c] <- cy + 1
                mcBuf[c] <- cost
                c <- c + 1
            | _ -> ()

            c

    module DijkstraMap =
        open FSharpx.Collections

        let empty<'a, 'b when 'a: comparison and 'b: comparison> =
            (Map.empty: Map<'a, 'b>), ((Heap.empty false): Heap<'b * 'a>)

        let emptyMax<'a, 'b when 'a: comparison and 'b: comparison> =
            (Map.empty: Map<'a, 'b>), ((Heap.empty true): Heap<'b * 'a>)


        let add state cost (costs: Map<'a, 'b>, q) =
            let costs' = costs |> Map.add state cost
            let q' = q |> Heap.insert (cost, state)
            costs', q'

        let run infiniteCost movesFunc goalFunc (scores: Map<'a, 'b>, q) =
            let mutable q = q
            let mutable scores = scores

            let mutable found = false

            while not (found || Heap.isEmpty q) do
                let (currentCost, current), nq = Heap.uncons q

                // printfn "%A %A" currentCost current

                if goalFunc current then
                    found <- true
                else
                    q <- nq

                    for (move, moveCost) in movesFunc current do
                        let tentativeScore = scores[current] + moveCost

                        if tentativeScore < (scores |> Map.tryFind move |> Option.defaultValue infiniteCost) then
                            scores <- scores |> Map.add move tentativeScore
                            q <- q |> Heap.insert (tentativeScore, move)

            scores, q

        let runAstar infiniteCost movesFunc goalFunc h (s: Map<'a, 'b>, q) =
            let mutable q = q
            let mutable gScores = s
            let mutable fScores = s

            let mutable found = false

            while not (found || Heap.isEmpty q) do
                let (currentCost, current), nq = Heap.uncons q

                // printfn "%A %A" currentCost current

                if goalFunc current then
                    found <- true
                else
                    q <- nq

                    for (move, moveCost) in movesFunc current do
                        let tentative_gScore = gScores[current] + moveCost

                        let existing_gScore =
                            gScores |> Map.tryFind move |> Option.defaultValue infiniteCost

                        if tentative_gScore < existing_gScore then
                            gScores <- gScores |> Map.add move tentative_gScore

                            let tentative_fScore = tentative_gScore + h (move)

                            fScores <- fScores |> Map.add move tentative_fScore
                            q <- q |> Heap.insert (tentative_fScore, move)

            gScores, q

        let runMax zeroCost movesFunc goalFunc (scores: Map<'a, 'b>, q) =
            let mutable q = q
            let mutable scores = scores

            let mutable found = false

            while not (found || Heap.isEmpty q) do
                let (currentCost, current), nq = Heap.uncons q

                if goalFunc current then
                    found <- true
                else
                    q <- nq

                    for (move, moveCost) in movesFunc current do
                        let tentativeScore = scores[current] + moveCost

                        if tentativeScore > (scores |> Map.tryFind move |> Option.defaultValue zeroCost) then
                            scores <- scores |> Map.add move tentativeScore
                            q <- q |> Heap.insert (tentativeScore, move)

            scores, q

    module Dijkstra2D =
        open FSharpx.Collections

        let init mx my infinityCost =
            let costs = Array2D.create mx my infinityCost
            let q = Heap.empty false
            costs, q

        let add x y (cost: 'a) (costs: 'a[,], q) =
            costs[x, y] <- cost
            let q' = q |> Heap.insert (cost, (x, y))
            costs, q'

        let run maxMoves zeroCost moveFunc goalFunc (costs: 'a[,], q) =
            let mutable q = q

            let mutable found = false

            let mxBuf = Array.create maxMoves 0
            let myBuf = Array.create maxMoves 0
            let mcBuf = Array.create maxMoves zeroCost

            while not (found || Heap.isEmpty q) do
                let (_, (cx, cy)), nq = Heap.uncons q

                if goalFunc cx cy then
                    found <- true
                else
                    q <- nq

                    let moveCount = moveFunc cx cy mxBuf myBuf mcBuf

                    for i = 1 to moveCount do
                        let mx = mxBuf[i - 1]
                        let my = myBuf[i - 1]
                        let moveCost = mcBuf[i - 1]

                        let tentative_score = costs[cx, cy] + moveCost

                        if tentative_score < costs[mx, my] then
                            costs[mx, my] <- tentative_score
                            q <- q |> Heap.insert (tentative_score, (mx, my))

            costs, q

    module Counter =
        let create (input: #seq<'T>) : Map<'T, int64> =
            input
            |> Seq.groupBy id
            |> Seq.map (fun (k, v) -> (k, Seq.length v |> int64))
            |> Map.ofSeq

        let inline add (key: 'T) (value: int64) (counter: Map<'T, int64>) : Map<'T, int64> =
            counter |> Map.change key (fun o -> value + defaultArg o 0L |> Some)

        let inline remove (key: 'T) (value: int64) (counter: Map<'T, int64>) : Map<'T, int64> = add key -value counter

        let inline incr (key: 'T) (counter: Map<'T, int64>) : Map<'T, int64> = add key 1 counter

        let inline decr (key: 'T) (counter: Map<'T, int64>) : Map<'T, int64> = add key -1 counter

    let cacheSequence (cacheName: string) (a: byte[] seq) =
        let year, day = NorthPole.executingYearDay.Value
        let inputPath = NorthPole.Impl.getInputFolder year day
        let cacheFile = System.IO.Path.Combine [| inputPath; sprintf "%s.cache" cacheName |]

        seq {
            let mutable index = 0

            if System.IO.File.Exists cacheFile then
                use f = System.IO.File.OpenRead cacheFile
                use r = new System.IO.BinaryReader(f)

                while r.PeekChar() >= 0 do
                    let len = r.Read7BitEncodedInt()
                    yield r.ReadBytes len
                    index <- index + 1

            use f = System.IO.File.Open(cacheFile, IO.FileMode.Append)
            use w = new System.IO.BinaryWriter(f)

            yield!
                a
                |> Seq.skip index
                |> Seq.map (fun b ->
                    w.Write7BitEncodedInt b.Length
                    w.Write b
                    b)
        }

    let text (data: byte[]) : string = Text.Encoding.ASCII.GetString(data)

    let rec comb n l =
        match n, l with
        | 0, _ -> [ [] ]
        | _, [] -> []
        | k, (x :: xs) -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs

    let ints (s: string) =
        s.Split(' ', ',')
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map int

    let list2tuple2 =
        function
        | [ x; y ] -> (x, y)
        | _ -> failwith "Invalid list item"

    let array2tuple3 =
        function
        | [| x; y; z |] -> (x, y, z)
        | _ -> failwith "Invalid list item"

    let list2tuple3 =
        function
        | [ x; y; z ] -> (x, y, z)
        | _ -> failwith "Invalid list item"

    let split (split: string) (s: string) =
        s.Split([| split |], StringSplitOptions.RemoveEmptyEntries)

    let splitLine (s: string) =
        s.Split([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)

    let splitSpace (s: string) =
        s.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)

    let splitDoubleLine (s: string) =
        s.Split([| "\r\n\r\n"; "\n\n" |], StringSplitOptions.RemoveEmptyEntries)

    let bsplit (splitBy: byte) (b: byte[]) =
        let lenEstimate = b.Length / 81
        let mutable res = []
        let item = System.Collections.Generic.List<byte>(lenEstimate)

        for i = 0 to b.Length - 1 do
            let c = b.[i]

            if c = splitBy then
                res <- item.ToArray() :: res
                item.Clear()
            else
                item.Add(c)

        if item.Count > 0 then
            res <- item.ToArray() :: res

        res |> List.rev |> Array.ofList

    type bits = System.Collections.BitArray

    module Bits =
        let toString (bits: bits) =
            let mutable buf = System.Text.StringBuilder bits.Count

            for i = 1 to bits.Count do
                let p = bits.Count - i
                buf <- buf.Append(if bits[p] then '1' else '0')

            buf.ToString()

        let reverse =
            [| 0x00uy
               0x80uy
               0x40uy
               0xc0uy
               0x20uy
               0xa0uy
               0x60uy
               0xe0uy
               0x10uy
               0x90uy
               0x50uy
               0xd0uy
               0x30uy
               0xb0uy
               0x70uy
               0xf0uy
               0x08uy
               0x88uy
               0x48uy
               0xc8uy
               0x28uy
               0xa8uy
               0x68uy
               0xe8uy
               0x18uy
               0x98uy
               0x58uy
               0xd8uy
               0x38uy
               0xb8uy
               0x78uy
               0xf8uy
               0x04uy
               0x84uy
               0x44uy
               0xc4uy
               0x24uy
               0xa4uy
               0x64uy
               0xe4uy
               0x14uy
               0x94uy
               0x54uy
               0xd4uy
               0x34uy
               0xb4uy
               0x74uy
               0xf4uy
               0x0cuy
               0x8cuy
               0x4cuy
               0xccuy
               0x2cuy
               0xacuy
               0x6cuy
               0xecuy
               0x1cuy
               0x9cuy
               0x5cuy
               0xdcuy
               0x3cuy
               0xbcuy
               0x7cuy
               0xfcuy
               0x02uy
               0x82uy
               0x42uy
               0xc2uy
               0x22uy
               0xa2uy
               0x62uy
               0xe2uy
               0x12uy
               0x92uy
               0x52uy
               0xd2uy
               0x32uy
               0xb2uy
               0x72uy
               0xf2uy
               0x0auy
               0x8auy
               0x4auy
               0xcauy
               0x2auy
               0xaauy
               0x6auy
               0xeauy
               0x1auy
               0x9auy
               0x5auy
               0xdauy
               0x3auy
               0xbauy
               0x7auy
               0xfauy
               0x06uy
               0x86uy
               0x46uy
               0xc6uy
               0x26uy
               0xa6uy
               0x66uy
               0xe6uy
               0x16uy
               0x96uy
               0x56uy
               0xd6uy
               0x36uy
               0xb6uy
               0x76uy
               0xf6uy
               0x0euy
               0x8euy
               0x4euy
               0xceuy
               0x2euy
               0xaeuy
               0x6euy
               0xeeuy
               0x1euy
               0x9euy
               0x5euy
               0xdeuy
               0x3euy
               0xbeuy
               0x7euy
               0xfeuy
               0x01uy
               0x81uy
               0x41uy
               0xc1uy
               0x21uy
               0xa1uy
               0x61uy
               0xe1uy
               0x11uy
               0x91uy
               0x51uy
               0xd1uy
               0x31uy
               0xb1uy
               0x71uy
               0xf1uy
               0x09uy
               0x89uy
               0x49uy
               0xc9uy
               0x29uy
               0xa9uy
               0x69uy
               0xe9uy
               0x19uy
               0x99uy
               0x59uy
               0xd9uy
               0x39uy
               0xb9uy
               0x79uy
               0xf9uy
               0x05uy
               0x85uy
               0x45uy
               0xc5uy
               0x25uy
               0xa5uy
               0x65uy
               0xe5uy
               0x15uy
               0x95uy
               0x55uy
               0xd5uy
               0x35uy
               0xb5uy
               0x75uy
               0xf5uy
               0x0duy
               0x8duy
               0x4duy
               0xcduy
               0x2duy
               0xaduy
               0x6duy
               0xeduy
               0x1duy
               0x9duy
               0x5duy
               0xdduy
               0x3duy
               0xbduy
               0x7duy
               0xfduy
               0x03uy
               0x83uy
               0x43uy
               0xc3uy
               0x23uy
               0xa3uy
               0x63uy
               0xe3uy
               0x13uy
               0x93uy
               0x53uy
               0xd3uy
               0x33uy
               0xb3uy
               0x73uy
               0xf3uy
               0x0buy
               0x8buy
               0x4buy
               0xcbuy
               0x2buy
               0xabuy
               0x6buy
               0xebuy
               0x1buy
               0x9buy
               0x5buy
               0xdbuy
               0x3buy
               0xbbuy
               0x7buy
               0xfbuy
               0x07uy
               0x87uy
               0x47uy
               0xc7uy
               0x27uy
               0xa7uy
               0x67uy
               0xe7uy
               0x17uy
               0x97uy
               0x57uy
               0xd7uy
               0x37uy
               0xb7uy
               0x77uy
               0xf7uy
               0x0fuy
               0x8fuy
               0x4fuy
               0xcfuy
               0x2fuy
               0xafuy
               0x6fuy
               0xefuy
               0x1fuy
               0x9fuy
               0x5fuy
               0xdfuy
               0x3fuy
               0xbfuy
               0x7fuy
               0xffuy |]

        let rev (ba: bits) =
            let bytesCount = (ba.Count |> float) / 8.0 |> ceil |> int
            let bytes: byte array = Array.zeroCreate bytesCount
            ba.CopyTo(bytes, 0)
            let reverseBytes = bytes |> Array.map (fun i -> reverse[i |> int]) |> Array.rev
            let reverseBa = bits reverseBytes
            reverseBa.RightShift(reverseBa.Count - ba.Count) |> ignore
            reverseBa.Length <- ba.Count
            reverseBa

        let fromString (inputText: string) =
            let parsed = int ("0b" + inputText)
            let initial = bits [| parsed |]
            initial.Length <- inputText.Length
            initial

        let popCount (row: bits) =
            let dest: int[] = Array.zeroCreate (((row.Count |> float) / 32.0) |> ceil |> int)
            row.CopyTo(dest, 0)
            dest |> Array.map (uint >> System.Numerics.BitOperations.PopCount) |> Array.sum

    let intersects aStart aEnd bStart bEnd = aStart <= bEnd && aEnd >= bStart

    let rec distribute e =
        function
        | [] -> [ [ e ] ]
        | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

    let rec permute =
        function
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

    let rec fromChoices (input: char list list) : char list list =
        match input with
        | [] -> failwith "Invalid input"
        | [ x ] -> x |> List.map (fun y -> [ y ])
        | x :: xs ->
            x
            |> List.map (fun y ->
                // remove y from all remaining choices
                let noy_xs = xs |> List.map (List.except [ y ])
                (fromChoices noy_xs) |> List.map (fun yy -> [ y ] @ yy))
            |> List.concat


    let mapIncr (key: 'Key) (m: Map<'Key, int>) =
        m |> Map.change key (fun v -> Some(1 + Option.defaultValue 0 v))

    let mapDecr (key: 'Key) (m: Map<'Key, int>) =
        m |> Map.change key (fun v -> Some(-1 + Option.defaultValue 0 v))

    let mapDecrDel (key: 'Key) (m: Map<'Key, int>) =
        m
        |> Map.change key (fun v ->
            let v' = (v |> Option.get) - 1
            if v' = 0 then None else Some v')

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            [ for g in m.Groups -> g.Value ] |> List.tail |> Some
        else
            None

    let takeWhile (e: IEnumerator<string>) =
        seq {
            while e.MoveNext() && not (String.IsNullOrEmpty e.Current) do
                yield e.Current
        }

    let inline parseIntToAny (s: byte[]) (pos: int) =
        let mutable res = 0
        let mutable pos' = pos
        let mutable foundNonDigit = false

        while pos' < (s.Length - 1)
              && s[pos'] <> '-'B
              && not ('0'B <= s[pos'] && s[pos'] <= '9'B) do
            pos' <- pos' + 1

        let sign =
            if s[pos'] = '-'B then
                pos' <- pos' + 1
                -1
            else
                1

        while (not foundNonDigit) && pos' < s.Length do
            let c = s[pos']

            match c with
            | c when '0'B <= c && c <= '9'B ->
                res <- res * 10 + int (c - '0'B)
                pos' <- pos' + 1
            | _ -> foundNonDigit <- true

        (pos', sign * res)

    let parseInts (s: byte[]) =
        let mutable i = 0
        let mutable res = []

        while i < (s.Length - 1) do
            let (ni, v) = parseIntToAny s i
            res <- v :: res
            i <- ni

        res |> List.rev |> Array.ofList

    let inline parseIntToDelim (s: byte[]) (pos: int) (delimChar: byte) =
        let mutable res = 0
        let mutable sign = 1
        let mutable pos' = pos
        let mutable foundDelim = false

        while (not foundDelim) && pos' < (s.Length - 1) do
            let c = s[pos']

            match c with
            | c when c = delimChar -> foundDelim <- true
            | '-'B -> sign <- -1
            | c when '0'B <= c && c <= '9'B -> res <- res * 10 + int (c - '0'B)
            | _ -> failwithf "Bad Format at '%c'" (char c)

            pos' <- pos' + 1

        (pos', sign * res)

    let inline gcd (a: ^a) (b: ^a) =
        let mutable a = a
        let mutable b = b

        while a <> LanguagePrimitives.GenericZero && b <> LanguagePrimitives.GenericZero do
            if a > b then a <- a % b else b <- b % a

        a ||| b
