namespace AdventOfCode.FSharp

module Util = 
    open System
    open System.Text.RegularExpressions
    open System.Collections.Generic

    exception Unreachable

    module OrthoGrid =
        let movesToSeq (mx, my) (x, y) =
            seq {
                if x > 0 then yield (x - 1, y)
                if y > 0 then yield (x, y - 1)
                if x < (mx - 1) then yield (x + 1, y)
                if y < (my - 1) then yield (x, y + 1) 
            }    
        let checkBounds a cx cy =
            cx >= 0 && cy >= 0 && (cx < Array2D.length1 a) && (cy < Array2D.length2 a)

        let movesToBuffers costFunc cx cy (mxBuf: int array) (myBuf: int array) (mcBuf: 'a array) =
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

        let empty<'a,'b when 'a : comparison and 'b : comparison> = (Map.empty : Map<'a, 'b>), ((Heap.empty false) : Heap<'b * 'a>)

        let add state cost (costs: Map<'a,'b>, q) =
            let costs' = costs |> Map.add state cost 
            let q' = q |> Heap.insert (cost, state)            
            costs', q'            
                
        let run infiniteCost movesFunc goalFunc (scores: Map<'a,'b>, q) =
            let mutable q = q
            let mutable scores = scores

            let mutable found = false

            while not (found || Heap.isEmpty q) do
                let (_, current), nq = Heap.uncons q

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
        let create (input : #seq<'T>) : Map<'T, int64> =
            input 
            |> Seq.groupBy id
            |> Seq.map (fun (k,v) -> (k, Seq.length v |> int64))
            |> Map.ofSeq
        
        let inline add (key : 'T) (value : int64)  (counter : Map<'T, int64>) : Map<'T, int64> =
            counter |> Map.change key (fun o -> value + defaultArg o 0L |> Some)

        let inline remove (key : 'T) (value : int64)  (counter : Map<'T, int64>) : Map<'T, int64> =
            add key -value counter

        let inline incr (key : 'T) (counter : Map<'T, int64>) : Map<'T, int64> =
            add key 1 counter

        let inline decr (key : 'T) (counter : Map<'T, int64>) : Map<'T, int64> =
            add key -1 counter

    let text (data: byte array) : string = 
        Text.Encoding.ASCII.GetString(data)

    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

    let ints (s : string) =
        s.Split(' ',',') 
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map int 

    let list2tuple2 = function | [x;y] -> (x,y) | _ -> failwith "Invalid list item"
    let array2tuple3 = function | [|x;y;z|] -> (x,y,z) | _ -> failwith "Invalid list item"
    let list2tuple3 = function | [x;y;z] -> (x,y,z) | _ -> failwith "Invalid list item"

    let split (split : string) (s : string) = s.Split([|split|], StringSplitOptions.RemoveEmptyEntries)

    let splitLine (s : string) = s.Split([|"\r\n";"\n"|], StringSplitOptions.RemoveEmptyEntries)

    let splitSpace (s : string) = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
 
    let splitDoubleLine (s : string) = s.Split([|"\r\n\r\n";"\n\n"|], StringSplitOptions.RemoveEmptyEntries)

    let intersects aStart aEnd bStart bEnd =
        aStart <= bEnd && aEnd >= bStart

    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

    let rec fromChoices (input : char list list) : char list list =
        match input with 
        | [] -> failwith "Invalid input"
        | [x] -> x |> List.map (fun y -> [y])
        | x::xs -> 
            x 
            |> List.map (fun y ->
                // remove y from all remaining choices
                let noy_xs = xs |> List.map (List.except [y]) 
                (fromChoices noy_xs) 
                |> List.map (fun yy -> [y] @ yy))
            |> List.concat


    let mapIncr (key: 'Key) (m : Map<'Key, int>) = m |> Map.change key (fun v -> Some (1 + Option.defaultValue 0 v))
    let mapDecr (key: 'Key) (m : Map<'Key, int>) = m |> Map.change key (fun v -> Some (-1 + Option.defaultValue 0 v))
        
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
        
    let takeWhile (e : IEnumerator<string>) = 
        seq {
            while e.MoveNext() && not (String.IsNullOrEmpty e.Current) do
                yield e.Current
        }
    
    let zero = byte '0' 
    let dash = byte '-'
        
    let inline parseIntToDelim (s: byte array) (pos: int) (delimChar: byte) =
        let mutable res = 0
        let mutable sign = 1
        let mutable pos' = pos
        let mutable foundDelim = false
        while (not foundDelim) && pos' < (s.Length - 1) do
            let c = s[pos']
            if c = delimChar then foundDelim <- true
            elif c = dash then sign <- -1
            else res <- res * 10 + int (c - zero)
            pos' <- pos' + 1  
        (pos', sign * res)