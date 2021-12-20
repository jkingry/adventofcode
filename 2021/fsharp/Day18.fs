namespace AdventOfCode.FSharp.Y2021

// Day 18
module Day18 =
    open AdventOfCode.FSharp.Util
    open FParsec

    type SnailFish =
        | FNumber of int32
        | FPair of SnailFish*SnailFish


    let runParser p str =
        match run p str with
        | Success (result, _, _) -> result
        | Failure (errorMsg, _, _) ->
            failwith (sprintf "Failure: %s" errorMsg)

    let rec addRight n x =
        match x with 
        | FNumber xn -> 0,FNumber (xn + n)
        | FPair (a, b) -> 
            let (nb,b') = addRight n b
            let (na,a') = addRight nb a
            na, FPair (a',b')

    let rec addLeft n x =
        match x with 
        | FNumber xn -> 0,FNumber (xn + n)
        | FPair (a, b) -> 
            let (na,a') = addLeft n a
            let (nb,b') = addLeft na b
            nb, FPair (a',b')

    let rec toString f =
        match f with
        | FNumber a -> sprintf "%d" a
        | FPair (a,b) -> sprintf "[%s,%s]" (toString a) (toString b)

    let reduce f =
        let rec innerExplode depth f =
            match f with
            | FPair (FNumber a, FNumber b) when depth = 4 ->
                Some (a, FNumber 0, b)
            | FPair (a, b) -> 
                match innerExplode (depth + 1) a with
                | Some (left, a, right) -> 
                    let right, b = addLeft right b
                    Some (left, FPair (a, b), right)
                | None -> 
                    match innerExplode (depth + 1) b with
                    | Some (left, b, right) ->
                        let left, a = addRight left a
                        Some (left, FPair (a, b), right)
                    | None -> 
                        None
            | _ -> None
        let rec innerSplit f =
            match f with
            | FNumber a when a >= 10 ->
                let af = (a |> float) / 2.0
                let roundDn = System.Math.Floor(af) |> int
                let roundUp = System.Math.Ceiling(af) |> int
                FPair(FNumber roundDn, FNumber roundUp) |> Some
            | FPair (a,b) ->                 
                match innerSplit a with
                | Some a -> FPair (a,b) |> Some
                | None -> 
                    match innerSplit b with
                    | Some b -> FPair (a,b) |> Some
                    | None -> None
            | _ -> None 

        let mutable pf = f
        let mutable reduced = true
        while reduced do
            match innerExplode 0 pf with
            | Some (_,nf,_) -> pf <- nf
            | None -> 
                match innerSplit pf with
                | Some nf -> pf <- nf
                | None -> reduced <- false
        pf

    
    let add a b =
        FPair (a,b)

    let rec magnitude f =
        match f with
        | FNumber n -> n |> int64
        | FPair (a,b) ->  
            (3L * (magnitude a)) + (2L * (magnitude b))

    let run (input: string) (output: int -> string -> unit) =
        let fvalue, fvalueRef = createParserForwardedToRef<SnailFish, unit>()
        let pnumber = pint32 |>> FNumber
        let rec ppair = between (pstring "[") (pstring "]") (tuple2 (fvalue .>> (pstring ",")) fvalue |>> FPair) 
        fvalueRef.Value <- choice [pnumber; ppair]        

        let list =
            input 
            |> splitLine
            |> Array.map (runParser fvalue)

        let part1 =
            list |> Array.reduce (fun a b -> add a b |> reduce)

        part1 |> magnitude |> string |> output 1

        let part2 =
            Seq.allPairs list list
            |> Seq.choose (fun (a,b) -> if a = b then None else add a b |> reduce |> magnitude |> Some)
            |> Seq.max
        
        part2 |> string |> output 2