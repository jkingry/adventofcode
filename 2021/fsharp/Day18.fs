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

    let rec addLeft n x =
        match x with 
        | FNumber xn -> 0,FNumber (xn + n)
        | FPair (a, b) -> 
            let (nb,b') = addLeft n b
            let (na,a') = addLeft nb a
            na, FPair (a',b')

    let rec addRight n x =
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


    let explode f =
        let rec innerExplode depth f =
            match f with
            | FPair (FNumber a, FNumber b) when depth = 4 ->
                Some (a, FNumber 0, b)
            | FPair (a, b) -> 
                match innerExplode (depth + 1) a with
                | Some (left, a, right) -> 
                    let right, b = addRight right b
                    Some (left, FPair (a, b), right)
                | None -> 
                    match innerExplode (depth + 1) b with
                    | Some (left, b, right) ->
                        let left, a = addLeft left a
                        Some (left, FPair (a, b), right)
                    | None -> 
                        None
            | _ -> None
        let mutable pf = f
        let mutable reduced = true
        while reduced do
            printfn "%s" (toString pf)
            let result = innerExplode 0 pf
            match result with
            | Some (_,nf,_) -> pf <- nf
            | None -> reduced <- false
        pf

    let split f =
        let rec innerSplit f =
            match f with
            | FNumber a when a >= 10 ->
                let af = (a |> float) / 2
                let roundDn = System.Math.Floor(af) |> int
                let roundUp = System.Math.Ceiling(af) |> int
                Some FPair(FNumber roundDn, FNumber roundUp)
            | FPair (a,b) ->                 
                match innerSplit (depth + 1) a with
                | Some (left, a, right) -> 
                    let right, b = addRight right b
                    Some (left, FPair (a, b), right)
                | None -> 
                    match innerExplode (depth + 1) b with
                    | Some (left, b, right) ->
                        let left, a = addLeft left a
                        Some (left, FPair (a, b), right)
                    | None -> 
                        None
            | _ -> None
        let mutable pf = f
        let mutable reduced = true
        while reduced do
            printfn "%s" (toString pf)
            let result = innerExplode 0 pf
            match result with
            | Some (_,nf,_) -> pf <- nf
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

        let a = list[0]
        let b = list[1]
        let ab = add a b
        let abr = reduce ab

        -1 |> string |> output 1