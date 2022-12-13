namespace AdventOfCode.FSharp.Y2022

// Day 13
module Day13 =
    open Checked
    open AdventOfCode.FSharp.Util 
    open FParsec

    type P =
        | PList of P list
        | PNum of int

    let runParser p str =
        match run p str with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith (sprintf "Failure: %s" errorMsg)        

    let run (input: byte array) (output: int -> string -> unit) =
        let fvalue, fvalueRef = createParserForwardedToRef<P, unit>()
        let pnumber = pint32 |>> PNum
        let rec plist = pstring "[" >>. sepBy fvalue (pstring ",") .>> pstring "]" |>> PList
        fvalueRef.Value <- choice [pnumber; plist]        


        let pairs = input |> text |> splitDoubleLine

        let x = None
        let mutable t = 0

        let rec tostr p =
            match p with
            | PList items -> sprintf "[%s]" (String.concat "," (items |> List.map tostr))
            | PNum n -> sprintf "%i" n

        let rec compare a b =
            match a,b with
            | PNum(aa), PNum(bb) -> 
                if aa = bb then None else 
                    Some (aa < bb)   
            | PList(aa), PList(bb) -> 
                match (aa,bb) with 
                | xh::xr,bh::br -> 
                    match compare xh bh with
                    | Some v -> Some v
                    | None -> compare (PList(xr)) (PList(br))
                | [],[] -> None
                | [],_ ->  Some true
                | _,[] ->  Some false
            | PNum(_), PList(_) -> compare (PList([a])) b
            | PList(_), PNum(_) -> compare a (PList([b]))
        
        for (index, pair) in pairs |> Array.indexed do
            let p = pair |> splitLine
            let a = p[0] |> (runParser fvalue)
            let b = p[1] |> (runParser fvalue)

            if (compare a b).Value then
                t <- t + (index + 1)

        let lines = 
            ((input |> text) + "\n[[2]]\n[[6]]\n") 
            |> splitDoubleLine 
            |> Array.map splitLine 
            |> Array.concat

        let foo = 
            lines
            |> Array.map (runParser fvalue)
            |> Array.sortWith (fun a b -> if (compare a b).Value then -1 else 1 )
            
        let xx = foo |> Array.findIndex(fun q -> q = PList([PList([PNum(6)])]))
        let yy = foo |> Array.findIndex (fun q -> q = PList([PList([PNum(2)])]))

        t |> string |> output 1
        ((xx + 1)*(yy + 1)) |> string |> output 2