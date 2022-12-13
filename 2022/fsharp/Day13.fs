namespace AdventOfCode.FSharp.Y2022

// Day 13
module Day13 =
    open Checked
    open AdventOfCode.FSharp.Util 
    open FParsec

    type Packet =
        | PList of Packet list
        | PNum of int

    let rec tostr p =
        match p with
        | PList items -> sprintf "[%s]" (String.concat "," (items |> List.map tostr))
        | PNum n -> sprintf "%i" n    
        
    let rec compare a b =
        match a,b with
        | PNum(aval), PNum(bval) -> 
            if aval = bval then None else Some (aval < bval)   
        | PList(alist), PList(blist) -> 
            match (alist,blist) with 
            | x::xs,y::ys -> 
                match compare x y with
                | Some v -> Some v
                | None -> compare (xs |> PList) (ys |> PList)
            | [],[] -> None
            | [],_ ->  Some true
            | _,[] ->  Some false
        | PNum(_), PList(_) -> compare ([a] |> PList) b
        | PList(_), PNum(_) -> compare a ([b] |> PList)            

    let runParser p str =
        match run p str with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg        

    let run (input: byte array) (output: int -> string -> unit) =
        let fvalue, fvalueRef = createParserForwardedToRef<Packet, unit>()
        let pnumber = pint32 |>> PNum
        let plist = pstring "[" >>. sepBy fvalue (pstring ",") .>> pstring "]" |>> PList
        fvalueRef.Value <- choice [pnumber; plist]        

        let pairs = 
            input 
            |> text 
            |> splitDoubleLine
            |> Array.map (fun text ->
                let parts = text |> splitLine |> Array.map (runParser fvalue)
                parts[0],parts[1])

        let mutable part1 = 0
        
        for (index, (a,b)) in pairs |> Array.indexed do
            if (compare a b).Value then
                part1 <- part1 + (index + 1)

        part1 |> string |> output 1

        let divPack1 = runParser fvalue "[[2]]"
        let divPack2 = runParser fvalue "[[6]]"

        let packets = 
            pairs 
            |> Array.map (fun (a,b) -> [|a;b|]) 
            |> Array.concat
            |> Array.append [| divPack1; divPack2 |]

        let packets = 
            packets
            |> Array.sortWith (fun a b -> if (compare a b).Value then -1 else 1)
            
        let index1 = packets |> Array.findIndex(fun v -> v = divPack1)
        let index2 = packets |> Array.findIndex (fun v -> v = divPack2)

        let part2 = (index1 + 1) * (index2 + 1)
        part2 |> string |> output 2
