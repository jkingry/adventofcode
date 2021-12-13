namespace AdventOfCode.FSharp.Y2021

// Day 13
module Day13 =     
    open AdventOfCode.FSharp.Util

    let part1 (text : string) : string =
        let mutable dots =
            (text |> splitDoubleLine)[0] 
            |> splitLine
            |> Array.map ints
            |> Array.map (fun [|x;y|] -> (x,y))
            |> Set.ofArray
        
        
        let folds =
            (text |> splitDoubleLine)[1]
            |> splitLine
            |> Array.map (fun line -> 
                match line with
                | Regex @"fold along (y|x)=(\d+)" [axis;num] -> (axis, int num)
                | _ -> failwith "Invalid")

        let foldy (yy:int) (i : Set<int*int>) : Set<int*int> = 
            let bottom = i |> Seq.filter(fun (_,y) -> y > yy)
            bottom 
            |> Seq.fold (fun a (x,y) -> a |> Set.add (x, yy - (y - yy))) i
            |> Set.filter (fun (_,y) -> y <= yy)

        let foldx (yy:int) (i : Set<int*int>) : Set<int*int> = 
            let bottom = i |> Seq.filter(fun (y,_) -> y > yy)
            bottom 
            |> Seq.fold (fun a (y,x) -> a |> Set.add (yy - (y - yy), x)) i
            |> Set.filter (fun (y,_) -> y <= yy)            
        
        for f in folds do
            match f with
            | ("x",n) -> dots <- foldx n dots
            | ("y",n) -> dots <- foldy n dots

        let maxx = dots |> Seq.map (fun (x,_) -> x) |> Seq.max
        let maxy = dots |> Seq.map (fun (_,y) -> y) |> Seq.max

        let mutable result = System.Environment.NewLine

        for y=0 to maxy do
            let s = new System.Text.StringBuilder (String.replicate (maxx + 1) ".")
            let row = dots |> Seq.filter (fun (_,yy) -> yy = y)
            for (x,_) in row do
                s[x] <- '#'
            result <- result + (string s) + System.Environment.NewLine
  
        result

    let part2 (text : string) : string =   
        -1 |> string