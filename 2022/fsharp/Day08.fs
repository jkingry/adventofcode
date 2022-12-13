namespace AdventOfCode.FSharp.Y2022

// Day 8: Treetop Tree House
module Day08 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: byte array) (output: int -> string -> unit) =
        let lines = input |> text |> splitLine 
        let a = lines |> Array.map (
            fun s -> s.ToCharArray() |> Array.map (fun x -> (byte x) - '0'B)) |> array2D

        let mx = Array2D.length1 a
        let my = Array2D.length2 a

        let mutable visible = 0    

        let mutable maxScore = 0

        let countUntil v l =
            let mutable r = 0
            let mutable stop = false
            for e in l do
                if not stop then
                    r <- r + 1
                    if e >= v then stop <- true
            r        

        let traverse x y v =
            let top = a[0..(x-1), y] 
            let bot = a[(x+1)..(mx-1), y] 
            let rgt = a[x, 0..(y-1)]      
            let lft = a[x, (y+1)..(my-1)] 

            if top |> Array.forall (fun vv -> vv < v) 
                || bot |> Array.forall (fun vv -> vv < v) 
                || rgt |> Array.forall (fun vv -> vv < v) 
                || lft |> Array.forall (fun vv -> vv < v) then
                visible <- visible + 1

            let tops = top |> Array.rev |> countUntil v
            let bots = bot |> countUntil v
            let rgts = rgt |> Array.rev |> countUntil v            
            let lfts = lft |> countUntil v            
            
            let score = tops * bots * rgts * lfts
            if score > maxScore then maxScore <- score

        a |> Array2D.iteri traverse

        visible |> string |> output 1
        maxScore |> string |> output 2    

    type RightEntry = {
        col: int
        rgt: int
    }

    type BottomEntry =
        { 
            bot: int
            score: int }

    let emptyRight = { col = -1; rgt = -1 }

    let emptyBottom = { bot = -1; score = 1; }

    let runFast (input: byte array) (output: int -> string -> unit) =
        let fromleft = Array.create 10 0
        let fromright = Array.create 10 emptyRight

        let mutable fromtops = Array.empty
        let mutable frombots: BottomEntry[][] = Array.empty

        let mutable col = 0
        let mutable row = 0

        let mutable maxScore = 0

        for c in (input |> text) do 
            if c = '\n' then            
                col <- 0 
                row <- row + 1

                for i in 0..9 do
                    let e = fromright[i]
                    if e.rgt >= 0 then
                        let f = frombots[e.col][i]
                        frombots[e.col][i] <- { f with score = f.score * e.rgt }

                Array.fill fromright 0 fromright.Length emptyRight                                               
                Array.fill fromleft 0 fromleft.Length 0 
            else
                let v = ((byte c) - '0'B) |> int

                if fromtops.Length <= col then
                    fromtops <- fromtops |> Array.insertAt fromtops.Length (Array.create 10 0)
                let fromtop = fromtops[col]                
                if frombots.Length <= col then
                    frombots <- frombots |> Array.insertAt frombots.Length (Array.create 10 emptyBottom)
                let frombot = frombots[col]                
                
                let lft = fromleft[v]                
                let top = fromtop[v]

                for i in 0..9 do
                    let e = fromright[i]
                    if e.rgt >= 0 then
                        fromright[i] <- { e with rgt = e.rgt + 1 }
                        let e' = fromright[i]
                        if i <= v then
                            let f = frombots[e.col][i]
                            frombots[e.col][i] <- { f with score = f.score * e'.rgt }
                            fromright[i] <- emptyRight

                fromright[v] <- { emptyRight with col = col; rgt = 0 }                

                for i in 0..9 do
                    let e = frombot[i]
                    if e.bot >= 0 then
                        frombot[i] <- { e with bot = e.bot + 1 }
                        if i <= v then
                            let f = frombot[i]
                            let score = f.score * f.bot 
                            if score > maxScore then maxScore <- score
                            frombot[i] <- emptyBottom
                frombot[v] <- { frombot[v] with bot = 0; score = frombot[v].score * lft * top; }

                for i in 0..9 do
                    if i <= v then
                        fromleft[i] <- 1
                    else
                        fromleft[i] <- fromleft[i] + 1

                for i in 0..9 do
                    if i <= v then fromtop[i] <- 1
                    else fromtop[i] <- fromtop[i] + 1

                col <- col + 1

        for frombot in frombots do
            for i in 0..9 do
                let e = frombot[i]
                if e.bot >= 0 then
                    let f = frombot[i]
                    let score = f.score * f.bot 
                    if score > maxScore then maxScore <- score

        0 |> string |> output 1
        maxScore |> string |> output 2
