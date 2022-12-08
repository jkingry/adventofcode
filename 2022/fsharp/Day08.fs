namespace AdventOfCode.FSharp.Y2022

// Day 8: Treetop Tree House
module Day08 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =
        let zero = int '0'

        let fromleft = Array.zeroCreate 10
        let mutable fromtops = [| |]
        let mutable col = 0

        let mutable o = ""
        for c in input do 
            if c = '\n' then
                o <- sprintf "%s%c" o c

                col <- 0                
                Array.fill fromleft 0 fromleft.Length 0 
            else
                let v = (int c) - zero
                
                let lv = char (zero + fromleft[v])
                for i in 0..9 do
                    if i <= v then fromleft[i] <- 1
                    else fromleft[i] <- fromleft[i] + 1

                if fromtops.Length <= col then
                    fromtops <- fromtops |> Array.insertAt fromtops.Length (Array.zeroCreate 10)
                let fromtop = fromtops[col]
                
                let lt = char (zero + fromtop[v])

                o <- sprintf "%s[%c|%c]" o lv lt

                for i in 0..9 do
                    if i <= v then fromtop[i] <- 1
                    else fromtop[i] <- fromtop[i] + 1
                col <- col + 1                      
                  

        printfn "from-left-top:\n%s\n" o

        o <- ""
        let mutable fromright = Array.create 10 (-1)
        for c in input do 
            if c = '\n' then
                for i in 0..9 do
                    if fromright[i] >= 0 then
                        let fr = fromright[i]
                        o <- sprintf "%s%c" o (char (zero + fr))
                o <- sprintf "%s%c" o c
                Array.fill fromright 0 fromright.Length (-1) 
            else
                let v = (int c) - zero
                for i in 9..(-1)..0 do
                    if fromright[i] >= 0 then
                        fromright[i] <- fromright[i] + 1
                        if i <= v then
                            let fr = fromright[i]
                            o <- sprintf "%s%c" o (char (zero + fr))
                            fromright[i] <- -1
                fromright[v] <- 0

        printfn "fromright:\n%s\n" o

        o <- ""
        let mutable frombots = [| |]
        let mutable col = 0
        for c in input do 
            if c = '\n' then
                o <- sprintf "%s%c" o c
                col <- 0
            else
                if frombots.Length <= col then
                    frombots <- frombots |> Array.insertAt frombots.Length (Array.create 10 (-1))
                let frombot = frombots[col]

                let v = (int c) - zero

                for i in 9..(-1)..0 do
                    if frombot[i] >= 0 then
                        frombot[i] <- frombot[i] + 1
                        if i <= v then
                            let fb = frombot[i]
                            o <- sprintf "%s%c" o (char (zero + fb))
                            frombot[i] <- -1
                frombot[v] <- 0

                col <- col + 1
        for frombot in frombots do
            for i in 0..9 do
                if frombot[i] >= 0 then
                    let fb = frombot[i]
                    o <- sprintf "%s%c" o (char (zero + fb))

        printfn "frombot:\n%s\n" o

        o <- ""        

                  

        //printfn "fromtop:\n%s\n" o



        let lines = input |> splitLine 
        let a = lines |> Array.map (
            fun s -> s.ToCharArray() |> Array.map (fun x -> (byte x) - (byte '0'))) |> array2D

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
