namespace AdventOfCode.FSharp.Y2020

// Day 24: Lobby Layout
module Day24 =
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic

    type grid<'T> = ('T[,] * int)

    module Grid =
        let create size =
            let grid = Array2D.zeroCreate size size
            let offset = size / 2
            (grid, offset)
        let set (x: int) (y: int) (v: 'T) (grid: grid<'T>): grid<'T> =
            let mutable (g,o) = grid            
            let mutable (ax,ay) = (x + o, y + o)
            let mutable sz = Array2D.length1 g
            while ax < 0 || ay < 0 || ax >= sz || ay >= sz do                            
                let sz' = sz * 2
                let d = (sz' - sz)  / 2
                g <- Array2D.init sz' sz' (fun x y -> if x >= d && y >= d && x < (sz + d) && y < (sz + d) then g[x-d,y-d] else Unchecked.defaultof<'T>)
                o <- o + d
                ax <- x + o
                ay <- y + o
                sz <- sz'
                
            g[ax,ay] <- v
            g,o            
        let get (x: int) (y: int) (grid: grid<'T>) =
            let  (g,o) = grid            
            let  (ax,ay) = (x + o, y + o)
            let sz = Array2D.length1 g
            if ax >= 0 && ay >= 0 && ax < sz && ay < sz then g[ax,ay] else Unchecked.defaultof<'T>

        let setWith (x: int) (y: int) (getValue: 'T -> 'T) (grid: grid<'T>) =
            set x y (get x y grid |> getValue) grid
        let copy (g,o) =
            (Array2D.copy g),o


    let run data output =
        let input = data |> text
        let parseTile line =
            let (q,r,_) = 
                line |> Seq.fold (fun (q, r, s) c -> 
                    match c with
                    | 'n' -> (q,r-1,1) 
                    | 's' -> (q,r+1,-1) 
                    | 'e' -> ((q + if s >= 0 then 1 else 0),r,0) 
                    | 'w' -> ((q - if s <= 0 then 1 else 0),r,0) 
                    | a -> failwith (sprintf "Unreachable %A" a) ) (0,0,0)
            q,r

        let DARK_BIT = 0b1000y
        let NEIGHBOR_MASK = 0b111y

        let flip q r (g,c) =
            let v = Grid.get q r g
            let dark = (v &&& DARK_BIT) <> 0y
            let neighbours = v &&& NEIGHBOR_MASK
            let (v', c', delta) = 
                if dark then (neighbours, c - 1, -1y) 
                else (neighbours ||| DARK_BIT, c + 1, 1y)
            
            let g' = g |> Grid.set q r v'
            let g' = 
                [(1,0);(1,-1);(0,-1);(-1,0);(-1,1);(0,1)]
                |> List.fold (fun g (dq,dr) -> g |> Grid.setWith (q+dq) (r+dr) (fun v -> v + delta)) g'
            g',c'

        let mutable (g,c) =
            input
            |> splitLine
            |> Array.map parseTile
            |> Array.fold (fun lobby (q,r) -> flip q r lobby) ((Grid.create 1000), 0)     
        
        c |> string |> output 1

        // let step ((g,o),c) =
        //     let g' Array2D.copy g 
        //     g |> Array2D.iteri (fun x y v -> 
        //         (g',c') <- 
        //             match v with
        //             | 2 -> flip x y g'
        //             | 9 | 10 -> ()
        //             | v when (v &&& DARK_BIT) = DARK_BIT -> flip x y g'
        //             | _ -> ())


