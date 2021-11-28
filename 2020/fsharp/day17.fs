namespace AdventOfCode2020

// Conway Cubes
module Day17 =    
    let print (p : 'a[,,]) =
        for z = 0 to (Array3D.length3 p) - 1 do
            let slice = p[*, *, z]

            printfn "z=%d" z
            printfn "%A" slice            

    let adj x y z (p : char[,,]) =
        let n = p[(x - 1)..(x + 1), (y - 1)..(y + 1), (z - 1)..(z + 1)]
        let mutable c = 0
        Array3D.iter (fun v -> if v = '#' then c <- c + 1) n
        c

    let trim (p : char[,,]) : char[,,] =
        let min = Array.zeroCreate p.Rank
        let max = Array.zeroCreate p.Rank

        let findRange x y z v = 
            if v = '#' then
                let on_coords = [| x; y; z|]
                Array.iteri2 (fun d n e -> if n < e then min[d] <- n) on_coords min  
                Array.iteri2 (fun d n e -> if n > e then max[d] <- n) on_coords max 
        p |> Array3D.iteri findRange
        

        Array3D.init (1 + max[0] - min[0]) (1 + max[1] - min[1]) (1 + max[2] - min[2]) (fun x y z -> p[x + min[0], y + min[1], z + min[2]])

    let cycle (p : char[,,]) : char[,,] =
        let nmap x y z =
            let c = adj (x - 1) (y - 1) (z - 1) p

            let v = 
                if x > 0 && (x <= Array3D.length1 p) && y > 0 && (y <= Array3D.length2 p) && z > 0 && (z <= Array3D.length3 p) then p[x-1, y-1, z-1] 
                else '.'

            match v with
            | '#' when c <> 2 && c <> 3 -> '.'
            | '.' when c = 3 -> '#'
            | _ -> v
        
        Array3D.init ((Array3D.length1 p) + 2) ((Array3D.length2 p) + 2) ((Array3D.length3 p) + 2) nmap
        //|> trim

    let part1 (input : string seq) =
        let init = array2D input
        let p = Array3D.create (Array2D.length1 init) (Array2D.length2 init) 1 '.'
        p[*,*,0] <- init

        print p

        let np = cycle p

        print np
        -1
        
    let part2 input =
        -1