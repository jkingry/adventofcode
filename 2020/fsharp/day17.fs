namespace AdventOfCode.FSharp.Y2020

// Conway Cubes
module Day17 =
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic

    let print (p: 'a[,,]) =
        for z = 0 to (Array3D.length3 p) - 1 do
            let slice = p.[*, *, z]
            printfn "z=%d" z
            printfn "%A" slice

    let countOn (p: char[,,]) =
        let mutable c = 0

        Array3D.iter
            (fun v ->
                if v = '#' then
                    c <- c + 1)
            p

        c

    let adj x y z (p: char[,,]) =
        let n = p.[(x - 1) .. (x + 1), (y - 1) .. (y + 1), (z - 1) .. (z + 1)]

        (countOn n) - (if p.[x, y, z] = '#' then 1 else 0)

    let trim (p: char[,,]) : char[,,] =
        let min = Array.create p.Rank System.Int32.MaxValue
        let max = Array.create p.Rank System.Int32.MinValue

        let findRange x y z v =
            if v = '#' then
                let on_coords = [| x; y; z |]

                Array.iteri2
                    (fun d n e ->
                        if n < e then
                            min.[d] <- n)
                    on_coords
                    min

                Array.iteri2
                    (fun d n e ->
                        if n > e then
                            max.[d] <- n)
                    on_coords
                    max

        p |> Array3D.iteri findRange

        Array3D.init (1 + max.[0] - min.[0]) (1 + max.[1] - min.[1]) (1 + max[2] - min[2]) (fun x y z ->
            p.[x + min.[0], y + min.[1], z + min.[2]])

    let expand (sz: int) (p: char[,,]) : char[,,] =
        let nd d = (p.GetLength d) + sz

        let init x y z =
            try
                p.[x - (sz / 2), y - (sz / 2), z - (sz / 2)]
            with _ ->
                '.'

        Array3D.init (nd 0) (nd 1) (nd 2) init

    let cycle (p: char[,,]) : char[,,] =
        let p' = expand 2 p

        let cycle' x y z v =
            let c = adj x y z p'

            match v with
            | '#' -> if c = 2 || c = 3 then '#' else '.'
            | '.' -> if c = 3 then '#' else '.'
            | _ -> failwith $"Unexpected '%c{v}'"

        Array3D.mapi cycle' p' |> trim


    let part1 (input: string) =
        let init = input |> splitLine |> array2D
        let mutable p = Array3D.create (Array2D.length1 init) (Array2D.length2 init) 1 '.'
        p.[*, *, 0] <- init

        for i = 1 to 6 do
            p <- cycle p

        p |> countOn |> string

    let Array4D_iteri (f: int -> int -> int -> int -> 'a -> unit) (a: 'a[,,,]) =
        for x = 0 to (Array4D.length1 a) - 1 do
            for y = 0 to (Array4D.length2 a) - 1 do
                for z = 0 to (Array4D.length3 a) - 1 do
                    for w = 0 to (Array4D.length4 a) - 1 do
                        f x y z w a.[x, y, z, w]

    let Array4D_mapi (f: int -> int -> int -> int -> 'a -> 'b) (a: 'a[,,,]) : 'b[,,,] =
        let f' x y z w = f x y z w a.[x, y, z, w]
        Array4D.init (Array4D.length1 a) (Array4D.length2 a) (Array4D.length3 a) (Array4D.length4 a) f'

    let Array4D_iter (f: 'a -> unit) (a: 'a[,,,]) =
        let f' x y z w v = f v
        Array4D_iteri f' a

    let countOn4 (p: char[,,,]) =
        let mutable c = 0

        Array4D_iter
            (fun v ->
                if v = '#' then
                    c <- c + 1)
            p

        c

    let adj4 x y z w (p: char[,,,]) =
        let n =
            p.[(x - 1) .. (x + 1), (y - 1) .. (y + 1), (z - 1) .. (z + 1), (w - 1) .. (w + 1)]

        (countOn4 n) - (if p.[x, y, z, w] = '#' then 1 else 0)

    let trim4 (p: char[,,,]) : char[,,,] =
        let min = Array.create p.Rank System.Int32.MaxValue
        let max = Array.create p.Rank System.Int32.MinValue

        let findRange x y z w v =
            if v = '#' then
                let on_coords = [| x; y; z; w |]

                Array.iteri2
                    (fun d n e ->
                        if n < e then
                            min.[d] <- n)
                    on_coords
                    min

                Array.iteri2
                    (fun d n e ->
                        if n > e then
                            max.[d] <- n)
                    on_coords
                    max

        p |> Array4D_iteri findRange

        Array4D.init
            (1 + max.[0] - min.[0])
            (1 + max.[1] - min.[1])
            (1 + max.[2] - min.[2])
            (1 + max.[3] - min.[3])
            (fun x y z w -> p.[x + min.[0], y + min.[1], z + min.[2], w + min.[3]])

    let expand4 (sz: int) (p: char[,,,]) : char[,,,] =
        let nd d = (p.GetLength d) + sz

        let init x y z w =
            try
                p.[x - (sz / 2), y - (sz / 2), z - (sz / 2), w - (sz / 2)]
            with _ ->
                '.'

        Array4D.init (nd 0) (nd 1) (nd 2) (nd 3) init

    let cycle4 (p: char[,,,]) : char[,,,] =
        let p' = expand4 2 p

        let cycle' x y z w v =
            let c = adj4 x y z w p'

            match v with
            | '#' -> if c = 2 || c = 3 then '#' else '.'
            | '.' -> if c = 3 then '#' else '.'
            | _ -> failwith $"Unexpected '%c{v}'"

        Array4D_mapi cycle' p' |> trim4

    let part2 (input: string) =
        let init = input |> splitLine |> array2D
        let mutable p = Array4D.create (Array2D.length1 init) (Array2D.length2 init) 1 1 '.'
        p[*, *, 0, 0] <- init

        for i = 1 to 6 do
            p <- cycle4 p

        p |> countOn4 |> string
