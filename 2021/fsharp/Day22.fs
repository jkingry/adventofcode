namespace AdventOfCode.FSharp.Y2021

module Day22 =
    open AdventOfCode.FSharp.Util
        
    let run (input: string) (output: int -> string -> unit) =
        let rebootSteps = 
            input 
            |> splitLine
            |> Array.map (fun line -> 
                match line with
                | Regex "(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" [onoff;x1;x2;y1;y2;z1;z2] ->    
                    onoff = "on", (int x1,int y1, int z1), (int x2, int y2, int z2)
                | _ -> failwith "invalid")

        let getD d (x,y,z) = match d with | 0 -> x | 1 -> y | 2 -> z | _ -> failwith "impossible"

        let axi = 
            [|0..2|]
            |> Array.map (fun d -> 
                Array.concat (rebootSteps |> Array.map (fun (_,a,b) -> [|(getD d a); (getD d b) + 1|]))
                |> Array.map int64
                |> Array.distinct
                |> Array.sort)

        let (mx,my,mz) = axi |> Array.map Array.length |> array2tuple3 
        let core = Array3D.init mx my mz (fun x y z -> 
            if x < mx - 1 && y < my - 1 && z < mz - 1 then 
                (axi[0][x + 1] - axi[0][x]) * (axi[1][y+1] - axi[1][y]) * (axi[2][z+1] - axi[2][z])
            else 
                0L)

        let getAxiIndexes d a b =
            let f = getD d a |> int64
            let t = getD d b |> int64
            let fi = axi[d] |> Array.findIndex (fun v -> f = v)
            let ti = axi[d] |> Array.findIndex (fun v -> t < v)
            [fi..(ti-1)]            

        let mutable powered = 0L
        let mutable c = 0

        let applyRebootSteps steps =
            steps
            |> Array.iter (fun (on,a,b) ->
                for x in (getAxiIndexes 0 a b) do
                    for y in (getAxiIndexes 1 a b) do
                        for z in (getAxiIndexes 2 a b) do
                            c <- c + 1
                            if (core[x,y,z] > 0L) = on then
                                powered <- powered + core[x,y,z]
                                core[x,y,z] <- -1L * core[x,y,z])
        
        let (initialization, reboot) =                                
            rebootSteps
            |> Array.partition (fun (_,(x1,y1,z1),(x2,y2,z2)) -> x1 >= -50 && y1 >= -50 && z1 >= -50 && x2 <= 50 && y2 <= 50 && z2 <= 50)

        initialization |> applyRebootSteps
        powered |> string |> output 1

        reboot |> applyRebootSteps
        printfn "%d" c
        powered |> string |> output 2
