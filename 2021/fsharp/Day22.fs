namespace AdventOfCode.FSharp.Y2021

// Day 22: Reactor Reboot
module Day22 =
    open AdventOfCode.FSharp.Util
    type Point = int64 * int64 * int64    

    type Cuboid = Point * Point

    let isValid ((x1,y1,z1), (x2,y2,z2)) =
        x1 < x2 && y1 < y2 && z1 < z2

    let getVolume ((x1,y1,z1),(x2,y2,z2)) =
        (x2 - x1) * (y2 - y1) * (z2 - z1)

    let getOverlap (a:Cuboid) (b:Cuboid) =
        let ((aulx, auly, aulz), (abrx, abry, abrz)) = a
        let ((bulx, buly, bulz), (bbrx, bbry, bbrz)) = b

        let overlap = ((max aulx bulx),(max auly buly),(max aulz bulz)), ((min abrx bbrx),(min abry bbry), (min abrz bbrz))

        if isValid overlap then Some overlap else None        

    type Instruction = 
        {
            value : bool
            cube : Cuboid
        }

    let parseInstructions input = 
        input 
        |> splitLine
        |> Array.toList
        |> List.map (fun line -> 
            match line with
            | Regex "(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" [onoff;x1;x2;y1;y2;z1;z2] ->
                {   
                    value = (onoff = "on")
                    cube = ((int64 x1),(int64 y1),(int64 z1)), ((int64 x2) + 1L, (int64 y2) + 1L, (int64 z2) + 1L)
                }    
            | _ -> failwith "invalid")            

    let rec runInstructions (depth: int) (instructions :  list<Instruction>) =

        let runInstruction (placed: list<Cuboid>, volume: int64) (instruction: Instruction)  =
            let newPlaced = placed@[instruction.cube]
            let newVolume =
                if instruction.value then
                    let overlaps = 
                        placed 
                        |> List.choose (getOverlap instruction.cube) 
                        |> List.map (fun overlapping -> { value = true; cube = overlapping})
                    volume + (getVolume instruction.cube) - (runInstructions (depth + 1) overlaps)
                else
                    volume
            (newPlaced, newVolume)

        let volume = instructions |> List.rev |> List.fold runInstruction ([], 0L) |> snd
        assert (volume >= 0L)
        volume

    let runPart1Instructions instructions =
        let region = ((-50L, -50L, -50L),(51L, 51L, 51L))
        instructions 
            |> List.filter (fun x -> getOverlap region x.cube |> Option.isSome)
            |> runInstructions 0

    let run (input: string) (output: int -> string -> unit) =
        let instructions = parseInstructions input

        instructions |> runPart1Instructions |> string |> output 1
        instructions |> runInstructions 0 |> string |> output 2

