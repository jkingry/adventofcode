namespace AdventOfCode.FSharp.Y2022

// Day 7: No Space Left On Device
module Day07 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =
        let mutable pathList = []
        let mutable d = Map.empty

        let noPrefixLines = 
            input
            |> splitLine
            // who needs prefixes
            |> Array.map (fun s -> s.TrimStart('$',' '))
            // who needs ls
            |> Array.filter (fun s -> s <> "ls")

        for line in noPrefixLines do
            let args = line.Split(' ')
            match args[0] with 
            | "cd" -> 
                if args[1] = ".." then
                    pathList <- List.tail pathList
                else    
                    pathList <-  args[1]::pathList                                                        
            | "dir" -> ()
            | _ ->
                let fsize = (int args[0])

                // add filesize to every parent path
                for pathLength in 1 ..(List.length pathList) do
                    let parentPathList = pathList |> List.rev |> List.take pathLength
                    let parentPath = System.String.Join ("/", parentPathList)
                    d <- d |>  Map.change parentPath (fun v -> fsize + defaultArg v 0 |> Some)


        let dirSizes = d |> Map.values |> Seq.toArray

        let smallDirsTotal = dirSizes |> Seq.filter (fun v -> v <= 100000) |> Seq.sum
        smallDirsTotal |> string |> output 1
                
        let totalSize = dirSizes |> Array.max
        let allowedSize = 70000000 - 30000000
        let needToDeleteSize =  totalSize- allowedSize 
        let dirToDeleteSize = dirSizes |> Seq.filter (fun v -> v >= needToDeleteSize ) |> Seq.sort |> Seq.head       
        dirToDeleteSize |> string |> output 2
