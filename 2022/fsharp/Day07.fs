namespace AdventOfCode.FSharp.Y2022

// Day 7: No Space Left On Device
module Day07 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =
        let getAllParentPaths pathList =
            (pathList, []) 
            ||> List.scanBack (fun a b -> a::b ) 
            |> List.filter (fun p -> p.Length > 0)
            |> List.map (fun p -> "/" + System.String.Join ("/", p |> List.rev |> List.tail))

        let foundFile pathList dirSizes fileSize =
            pathList
            |> getAllParentPaths
            |> List.fold (fun dirSizes' dirPath -> 
                dirSizes' |> Map.change dirPath (fun v -> fileSize + defaultArg v 0 |> Some)) dirSizes

        let rec processInput (pathList, dirSizes) inputLines =
            match inputLines with
            | line::nextLines ->
                match line with
                | "$ cd .." -> 
                    let newPathList = List.tail pathList
                    processInput (newPathList, dirSizes) nextLines
                | "$ ls" -> handleList (pathList, dirSizes) nextLines
                | _ ->
                    let dir = (line.Split(' ')[2])
                    let newPathList = dir::pathList
                    processInput (newPathList, dirSizes) nextLines
            | [] -> (pathList, dirSizes)
        and handleList state inputLines = 
            match inputLines with
            | line::nextLines ->
                if line[0] = '$' then
                    processInput state inputLines
                else
                    let parts = line.Split(' ')
                    if parts[0] = "dir" then
                        handleList state nextLines
                    else
                        let (pathList, dirSizes) = state
                        let fsize = int parts[0]
                        let newDirSizes = foundFile pathList dirSizes fsize
                        handleList (pathList, newDirSizes) nextLines
            | [] -> state

        let (_, dirMap) = input |> splitLine |> Array.toList |> processInput ([], Map.empty) 

        let dirSizes = dirMap |> Map.values |> Seq.toArray

        let smallDirsTotal = dirSizes |> Seq.filter (fun v -> v <= 100000) |> Seq.sum
        smallDirsTotal |> string |> output 1
        
        let diskSize   = 70_000_000
        let neededFree = 30_000_000

        let usedSize = dirSizes |> Array.max

        let needToDeleteSize =  usedSize- (diskSize - neededFree) 
        let dirToDeleteSize = dirSizes |> Seq.filter (fun v -> v >= needToDeleteSize ) |> Seq.sort |> Seq.head       
        
        dirToDeleteSize |> string |> output 2
