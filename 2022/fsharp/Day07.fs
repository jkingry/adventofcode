namespace AdventOfCode.FSharp.Y2022

// Day 7: No Space Left On Device
module Day07 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: byte[]) (output: int -> string -> unit) =
        let getAllParentPaths pathList =
            (pathList, [])
            ||> List.scanBack (fun a b -> a :: b)
            |> List.filter (fun p -> p.Length > 0)
            |> List.map (fun p -> "/" + System.String.Join("/", p |> List.rev |> List.tail))

        let foundFile pathList fileSize dirSizes =
            pathList
            |> getAllParentPaths
            |> List.fold
                (fun dirSizes' dirPath -> dirSizes' |> Map.change dirPath (fun v -> fileSize + defaultArg v 0 |> Some))
                dirSizes

        let rec processInput (pathList, dirSizes) inputLines =
            match inputLines with
            | line :: nextLines ->
                match line with
                | "$ cd .." ->
                    let newPathList = List.tail pathList
                    processInput (newPathList, dirSizes) nextLines
                | "$ ls" ->
                    let (nextLines, dirSize) = handleList nextLines 0
                    let dirSizes' = dirSizes |> foundFile pathList dirSize
                    processInput (pathList, dirSizes') nextLines
                | _ ->
                    let dir = (line.Split(' ')[2])
                    let newPathList = dir :: pathList
                    processInput (newPathList, dirSizes) nextLines
            | [] -> (pathList, dirSizes)

        and handleList inputLines dirSize =
            match inputLines with
            | line :: nextLines ->
                if line[0] = '$' then
                    inputLines, dirSize
                else
                    let parts = line.Split(' ')
                    let fsize = if parts[0] = "dir" then 0 else int parts[0]
                    handleList nextLines (dirSize + fsize)
            | [] -> inputLines, dirSize

        let (_, dirMap) =
            input |> text |> splitLine |> Array.toList |> processInput ([], Map.empty)

        let dirSizes = dirMap |> Map.values |> Seq.toArray

        let smallDirsTotal = dirSizes |> Seq.filter (fun v -> v <= 100000) |> Seq.sum
        smallDirsTotal |> string |> output 1

        let diskSize = 70_000_000
        let neededFree = 30_000_000

        let usedSize = dirSizes |> Array.max

        let needToDeleteSize = usedSize - (diskSize - neededFree)

        let dirToDeleteSize =
            dirSizes |> Seq.filter (fun v -> v >= needToDeleteSize) |> Seq.min

        dirToDeleteSize |> string |> output 2
