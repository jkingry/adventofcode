namespace AdventOfCode.FSharp.Y2022

// Day 7
module Day07 =
    open AdventOfCode.FSharp.Util
    open System.Numerics
    open Checked

    let run (input: string) (output: int -> string -> unit) =
        let mutable pos = []
        let mutable f = Set.empty
        let mutable d = Map.empty
        for line in (input |> splitLine) do
            let xline =  
                if line.StartsWith("$ ") then
                    line.Substring(2)
                else 
                    line

            if xline = "ls" then
                ignore
            else
                let a = xline.Split(' ')
                match a[0] with 
                | "cd" -> 
                    if a[1] = ".." then
                        let fff = List.tail pos
                        pos <- fff
                    else    
                        let qqq = a[1]::pos
                        pos <- qqq      
                    ignore                              
                | "dir" -> ignore
                | _ ->
                    let fsize = (int a[0])
                    let fpath = System.String.Join ("/", a[1]::pos)
                    if not (f |> Set.contains fpath) then
                        for j in 1 ..(List.length pos) do
                            let npos = pos |> List.rev |> List.take j 
                            let dpath = System.String.Join ("/", npos)
                            d <- d |>  Map.change dpath (fun v -> match v with | None -> Some fsize | Some q -> Some (q + fsize))
                        f <- f |> Set.add fpath
                        ignore
                    else
                        ignore

        printfn "%A" d
        let smalld = d |> Map.values |> Seq.filter (fun v -> v <= 100000) |> Seq.sum
                
        let tod = d |> Map.values |> Seq.filter (fun v -> v >= 6876531 ) |> Seq.sort |> Seq.head

                
        
        smalld |> string |> output 1
        tod |> string |> output 2
