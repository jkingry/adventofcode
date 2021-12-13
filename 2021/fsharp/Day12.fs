namespace AdventOfCode.FSharp.Y2021

// Day 12
module Day12 =     
    open AdventOfCode.FSharp.Util
    open Checked

    type graph<'k when 'k: comparison> = Map<'k, Set<'k>>

    module Graph =
        let empty : graph<'k> = Map.empty
        let add (source: 'k) (destination: 'k) (g: graph<'k>): graph<'k> =
            g |> Map.change source (fun v -> 
                (match v with | Some s -> s | _ -> Set.empty) |> Set.add destination |> Some)
        let fromList (input: ('k*'k) list) : graph<'k> =
            input
            |> List.fold (fun g (src,dest) -> g |> add src dest) empty
        let filterEdges (fn: 'k -> bool) (graph: graph<'k>): graph<'k> =
            graph |> Map.map (fun _ v -> v |> Set.filter fn)
        let filterNodes (fn: 'k -> bool) (graph: graph<'k>): graph<'k> =
            graph |> Map.filter (fun k _ -> fn k)
        let link (g: graph<'k>): graph<'k> =
            g
            |> Map.toSeq
            |> Seq.collect (fun (k,v) -> v |> Seq.map (fun e -> k ,e))
            |> Seq.fold (fun g' (src, dest) -> add dest src g') g


    let parse text =
        let pairs = 
            text 
            |> splitLine 
            |> Array.map (fun line -> let p = line.Split('-') in p[0],p[1])
            |> Array.toList
        
        (pairs @ (List.map (fun (a,b) -> (b,a)) pairs))
        |> List.filter (fun (src,dest) -> src <> "end" && dest <> "start")
        |> List.fold (fun g (src,dest) -> 
            g |> Map.change src (fun v -> 
                (match v with | Some s -> s | _ -> Set.empty) |> Set.add dest |> Some))  Map.empty

    let findPaths visitRule (caves : Map<string, Set<string>>) =
           
        let mutable q = [(["start"], Set.empty, false)]
        let mutable ends = []

        while q.Length > 0 do    
            match q with
            | (current::path, visited, doubleCave)::next ->                            
                q <- next
                if current = "end" then ends <- path::ends else
                q <- 
                    caves[current] 
                    |> Seq.choose (visitRule path visited doubleCave)
                    |> Seq.fold (fun a x -> x::a) q
            | _ -> failwith "Unreachable"
        ends

    let inline small (k: string) = k.ToLower() = k 

    let part1 (text : string) =   
        let caves = parse text

        let visitRule path visited doubleCave k =
            if not (small k) then
                Some (k::path, visited, doubleCave)
            else
                if not (Set.contains k visited) then
                    Some (k::path, visited, doubleCave)
                else    
                    None   

        let paths = caves |> findPaths visitRule                        

        List.length paths

    let part2 (text : string) =   
        let caves = parse text

        let visitRule path visited doubleCave k =
            if not (small k) then
                Some (k::path, visited, doubleCave)
            else
                if not (Set.contains k visited) then
                    Some (k::path, visited |> Set.add k, doubleCave)
                elif not doubleCave then
                    Some (k::path, visited, true)
                else
                    None   

        let paths = caves |> findPaths visitRule
        List.length paths