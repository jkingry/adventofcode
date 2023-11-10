namespace AdventOfCode.FSharp.Y2021

// Day 12: Passage Pathing
module Day12 =
    open AdventOfCode.FSharp.Util

    type graph<'k when 'k: comparison> = Map<'k, Set<'k>>

    module Graph =
        let empty: graph<'k> = Map.empty

        let add (source: 'k) (destination: 'k) (g: graph<'k>) : graph<'k> =
            g
            |> Map.change source (fun v ->
                (match v with
                 | Some s -> s
                 | _ -> Set.empty)
                |> Set.add destination
                |> Some)

        let fromList (input: ('k * 'k) list) : graph<'k> =
            input |> List.fold (fun g (src, dest) -> g |> add src dest) empty

        let filterEdges (fn: 'k -> bool) (graph: graph<'k>) : graph<'k> =
            graph |> Map.map (fun _ v -> v |> Set.filter fn)

        let filterNodes (fn: 'k -> bool) (graph: graph<'k>) : graph<'k> = graph |> Map.filter (fun k _ -> fn k)

        let link (g: graph<'k>) : graph<'k> =
            g
            |> Map.toSeq
            |> Seq.collect (fun (k, v) -> v |> Seq.map (fun e -> k, e))
            |> Seq.fold (fun g' (src, dest) -> add dest src g') g

        let rec findPaths
            (path: 'k list)
            (dest: 'k)
            (visitor: 'State -> 'k -> 'State option)
            (state: 'State)
            (g: graph<'k>)
            : 'k list seq =
            seq {
                match path with
                | x :: _ when x = dest -> yield path
                | x :: _ ->
                    for n in g.[x] do
                        match visitor state n with
                        | Some nextState -> yield! findPaths (n :: path) dest visitor nextState g
                        | _ -> ()
                | _ -> failwith "unreachable"
            }

    let parse text : graph<string> =
        text
        |> splitLine
        |> Array.map (fun line -> let p = line.Split('-') in p.[0], p.[1])
        |> Array.toList
        |> Graph.fromList
        |> Graph.link
        |> Graph.filterEdges (fun e -> e <> "start")
        |> Graph.filterNodes (fun n -> n <> "end")

    let inline small (k: string) = k.ToLower() = k

    let inline part1Visitor visited node =
        if not (small node) then Some visited
        elif visited |> Set.contains node then None
        else Some(visited |> Set.add node)

    let part1 (text: string) =
        let caves = parse text

        caves
        |> Graph.findPaths [ "start" ] "end" part1Visitor Set.empty
        |> Seq.length
        |> string

    let inline part2Visitor (visited, doubleCave) node =
        match part1Visitor visited node, doubleCave with
        | Some state, _ -> Some(state, doubleCave)
        | None, false -> Some(visited, true)
        | _ -> None

    let part2 (text: string) =
        let caves = parse text

        caves
        |> Graph.findPaths [ "start" ] "end" part2Visitor (Set.empty, false)
        |> Seq.length
        |> string

    let run (input: byte array) output =
        let textInput = text input
        part1 textInput |> output 1
        part2 textInput |> output 2
