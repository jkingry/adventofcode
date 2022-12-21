namespace AdventOfCode.FSharp.Y2022

// Day 16
module Day16 =
    open Checked
    open AdventOfCode.FSharp.Util


    let LineRegex =
        @"^Valve (..) has flow rate=(\d+); tunnel(?:(?: leads to valve (..))|(?:s lead to valves (.+)))$"

    type Valve = 
        {
            rate: int
            dests: string[]
        }

    let parse (input: byte[]) =
        input
        |> text
        |> splitLine
        |> Array.map (function
            | Regex LineRegex [ src; rate; dest; dests ] ->
                src, {
                    rate = int rate
                    dests = 
                        if dest.Length > 0 then
                            [| dest |]
                        else
                            (dests.Split ',' |> Array.map (fun s -> s.Trim()))

                    }
            | line -> failwithf "Failed parsing: %s" line)
        |> Map.ofArray

    let run (input: byte[]) (output: int -> string -> unit) =
        let valves = parse input
        let numValves = valves |> Map.values |> Seq.filter (fun v -> v.rate > 0) |> Seq.length
        let closedValves = valves |> Map.filter (fun _ v -> v.rate > 0) |> Map.keys |> Set.ofSeq

        let moves valve = 
            let v = valves |> Map.find valve
            v.dests |> Array.map (fun d -> (d, 1)) 

        let distances = 
            valves 
            |> Map.keys 
            |> Seq.map (fun src ->
                let dests = 
                    valves 
                    |> Map.keys 
                    |> Seq.map (fun dest -> 
                        let c, _ = 
                            DijkstraMap.empty
                            |> DijkstraMap.add src 0
                            |> DijkstraMap.run System.Int32.MaxValue moves (fun n -> n = dest)
                        dest, c[dest])
                src, (dests |> Map.ofSeq)) 
            |> Map.ofSeq

        let valveMoves (timeLeft, valve, cvalves) =
            let openPressure = 
                Set.difference closedValves cvalves
                |> Seq.map (fun o -> (valves |> Map.find o).rate)
                |> Seq.sum

            seq {                
                if timeLeft <= 0 then () 
                elif cvalves |> Set.isEmpty then 
                    yield (timeLeft - 1, valve, cvalves), openPressure
                else
                    let destDistances = distances |> Map.find valve

                    for dest in cvalves do
                        let dist = destDistances |> Map.find dest

                        let travelTimeAndValveChange = dist + 1
                        if travelTimeAndValveChange < timeLeft then
                            let pressureChange = openPressure * travelTimeAndValveChange                            
                            let newTime = timeLeft - travelTimeAndValveChange
                            let newValves = cvalves |> Set.remove dest

                            yield (newTime, dest, newValves), pressureChange
            }

        let h (timeLeft, _, cvalves) =
            let openPressure = 
                Set.difference closedValves cvalves
                |> Seq.map (fun o -> (valves |> Map.find o).rate)
                |> Seq.sum
            -(openPressure * timeLeft)

        let (costs, _) =
            DijkstraMap.empty
            |> DijkstraMap.add (30, "AA", closedValves) 0
            |> DijkstraMap.runAstar System.Int32.MaxValue valveMoves (fun (t, _, _) ->  t = 0) h
 
        costs |> Map.values |> Seq.map abs |> Seq.max |> string |> output 1
