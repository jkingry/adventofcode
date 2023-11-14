namespace AdventOfCode.FSharp.Y2022

// Day 16
module Day16 =
    open Checked
    open AdventOfCode.FSharp.Util
    open FSharpx.Collections

    let LineRegex =
        @"^Valve (..) has flow rate=(\d+); tunnel(?:(?: leads to valve (..))|(?:s lead to valves (.+)))$"

    type Valve = { rate: int; dests: string[] }

    let parse (input: byte[]) =
        input
        |> text
        |> splitLine
        |> Array.map (function
            | Regex LineRegex [ src; rate; dest; dests ] ->
                src,
                { rate = int rate
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

        let nonZeroValves =
            valves
            |> Map.toSeq
            |> Seq.choose (fun (k, v) -> if v.rate > 0 then k |> Some else None)
            |> Set.ofSeq

        let calculateDistances (adj: string -> seq<string>) (src: string) =
            let mutable q = Deque.empty |> Deque.conj src
            let mutable distances = Map.empty |> Map.add src 0

            while q |> Deque.isEmpty |> not do
                let (v, nq) = q |> Deque.uncons

                q <- nq

                let dv = distances |> Map.find v

                for w in adj v do
                    if distances |> Map.containsKey w then
                        ()
                    else

                        distances <- distances |> Map.add w (dv + 1)

                        q <- q |> Deque.conj w

            distances

        let valveAdj v =
            (valves |> Map.find v).dests |> Array.toSeq

        let distances = valves |> Map.map (fun k _ -> k |> calculateDistances valveAdj)

        let mutable maxPressure = 0
        let mutable maxPressureState = None

        let mutable q = Deque.empty |> Deque.conj (0, (30, "AA", nonZeroValves, 0))

        let mutable parents = Map.empty

        while q |> Deque.isEmpty |> not do
            let ((pressure, state), nq) = q |> Deque.uncons

            q <- nq

            let (timeLeft, currentValve, closedValves, openValvePressure) = state

            if pressure > maxPressure then
                maxPressure <- pressure
                maxPressureState <- state |> Some

            if timeLeft = 0 then
                ()
            else

                let newPressure = pressure + (timeLeft * openValvePressure)
                let newState = (0, currentValve, Set.empty, openValvePressure)
                parents <- parents |> Map.add newState state
                q <- q |> Deque.conj (newPressure, newState)

                let currentDistances = distances |> Map.find currentValve

                for closedValve in closedValves do
                    let closedValveDistance = currentDistances |> Map.find closedValve

                    let operationTime = closedValveDistance + 1

                    if timeLeft > operationTime then
                        // open valve

                        let newClosedValves = closedValves |> Set.remove closedValve
                        let newOpenValvePressure = (valves |> Map.find closedValve).rate + openValvePressure
                        let newPressure = pressure + (operationTime * openValvePressure)

                        let newState =
                            timeLeft - operationTime, closedValve, newClosedValves, newOpenValvePressure

                        // printfn "Move to %A = %A = %A" closedValve newState newPressure
                        parents <- parents |> Map.add newState state
                        q <- q |> Deque.conj (newPressure, newState)

        let path =
            maxPressureState
            |> List.unfold (fun sv ->
                match sv with
                | Some s -> (s, (parents |> Map.tryFind s)) |> Some
                | None -> None)
            |> List.rev
        // for k,v in distances |> Map.toSeq do
        //     printfn "%A = %A" k v

        // for p in path do
        //     printfn "%A" p

        maxPressure |> string |> output 1


    let runOld (input: byte[]) (output: int -> string -> unit) =
        let valves = parse input

        let numValves =
            valves |> Map.values |> Seq.filter (fun v -> v.rate > 0) |> Seq.length

        let closedValves =
            valves |> Map.filter (fun _ v -> v.rate > 0) |> Map.keys |> Set.ofSeq

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
                if timeLeft <= 0 then
                    ()
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
            |> DijkstraMap.runAstar System.Int32.MaxValue valveMoves (fun (t, _, _) -> t = 0) h

        costs |> Map.values |> Seq.map abs |> Seq.max |> string |> output 1
