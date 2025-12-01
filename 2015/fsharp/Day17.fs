namespace AdventOfCode.FSharp.Y2015

// Day 17: No Such Thing as Too Much

module Day17 =
    open AdventOfCode.FSharp.Util

    let totalEggNog = 150

    let run (input: byte array) (output: int -> string -> unit) =
        let containers = input |> parseInts

        let indexes = [ 0 .. containers.Length - 1 ] |> Set.ofList

        let mutable q = [ (150, Set.empty) ]
        let mutable solutionCount = 0
        let mutable visited = Set.empty
        let mutable minSolutionContainers = System.Int32.MaxValue
        let mutable minSolutionCount = 0

        while q |> List.isEmpty |> not do
            match q with
            | [] -> failwith "unreachable"
            | (remaining, selected) :: nq ->
                q <- nq

                let validContainers =
                    selected
                    |> Set.difference indexes
                    |> Seq.filter (fun index -> containers[index] <= remaining)

                for index in validContainers do
                    let possible = selected |> Set.add index

                    if visited |> Set.contains possible |> not then
                        visited <- visited |> Set.add possible

                        let c = containers[index]

                        if c = remaining then
                            solutionCount <- solutionCount + 1

                            if possible.Count < minSolutionContainers then
                                minSolutionContainers <- possible.Count
                                minSolutionCount <- 1
                            elif possible.Count = minSolutionContainers then
                                minSolutionCount <- minSolutionCount + 1
                        else
                            q <- (remaining - c, possible) :: q

        solutionCount |> string |> output 1
        minSolutionCount |> string |> output 2
