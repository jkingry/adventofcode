namespace AdventOfCode.FSharp.Y2015

// Day N
module Day09 =
    open AdventOfCode.FSharp.Util

    let parseLine =
        function
        | Regex "(\w+) to (\w+) = (\d+)" [ src; dst; distance ] -> src, dst, int distance
        | x -> failwithf "invalid line: %s" x

    let MAX_COUNT = 20

    let createDistances (entries: (string * string * int) seq) =
        let mutable names = Map.empty
        let distances = Array2D.zeroCreate MAX_COUNT MAX_COUNT

        let getNameIndex n =
            match names |> Map.tryFind n with
            | Some index -> index
            | None ->
                let index = names.Count
                names <- names |> Map.add n index
                index

        for src, dst, distance in entries do
            let srcIndex = getNameIndex src
            let dstIndex = getNameIndex dst

            distances[srcIndex, dstIndex] <- distance
            distances[dstIndex, srcIndex] <- distance

        // trim distances
        let n = names.Count

        // Add a "0" node that is zero cost to all points so any starting point can be picked
        let finalDistances = Array2D.zeroCreate (n + 1) (n + 1)
        Array2D.blit distances 0 0 finalDistances 1 1 n n

        finalDistances

    /// <summary>Solve the Traveling Salesman Problem using the Held–Karp algorithm.</summary>
    /// <param name="distances">a 2D square matrix of pairwise distances.</param>
    /// <returns>(min_cost, tour) where tour is a list of node indices, starting
    /// and ending at 0.</returns>
    let heldKarp (worstCost: int) (costFn: int -> int -> bool) (distances: int[,]) =
        let n = Array2D.length1 distances
        // Number of subsets: 2^n
        let subsetCount = 1 <<< n

        // dp[mask,j] = minimum cost to reach subset 'mask' and end at node j
        let dp = Array2D.create subsetCount n worstCost
        // parent[mask,j] = previous node before j in optimal path for (mask, j)
        // let parents = Array2D.create subsetCount (n - 1)

        // Base case: start at node 0, mask = 1<<0
        dp[1, 0] <- 0

        // Iterate over all subsets that include node 0
        for mask = 1 to subsetCount do
            // we always require the tour to start at node 0
            if mask &&& 1 <> 0 then
                for j = 1 to n - 1 do
                    if mask &&& (1 <<< j) <> 0 then
                        let previousMask = mask ^^^ (1 <<< j)

                        // Try all possibilities of coming to j from some k in previousMask
                        for k = 0 to n - 1 do
                            if previousMask &&& (1 <<< k) <> 0 then
                                let cost = dp[previousMask, k] + distances[k, j]

                                if costFn cost dp[mask, j] then
                                    dp[mask, j] <- cost
        // parents[mask, j] <- k

        // Close the tour: return to node 0
        let fullMask = (1 <<< n) - 1
        let mutable optimalCost = worstCost
        // let mutable lastCity = 0

        for j = 1 to n - 1 do
            let cost = dp[fullMask, j] + distances[j, 0]

            if costFn cost optimalCost then
                optimalCost <- cost

        // lastCity <- j

        // let mutable tour = [ 0 ]
        // let mutable currentMask = fullMask
        // let mutable currentCity = lastCity

        // while currentCity <> 0 do
        //     tour <- currentCity :: tour
        //     let prevCity = parents[currentMask, currentCity]
        //     currentMask <- currentMask ^^^ (1 <<< currentCity)
        //     currentCity <- prevCity

        // minCost, tour
        optimalCost

    let heldKarpMin = heldKarp System.Int32.MaxValue (fun a b -> a < b)

    let heldKarpMax = heldKarp 0 (fun a b -> a > b)

    let run (input: byte array) (output: int -> string -> unit) =
        let distances = input |> text |> splitLine |> Array.map parseLine |> createDistances

        heldKarpMin distances |> string |> output 1

        heldKarpMax distances |> string |> output 2
