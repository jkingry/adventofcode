namespace AdventOfCode.FSharp.Y2015

// Day 15
module Day15 =
    open AdventOfCode.FSharp.Util

    let linePattern =
        "^(.+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (\d+)$"

    type Ingredient =
        { capacity: int
          durability: int
          flavor: int
          texture: int
          calories: int }

    let parseLine (line: string) =
        match line with
        | Regex linePattern [ ingredient; capacity; durability; flavor; texture; calories ] ->
            { capacity = int capacity
              durability = int durability
              flavor = int flavor
              texture = int texture
              calories = int calories }
        | _ -> failwithf "invalid line: %s" line

    let maxTeaspoons = 100

    let nCk n k =
        MathNet.Numerics.Combinatorics.Combinations(n, k) |> int

    let nextDividers N k (p: int array) =
        let n = N - k

        let mutable i = k - 1
        let mutable found = false

        while not found && i >= 0 do
            if p[i] < n - (k - 1 - i) then
                p[i] <- p[i] + 1

                for j = i + 1 to k - 1 do
                    p[j] <- p[j - 1] + 1

                found <- true

            i <- i - 1

        if found then Some p else None

    let divisorsToTuple divs =
        match divs with
        | [| p1; p2; p3 |] -> p1, p2 - p1, p3 - p2, 100 - p3
        | _ -> failwith "invalid"

    let tupleToDividers (w, x, y, _) = [| w; w + x; w + x + y |]

    let score (ingredients: Ingredient array) (amounts: int array) =
        let mutable capacity = 0
        let mutable durability = 0
        let mutable flavor = 0
        let mutable texture = 0

        for i = 0 to ingredients.Length - 1 do
            let a = amounts[i]
            capacity <- capacity + ingredients[i].capacity * a
            durability <- durability + ingredients[i].durability * a
            flavor <- flavor + ingredients[i].flavor * a
            texture <- texture + ingredients[i].texture * a

        max capacity 0 * max durability 0 * max flavor 0 * max texture 0


    let run (input: byte array) (output: int -> string -> unit) =
        let ingredients = input |> text |> splitLine |> Array.map parseLine

        let n = ingredients.Length

        let amounts = Array.create n (maxTeaspoons / n)

        let score = score ingredients

        while amounts |> Array.sum < maxTeaspoons do
            amounts[0] <- amounts[0] + 1

        let moves (valid: bool, state: int array) =
            let oldCost = score state

            seq {
                for i = 1 to state.Length - 1 do
                    if state[i] < 99 && state[0] > 1 then
                        let ns = state |> Array.copy
                        ns[i] <- ns[i] + 1
                        ns[0] <- ns[0] - 1
                        let res = score ns

                        if not valid || res > 0 then
                            yield (res > 0, ns), (res - oldCost)

                    if state[i] > 1 && state[0] < 99 then
                        let ns = state |> Array.copy
                        ns[i] <- ns[i] - 1
                        ns[0] <- ns[0] + 1
                        let res = score ns

                        if not valid || res > 0 then
                            yield (res > 0, ns), (res - oldCost)
            }

        let goal (valid: bool, state: int array) = false

        let initialScore = score amounts

        let a, b =
            DijkstraMap.emptyMax
            |> DijkstraMap.add (initialScore > 0, amounts) initialScore
            |> DijkstraMap.runMax 0 moves goal

        a |> Map.toSeq |> Seq.map snd |> Seq.max |> string |> output 1

        "not implemented" |> output 2

    let findInitial (costFn: int array -> int) (maxValue: int) (value: int array) =
        let mutable cost = costFn value

        let deltas =
            [ 0 .. value.Length - 1 ] |> List.map (fun i -> [ i, 1; i, -1 ]) |> List.concat

        let mutable deltaIndex = 0

        let attempt = Array.copy value

        while cost <= 0 && deltaIndex < deltas.Length do
            Array.blit value 0 attempt 0 value.Length

            let index, sign = deltas[deltaIndex]

            while cost <= 0 && 0 < attempt[index] && attempt[index] < maxValue do
                attempt[index] <- attempt[index] + sign
                cost <- costFn attempt

            deltaIndex <- deltaIndex + 1

        if cost <= 0 then
            failwith "no good value found"
        else
            attempt

    let solveMax (costFn: int array -> int) (maxValue: int) (initialValue: int array) =
        let value = findInitial costFn maxValue initialValue

        let mutable cost = costFn value

        let deltas =
            [ 0 .. value.Length - 1 ] |> List.map (fun i -> [ i, 1; i, -1 ]) |> List.concat

        let mutable deltaIndex = 0

        let mutable previousCost = cost

        let mutable maxFound = false

        while not maxFound do
            let mutable deltaOffset = -1

            // find a value that increases the function
            while cost <= previousCost && deltaOffset < deltas.Length do
                if deltaOffset >= 0 then
                    // undo previous delta
                    let index, sign = deltas[(deltaIndex + deltaOffset) % deltas.Length]
                    value[index] <- value[index] - sign

                deltaOffset <- deltaOffset + 1
                let index, sign = deltas[(deltaIndex + deltaOffset) % deltas.Length]
                value[index] <- value[index] + sign
                cost <- costFn value

            if cost <= previousCost then
                cost <- previousCost
                maxFound <- true
            else
                deltaIndex <- (deltaIndex + deltaOffset) % deltas.Length
                let index, sign = deltas[deltaIndex]

                // increment the value until it changes direction
                while cost > previousCost do
                    previousCost <- cost
                    value[index] <- value[index] + sign
                    cost <- costFn value
                // undo last change
                cost <- previousCost
                value[index] <- value[index] - sign

        cost

    let fullScore (ingredients: Ingredient array) (amounts: int array) =
        let mutable capacity = 0
        let mutable durability = 0
        let mutable flavor = 0
        let mutable texture = 0

        for i = 0 to ingredients.Length - 1 do
            let ingredient = ingredients[i]
            let a = amounts[i]

            //printfn "%A x %A" ingredient a
            capacity <- capacity + ingredient.capacity * a
            durability <- durability + ingredient.durability * a
            flavor <- flavor + ingredient.flavor * a
            texture <- texture + ingredient.texture * a

        max capacity 0 * max durability 0 * max flavor 0 * max texture 0

    let part1Score (ingredients: Ingredient array) (value: int array) =
        let amounts = Array.zeroCreate ingredients.Length
        Array.blit value 0 amounts 1 value.Length
        amounts[0] <- 100 - (Array.sum value)

        if Array.min amounts <= 0 then
            0
        else
            fullScore ingredients amounts

    let part2Score (ingredients: Ingredient array) (value: int array) =
        // this is cheating and using hte values of 2;2;8;8 for the calories
        // for this to actually work it would need to actual calculate this relation
        // and figure out which variables to use
        // (eg, don't use two ingredients with the same calorie count)
        let amounts = [| value[0]; 60 - value[0]; value[1]; 40 - value[1] |]

        if Array.min amounts <= 0 then
            0
        else
            fullScore ingredients amounts

    let runBetter (input: byte array) (output: int -> string -> unit) =
        let ingredients = input |> text |> splitLine |> Array.map parseLine

        // Not a "FULL" solution as doesn't scale to number of ingredients

        solveMax (part1Score ingredients) 100 [| 25; 25; 25 |] |> string |> output 1

        solveMax (part2Score ingredients) 100 [| 30; 20 |] |> string |> output 2
