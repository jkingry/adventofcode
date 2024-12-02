namespace AdventOfCode.FSharp.Y2022

// Day 11: Monkey in the Middle
module Day11 =
    open AdventOfCode.FSharp.Util

    type Monkey =
        { mutable items: int list
          mutable inspections: int
          operation: int64 -> int64
          test: int
          destTrue: int
          destFalse: int }

    let run (input: byte[]) (output: int -> string -> unit) =
        let monkeys =
            input
            |> text
            |> splitDoubleLine
            |> Array.map (fun block ->
                let line = block |> splitLine
                let startingItems = line[1].Split(':').[1] |> ints

                let optext = (line[2].Split('=')[1]).Split(' ')

                let op =
                    if optext[2] = "+" then
                        if optext[3] = "old" then
                            fun x -> x + x
                        else
                            let operand = int optext[3] |> int64
                            fun x -> x + operand
                    elif optext[2] = "*" then
                        if optext[3] = "old" then
                            fun x -> x * x
                        else
                            let operand = int optext[3] |> int64
                            fun x -> x * operand
                    else
                        failwithf "Unexpected op: %s" optext[1]

                let test = line[3].Split(' ') |> Array.last |> int
                let destTrue = line[4].Split(' ') |> Array.last |> int
                let destFalse = line[5].Split(' ') |> Array.last |> int

                { items = startingItems |> List.ofArray
                  inspections = 0
                  operation = op
                  test = test
                  destTrue = destTrue
                  destFalse = destFalse })

        let product a =
            a |> Array.map int64 |> Array.reduce (fun a b -> a * b)

        let maxWorry = monkeys |> Array.map (fun m -> m.test) |> product

        let turn short i (monkeys: Monkey[]) =
            let monkey = monkeys[i]
            // printfn "Monkey %i:" i
            for item in monkey.items |> List.rev do
                monkey.inspections <- monkey.inspections + 1
                // printfn "  Monkey inspects an items with a worry level of %i." item
                let newWorry = monkey.operation item
                // printfn "    Worry level is **** to %i." newWorry
                let newWorry = (if short then newWorry / 3L else (newWorry % maxWorry)) |> int
                // printfn "    Monkey gets bored with item. Worry level is divided by 3 to %i." newWorry
                let testRes = (newWorry % monkey.test) = 0
                let dest = if testRes then monkey.destTrue else monkey.destFalse
                // printfn "    Current worry level is %sdivisible by %i." (if testRes then "" else "not ") monkey.test
                // printfn "    Item with worry level %i is thrown to monkey %i." newWorry dest
                monkeys[dest].items <- newWorry :: monkeys[dest].items

            monkey.items <- []

        let round short (monkeys: Monkey[]) =
            for i = 0 to (monkeys.Length - 1) do
                turn short i monkeys

        let shortMonkeys =
            monkeys |> Array.map (fun m -> { m with items = m.items |> List.ofSeq })

        for _ = 1 to 20 do
            round true shortMonkeys

        shortMonkeys
        |> Array.map (fun s -> s.inspections)
        |> Array.sortDescending
        |> Array.take 2
        |> product
        |> string
        |> output 1

        for _ = 1 to 10000 do
            round false monkeys

        monkeys
        |> Array.map (fun s -> s.inspections)
        |> Array.sortDescending
        |> Array.take 2
        |> product
        |> string
        |> output 2

    type FMonkey =
        { mutable count: int
          items: int[]
          mutable inspections: int
          operation: int64 -> int64
          test: int
          destTrue: int
          destFalse: int }

    // use arrays and counts to track items
    let runFast (input: byte[]) (output: int -> string -> unit) =
        let monkeys =
            input
            |> text
            |> splitDoubleLine
            |> Array.map (fun block ->
                let line = block |> splitLine
                let startingItems = line[1].Split(':').[1] |> ints

                let optext = (line[2].Split('=')[1]).Split(' ')

                let op =
                    if optext[2] = "+" then
                        if optext[3] = "old" then
                            fun x -> x + x
                        else
                            let operand = int optext[3] |> int64
                            fun x -> x + operand
                    elif optext[2] = "*" then
                        if optext[3] = "old" then
                            fun x -> x * x
                        else
                            let operand = int optext[3] |> int64
                            fun x -> x * operand
                    else
                        failwithf "Unexpected op: %s" optext[1]

                let test = line[3].Split(' ') |> Array.last |> int
                let destTrue = line[4].Split(' ') |> Array.last |> int
                let destFalse = line[5].Split(' ') |> Array.last |> int

                { count = startingItems.Length
                  items = startingItems
                  inspections = 0
                  operation = op
                  test = test
                  destTrue = destTrue
                  destFalse = destFalse })

        let product a =
            a |> Array.map int64 |> Array.reduce (fun a b -> a * b)

        let maxItems = monkeys |> Array.map (fun m -> m.items.Length) |> Array.max

        let monkeys =
            monkeys
            |> Array.map (fun m ->
                let newItems = Array.zeroCreate (maxItems * 3)
                Array.blit m.items 0 newItems 0 m.count
                { m with items = newItems })

        let maxWorry = monkeys |> Array.map (fun m -> m.test) |> product

        let turn short i (monkeys: FMonkey[]) =
            let monkey = monkeys[i]

            for j = 0 to (monkey.count - 1) do
                monkey.inspections <- monkey.inspections + 1
                let worry = monkey.items[j]
                let newWorry = monkey.operation worry
                let newWorry = (if short then newWorry / 3L else (newWorry % maxWorry)) |> int
                let testRes = (newWorry % monkey.test) = 0
                let dest = if testRes then monkey.destTrue else monkey.destFalse
                let destCount = monkeys[dest].count
                monkeys[dest].items[ destCount ] <- newWorry
                monkeys[dest].count <- destCount + 1

            monkey.count <- 0

        let round short (monkeys: FMonkey[]) =
            for i = 0 to (monkeys.Length - 1) do
                turn short i monkeys

        let shortMonkeys =
            monkeys |> Array.map (fun m -> { m with items = m.items |> Array.copy })

        for _ = 1 to 20 do
            round true shortMonkeys

        shortMonkeys
        |> Array.map (fun s -> s.inspections)
        |> Array.sortDescending
        |> Array.take 2
        |> product
        |> string
        |> output 1

        for _ = 1 to 10000 do
            round false monkeys

        monkeys
        |> Array.map (fun s -> s.inspections)
        |> Array.sortDescending
        |> Array.take 2
        |> product
        |> string
        |> output 2
