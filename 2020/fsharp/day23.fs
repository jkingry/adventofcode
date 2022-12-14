namespace AdventOfCode.FSharp.Y2020

// Crab Cups
module Day23 =
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic

    type Node =
        { mutable prev: Node option
          v: int
          mutable next: Node option }

    let cs (c: int) (cups: Map<int, Node>) =
        seq {
            let mutable p = c

            while true do
                yield p
                p <- cups.[p].next.Value.v
        }

    let move (c: int) (cups: Map<int, Node>) (n: int) =
        let r = (cs c cups) |> Seq.skip 1 |> Seq.take 3 |> Seq.toArray

        cups.[r.[0]].prev.Value.next <- cups.[r.[2]].next
        cups.[r.[2]].next.Value.prev <- cups.[r.[0]].prev
        let mutable d = c - 1

        while (Array.contains d r) || d < 1 do
            if d < 1 then d <- n else d <- d - 1

        let before = cups.[d]
        let after = cups.[d].next.Value
        before.next <- Some cups.[r.[0]]
        cups.[r.[0]].prev <- Some before
        cups.[r.[2]].next <- Some after
        after.prev <- Some cups.[r.[2]]
        cups.[c].next.Value.v


    let part1 (input: string) =
        let init =
            input.Trim()
            |> Seq.map (fun c -> c.ToString() |> int)
            |> Seq.map (fun n -> { prev = None; v = n; next = None })
            |> Seq.toArray

        let n = init.Length

        init
        |> Array.pairwise
        |> Array.iter (fun (a, b) ->
            a.next <- Some b
            b.prev <- Some a)

        init.[0].prev <- Some init.[n - 1]
        init.[n - 1].next <- Some init.[0]

        let cups = init |> Array.map (fun c -> (c.v, c)) |> Map.ofArray

        let mutable c = init.[0].v

        for i = 1 to 100 do
            c <- move c cups n

        let ss =
            cs 1 cups
            |> Seq.skip 1
            |> Seq.take (n - 1)
            |> Seq.map (fun x -> x.ToString())
            |> Seq.toArray

        String.Join("", ss)

    let part2 (input: string) =
        let nn = input.Trim() |> Seq.map (fun c -> c.ToString() |> int)

        let init =
            Seq.append nn [ 10..1000000 ]
            |> Seq.map (fun n -> { prev = None; v = n; next = None })
            |> Seq.toArray

        let n = init.Length

        init
        |> Array.pairwise
        |> Array.iter (fun (a, b) ->
            a.next <- Some b
            b.prev <- Some a)

        init.[0].prev <- Some init.[n - 1]
        init.[n - 1].next <- Some init.[0]

        let cups = init |> Array.map (fun c -> (c.v, c)) |> Map.ofArray

        let mutable c = init.[0].v

        for i = 1 to 10000000 do
            c <- move c cups n

        let [ a; b ] =
            cs 1 cups |> Seq.skip 1 |> Seq.take (2) |> Seq.map int64 |> Seq.toList

        a * b |> string
