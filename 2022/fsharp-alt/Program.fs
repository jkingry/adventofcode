open System

let newLine = byte '\n'

module Day01 =
    let zero = byte '0' 
    let newLine = byte '\n'

    let inline parseInt (s: ReadOnlySpan<byte>) =
        let mutable n = 0
        for c in s do
            n <- n * 10 + int (c - zero)  
        n

    let run () =
        let input = System.IO.File.ReadAllBytes("../input/01.txt")

        let s = new ReadOnlySpan<byte>(input)

        let mutable maxSum = 0
        let mutable sum = 0
        let mutable p = 0

        let top3 = Array.zeroCreate 3

        for i in 0 .. s.Length - 1 do
            if s[i] = newLine then
                if i > p then
                    sum <- sum + parseInt (s.Slice(p, i - p))
                else
                    for j in 0..2 do
                        if sum > top3[j] then
                            top3[j] <- sum
                            sum <- 0
                    sum <- 0
                p <- i + 1

        printfn "Day 01 - Part 1: %i" top3[0]
        printfn "Day 01 - Part 2: %i" (top3[0] + top3[1] + top3[2])

        input.Length

module Day02 =
    let run () =
        let input = System.IO.File.ReadAllBytes("../input/02.txt")

        let s = new ReadOnlySpan<byte>(input)

        let mutable part1 = 0
        let mutable part2 = 0

        let charA = byte 'A'
        let charX = byte 'X'

        let newLine = byte '\n'
        let mutable lastLinePosition = 0
        for i in 0 .. s.Length - 1 do
            if (i - lastLinePosition) = 2 then
                lastLinePosition <- lastLinePosition + 4
                let them = int (s[i-2] - charA)
                let you = int (s[i] - charX)
                part1 <- part1 + 
                    if you = them then 1 + 3 + you
                    elif you = (them + 1) % 3 then 1 + 6 + you
                    else 1 + 0 + you
                part2 <- part2 + 
                    if you = 0 then 1 + 0 + ((them + 2) % 3)
                    elif you = 1 then 1 + 3 + them
                    else 1 + 6 + ((them + 1) % 3)   

        printfn "Day 02 - Part 1: %i" part1
        printfn "Day 02 - Part 2: %i" part2

        input.Length

let mutable inputLen = 0L
let mutable before = 0L
let mutable after = 0L

before <- System.GC.GetAllocatedBytesForCurrentThread()
inputLen <- Day01.run () |> int64
after <- System.GC.GetAllocatedBytesForCurrentThread()
printfn "69042 %i" ((after - before) - inputLen)

before <- System.GC.GetAllocatedBytesForCurrentThread()
inputLen <- Day02.run () |> int64
after <- System.GC.GetAllocatedBytesForCurrentThread()
printfn "6361 %i" ((after - before) - inputLen)

