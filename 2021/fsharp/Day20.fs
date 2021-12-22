namespace AdventOfCode.FSharp.Y2021

// Day 20
module Day20 =
    open AdventOfCode.FSharp.Util
    open System.Collections

    let cellsize = sizeof<int> * 8

    let getBit i (data: int[]) =
        let bytepos = i / cellsize
        let bitmask = 1 <<< ((i % cellsize)-1)
        let bit = data[bytepos] &&& bitmask
        bit = bitmask

    let setBit i (data: int[]) =
        let bytepos = i / cellsize
        let bitmask = 1 <<< ((i % cellsize)-1)
        data[bytepos] <- data[bytepos] ||| bitmask

    let unsetBit i (data: int[]) =
        let bytepos = i / cellsize
        let bitmask = ~~~(1 <<< ((i % cellsize)-1))
        data[bytepos] <- data[bytepos] &&& bitmask

    let parseBits (input: string) =
        let algo = new BitArray(input.Length)
        input |> Seq.iteri (fun i c -> if c = '#' then algo.Set(i, true))
        algo 

    let get3x3 xi yi defaultPixel (image: BitArray[]) =
        let h = Array.length image
        let w = image[0].Length

        let mutable n = 0
        for y = yi - 1 to yi + 1 do
            for x = xi - 1 to xi + 1 do
                n <- n <<< 1
                if (if y >= 0 && y < h && x >= 0 && x < w then image[y][x] else defaultPixel) then 
                    n <- n + 1
        n

    let createImage sz =
        Array.init sz (fun _ -> new BitArray(sz))
            
    let transform (algo: BitArray) defaultPixel image =
        let oldSz = (Array.length image)
        let newSz = oldSz + 4
        let imageNext = createImage newSz

        for y = 0 to newSz - 1 do
            let row = imageNext[y]
            for x = 0 to newSz - 1 do
                let ox = x - 2
                let oy = y - 2
                let n = get3x3 ox oy defaultPixel image
                row[x] <- algo[n]
        let nextDefaultPixel = 
            match defaultPixel, algo[0], algo[algo.Count - 1] with
            | true, _, true -> true
            | true, _, _ -> false
            | false, false, _ -> false
            | false, _, _ -> true

        nextDefaultPixel, imageNext

    // https://stackoverflow.com/a/14354311/3046
    let cardinality (bits:BitArray) =
        let intsLength = (bits.Count >>> 5) + 1
        let ints = Array.zeroCreate intsLength
        bits.CopyTo(ints, 0)
        let mutable count = 0
        ints[intsLength - 1] <- ints[intsLength - 1]  &&& ~~~(-1 <<< (bits.Count % 32))
        
        for i = 0 to (intsLength - 1) do
            let mutable c = ints[i]
            // magic (http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel)
            c <- c - ((c >>> 1) &&& 0x55555555)
            c <- (c &&& 0x33333333) + ((c >>> 2) &&& 0x33333333)
            c <- ((c + (c >>> 4) &&& 0xF0F0F0F) * 0x1010101) >>> 24

            count <- count + c
        count 

    let imageDarkCount = Array.sumBy cardinality

    let printImage (image: BitArray[]) =
        for row in image do
            for c in row do
                printf "%c" (if c then '#' else '.')
            printfn ""
    
    let bitCount (image: BitArray[]) = 
        image[0]

    let run (input: string) (output: int -> string -> unit) =
        let sections = input |> splitDoubleLine

        let algo = sections[0] |> parseBits

        let mutable defaultPixel = false
        let mutable image = 
            sections[1]
            |> splitLine
            |> Array.map parseBits

        for i = 1 to 2 do
            let (newDefaultPixel, newImage) = transform algo defaultPixel image
            image <- newImage
            defaultPixel <- newDefaultPixel

        image |> imageDarkCount |> string |> output 1

        for i = 1 to 48 do
            let (newDefaultPixel, newImage) = transform algo defaultPixel image
            image <- newImage
            defaultPixel <- newDefaultPixel

        image |> imageDarkCount |> string |> output 2
