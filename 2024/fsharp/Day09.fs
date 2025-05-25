namespace AdventOfCode.FSharp.Y2024

// Day 9
module Day09 =
    open Checked
    open AdventOfCode.FSharp.Util


    let run (input: byte array) (output: int -> string -> unit) =
        let mutable checksum = 0L
        let mutable index = 0
        let mutable revIndex = (input.Length - 1) / 2 * 2
        let mutable revBlockSize = input[revIndex] - '0'B |> int

        let mutable sector = 0

        while index <= revIndex do
            let blockSize =
                if index = revIndex then
                    revBlockSize
                else
                    input[index] - '0'B |> int

            let sectorSum = [ sector .. sector + (blockSize - 1) ] |> List.sum |> int64
            let fileId = index / 2 |> int64
            //printfn "Leave: id:%i to sector %i blocks %i" fileId sector blockSize

            checksum <- checksum + (fileId * sectorSum)
            sector <- sector + blockSize

            index <- index + 1
            let mutable freespaceSize = input[index] - '0'B |> int

            while freespaceSize > 0 && index < revIndex do
                if revBlockSize = 0 then
                    revIndex <- revIndex - 2
                    revBlockSize <- input[revIndex] - '0'B |> int

                let movedSectors = min revBlockSize freespaceSize
                revBlockSize <- revBlockSize - movedSectors
                freespaceSize <- freespaceSize - movedSectors
                let sectorSum = [ sector .. sector + (movedSectors - 1) ] |> List.sum |> int64
                let fileId = revIndex / 2 |> int64
                //printfn "Copy: id:%i to sector %i blocks %i" fileId sector movedSectors
                checksum <- checksum + (fileId * sectorSum)
                sector <- sector + movedSectors

            index <- index + 1

        checksum |> string |> output 1
