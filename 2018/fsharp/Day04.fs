namespace AdventOfCode.FSharp.Y2018

module Day04 =
    open System
    open AdventOfCode.FSharp.Util

    let parseLine line =
        match line with
        | Regex @"\[\d{4}-\d{2}-\d{2} (\d{2}):(\d{2})] (.+)" [ hour; minute; entryText ] ->
            (int hour), (int minute), entryText
        | _ -> failwithf "Invalid line: %s" line

    type EntryType =
        | ShiftId of int
        | Wake
        | Sleep

    let parseEntry (hour, minute, entryText) =
        let entryType =
            match entryText with
            | "wakes up" -> Wake

            | "falls asleep" -> Sleep
            | Regex "Guard #(\d+) begins shift" [ guardId ] -> ShiftId(int guardId)
            | _ -> failwithf "Invalid entry: %s" entryText

        entryType, minute

    let mapEntries entries =
        let mutable guardId = 0
        let mutable sleepStart = 0

        seq {
            for entryType, minutes in entries do
                match entryType with
                | ShiftId id -> guardId <- id
                | Sleep -> sleepStart <- minutes
                | Wake -> yield guardId, (sleepStart, minutes - sleepStart)
        }

    let findMaxMinute entries =
        let minutesArray = Array.zeroCreate 60

        entries
        |> Seq.map snd
        |> Seq.iter (fun (sleepStart, duration) ->
            let sleepEnd = (sleepStart + duration) - 1

            for index = sleepStart to sleepEnd do
                minutesArray[index] <- minutesArray[index] + 1)

        minutesArray |> Array.indexed |> Array.maxBy snd


    let run (input: byte array) output =
        let guardEntries =
            input
            |> text
            |> splitLine
            |> Seq.sort
            |> Seq.map (parseLine >> parseEntry)
            |> mapEntries
            |> Seq.groupBy fst
            |> Seq.map snd

        let maxGuardEntries =
            guardEntries
            |> Seq.maxBy (fun entries -> entries |> Seq.map (snd >> snd) |> Seq.sum)

        let guardId = maxGuardEntries |> Seq.head |> fst

        let maxMinute = findMaxMinute maxGuardEntries |> fst

        guardId * maxMinute |> string |> output 1

        let maxGuardEntries2 = guardEntries |> Seq.maxBy (findMaxMinute >> snd)

        let guardId = maxGuardEntries2 |> Seq.head |> fst
        let maxMinute = findMaxMinute maxGuardEntries2 |> fst

        guardId * maxMinute |> string |> output 2
