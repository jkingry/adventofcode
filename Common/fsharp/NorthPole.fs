namespace AdventOfCode.FSharp

module NorthPole =
    open System
    open System.IO
    open Util

    type Command = 
        | Run = 0
        | Test = 1

    type InputType =
        | Default = 0
        | Test = 1
        | Alt = 2

    type RunDayThunk = byte array -> (int -> string -> unit) -> unit

    type Day = { 
        day: int 
        runs: RunDayThunk list 
    }

    type DayResult = {
        day: int
        index: int
        results: Result<string, string> option[]
        elapsedMs: float
    }

    module Impl =
        let NullOut = new StreamWriter (Stream.Null)

        let RequestLimit = TimeSpan.FromSeconds 30

        let mutable Year = DateTime.Today.Year

        let getInputPath (day: int) (inputType: InputType) =
            let baseFilename = sprintf "%02i.txt" day
            let filename =
                match inputType with
                | InputType.Default -> baseFilename
                | InputType.Test -> Path.ChangeExtension(baseFilename, ".test.txt")
                | InputType.Alt -> Path.ChangeExtension(baseFilename, ".alt.txt")
                | e -> failwithf "Unsupported value: %A" e

            let relativePath = Path.Combine("..", "inputs", filename)

            Path.GetFullPath relativePath

        let getExpectedPath (day: int) (inputType: InputType) (part: int) =
            let inputPath = getInputPath day inputType
            
            Path.ChangeExtension (inputPath, sprintf ".s%i.txt" part)

        let readFile path =
            if File.Exists path then
                let data = File.ReadAllBytes path
                if data.Length > 0 then Some data
                else None
            else
                let dir = Path.GetDirectoryName path
                if not (Directory.Exists dir) then
                    Directory.CreateDirectory dir |> ignore

                let fs = File.Create path 
                fs.Close ()
                None

        let ensureRequestLimit () =
            let aocTrackerPath = Path.Combine (Path.GetTempPath (), "aoc.tracker")
            let now = DateTime.UtcNow

            if File.Exists aocTrackerPath then
                let lastRequestTime = File.GetLastWriteTimeUtc aocTrackerPath
                let waitTime = (lastRequestTime + RequestLimit) - now
                if waitTime > TimeSpan.Zero then
                    printfn "Sleeping for %3.2fs" waitTime.TotalSeconds 
                    Threading.Thread.Sleep waitTime
            else
                (File.Create aocTrackerPath).Close () 
            File.SetLastWriteTimeUtc (aocTrackerPath, DateTime.UtcNow)

        let getReleaseTime day =
            new DateTime(Year, 12, day, 5, 0, 0, DateTimeKind.Utc)

        let getSessionValue () =
            File.ReadAllText ".adventofcode.session"

        let downloadInput day inputPath =
            let releaseDate = getReleaseTime day
            if releaseDate > DateTime.UtcNow then
                printf "%A is in the future, no input file yet" releaseDate             
                None 
            else
                let session = getSessionValue ()

                ensureRequestLimit ()

                let client = new Net.Http.HttpClient ()
                client.DefaultRequestHeaders.Clear ()
                client.DefaultRequestHeaders.Add("User-Agent", "github.com/0.1 jkingry/adventofcode by joe-at-kingry.ca")
                client.DefaultRequestHeaders.Add("Cookie", sprintf "session=%s" session)

                let inputUrl = sprintf "https://adventofcode.com/%i/day/%i/input" Year day |> Uri
                printfn "Downloading %O" inputUrl
                let task = client.GetByteArrayAsync inputUrl
                let data = task.Result
                printfn "Saving to %s" inputPath
                File.WriteAllBytes (inputPath, data)
                Some data 

        let getInput (day: int) (inputType: InputType)  =
            let path = getInputPath day inputType

            match readFile path with
            | Some data -> Some data
            | None when inputType = InputType.Default -> downloadInput day path
            | None -> None 

        let getExpected (day: int) (inputType: InputType) (part: int)=
            let path = getExpectedPath day inputType part
            readFile path |> Option.map text

        let runDay (d: Day) (inputType: InputType) (repeat: int) (silentOutput: bool) =
            let input = getInput d.day inputType |> Option.get
            let expected = 
                [|
                    getExpected d.day inputType 1
                    getExpected d.day inputType 2
                    None
                |]

            let actualsToResults actuals =
                Array.zip actuals expected 
                |> Array.map (function 
                    | Some a, Some e when e <> a -> sprintf "Expected: '%s' Actual: '%s'" e a |> Error |> Some
                    | None, Some e -> sprintf "Expected: '%s', Actual: BLANK" e |> Error |> Some
                    | None, _ -> None
                    | Some a, _ -> Ok a |> Some)

            let originalOut = Console.Out

            d.runs
            |> List.indexed
            |> Seq.ofList
            |> Seq.map (fun (index, thunk) ->
                let actuals = Array.create 3 None

                let output part result = 
                    actuals[part - 1] <- Some result

                let w = Diagnostics.Stopwatch ()
                try            
                    if silentOutput then Console.SetOut NullOut
                    
                    w.Start ()

                    for _ = 1 to repeat do
                        thunk input output

                    w.Stop ()
                finally
                    if silentOutput then Console.SetOut originalOut

                { 
                    day = d.day
                    index = index
                    results = actualsToResults actuals
                    elapsedMs = w.Elapsed.TotalMilliseconds
                })
        
        let tryParse (str:string) =
            match Int32.TryParse str with
            | true,int -> Some int
            | _ -> None
        
        let tryParseCommand (str:string) : Command option =
            match tryParse str with
            | Some _ -> None
            | None ->
                match Enum.TryParse (str, true) with
                | true,e when Enum.IsDefined(e) -> Some e
                | _ -> None

        let tryParseInputType (str: string) : InputType option =
            match Enum.TryParse (str, true) with
            | true, e when Enum.IsDefined(e) -> Some e
            | _ -> None

        let executeRun (dayIndexArg: int option) (inputType: InputType) (days: Day list) =
            let day =
                match dayIndexArg with
                | Some dayIndex -> days |> List.find (fun d -> d.day = dayIndex)
                | None -> days |> List.last

            printfn "Day Time   Part Value"        
            
            let repeats = 1
            let silentOutput = false        
            for r in runDay day inputType repeats silentOutput do
                printfn "%3d %6.3f %4d %A [%d]" r.day r.elapsedMs 1 r.results[0].Value r.index
                for (p, pr) in r.results |> Array.indexed |> Array.tail do
                    match pr with
                    | Some v -> printfn "%3d %6s %4d %A" r.day "" (p+1) v 
                    | None -> ()

        let executeTest (dayIndexArg: int option) (repeats: int option) (days: Day list) =
            let days =
                match dayIndexArg with
                | Some dayIndex -> days |>List.find (fun d -> d.day = dayIndex) |> List.singleton
                | None -> days

            printfn "%3s %8s" "Day" "Time"

            let silentOutput = true
            let repeats = repeats |> Option.defaultValue 1
            let mutable totalMs = 0.0
            for day in days do
                for r in runDay day InputType.Default repeats silentOutput do
                    printfn "%3d %8.3f [%d]" r.day (r.elapsedMs / (float repeats)) r.index
                    totalMs <- totalMs + r.elapsedMs
            
            printfn "%3s %8.3f" "" (totalMs / (float repeats))

    open Impl
    
    let runCommandLine (days : Day list) =
        let args = Environment.GetCommandLineArgs() |> Array.tail

        let mutable n = 0

        let parseArg (parseThunk: string -> 'a option) : 'a option =
            if args.Length <= n then None
            else 
                match parseThunk args[n] with
                | Some v -> 
                    n <- n + 1
                    Some v
                | None -> None

        let commandArg = parseArg tryParseCommand |> Option.defaultValue Command.Run

        let repeatsArg = if commandArg = Command.Test then parseArg tryParse else None
        
        let dayIndexArg = parseArg tryParse

        let inputTypeArg = parseArg tryParseInputType |> Option.defaultValue InputType.Default

        match commandArg with 
        | Command.Run -> days |> executeRun dayIndexArg inputTypeArg
        | Command.Test -> days |> executeTest dayIndexArg repeatsArg
        | x -> failwithf "Unexpected command: %A" x