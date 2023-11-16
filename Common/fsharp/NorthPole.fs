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

    type OutputThunk = int -> string -> unit

    type RunDayThunk = byte[] -> OutputThunk -> unit

    type RunDef = { run: RunDayThunk; name: string }

    type Day =
        { year: int
          day: int
          runs: RunDef list }

    type DayResult =
        { day: int
          name: string
          results: (string option * Result<unit, string> option)[]
          elapsedMs: float }

    type private BlackBox =
        [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
        static member public GetValue a = a

    let MaxOutputs = 10

    module private Impl =
        let NullOut = new StreamWriter(Stream.Null)

        let RequestLimit = TimeSpan.FromSeconds 30

        let getInputFolder (day: int) =
            let baseName = sprintf "%02i" day

            let relativePath = Path.Combine("..", "inputs", baseName)

            let path = Path.GetFullPath relativePath

            if Directory.Exists path |> not then
                Directory.CreateDirectory path |> ignore

            path

        let getInputPath (day: int) (inputType: InputType) =

            let filename =
                match inputType with
                | InputType.Default -> "input.txt"
                | InputType.Test -> "test.txt"
                | InputType.Alt -> "alt.txt"
                | e -> failwithf "Unsupported value: %A" e

            let inputFolder = getInputFolder day

            Path.Combine(inputFolder, filename)

        let getExpectedPath (day: int) (inputType: InputType) (part: int) =
            let inputPath = getInputPath day inputType

            Path.ChangeExtension(inputPath, sprintf ".s%i.txt" part)

        let readFile path =
            if File.Exists path then
                let data = File.ReadAllBytes path
                if data.Length > 0 then Some data else None
            else
                let dir = Path.GetDirectoryName path

                if not (Directory.Exists dir) then
                    Directory.CreateDirectory dir |> ignore

                let fs = File.Create path
                fs.Close()
                None

        let ensureRequestLimit () =
            let aocTrackerPath = Path.Combine(Path.GetTempPath(), "aoc.tracker")
            let now = DateTime.UtcNow

            if File.Exists aocTrackerPath then
                let lastRequestTime = File.GetLastWriteTimeUtc aocTrackerPath
                let waitTime = (lastRequestTime + RequestLimit) - now

                if waitTime > TimeSpan.Zero then
                    printfn "Sleeping for %3.2fs" waitTime.TotalSeconds
                    Threading.Thread.Sleep waitTime
            else
                (File.Create aocTrackerPath).Close()

            File.SetLastWriteTimeUtc(aocTrackerPath, DateTime.UtcNow)

        let getReleaseTime year day =
            new DateTime(year, 12, day, 5, 0, 0, DateTimeKind.Utc)

        let getSessionValue () =
            let mutable dir = Directory.GetCurrentDirectory()
            let mutable sessionValue = None

            while dir <> null && sessionValue = None do
                let sessionPath = Path.Combine(dir, ".adventofcode.session")

                if File.Exists sessionPath then
                    sessionValue <- Some(File.ReadAllText sessionPath)
                else
                    dir <- Path.GetDirectoryName dir

            if sessionValue = None then
                failwith "Could not find .adventofcode.session file"

            sessionValue.Value

        let downloadInput year day inputPath =
            let releaseDate = getReleaseTime year day

            if releaseDate > DateTime.UtcNow then
                printf "%A is in the future, no input file yet" releaseDate
                None
            else
                let session = getSessionValue ()

                ensureRequestLimit ()

                let client = new Net.Http.HttpClient()
                client.DefaultRequestHeaders.Clear()

                client.DefaultRequestHeaders.Add(
                    "User-Agent",
                    "github.com/0.1 jkingry/adventofcode by joe-at-kingry.ca"
                )

                client.DefaultRequestHeaders.Add("Cookie", sprintf "session=%s" session)

                let inputUrl = sprintf "https://adventofcode.com/%i/day/%i/input" year day |> Uri
                printfn "Downloading %O" inputUrl
                let task = client.GetByteArrayAsync inputUrl
                let data = task.Result
                printfn "Saving to %s" inputPath
                File.WriteAllBytes(inputPath, data)
                Some data

        let downloadExpected year day part (expectedPath: string) =
            let releaseDate = getReleaseTime year day

            if releaseDate > DateTime.UtcNow then
                printf "%A is in the future, no expected result yet" releaseDate
                None
            else
                let htmlCachePath = Path.Combine((Path.GetDirectoryName expectedPath), "day.html")

                let data =
                    if htmlCachePath |> File.Exists then
                        printfn "Using cached HTML file %s" htmlCachePath
                        File.ReadAllText htmlCachePath
                    else
                        let session = getSessionValue ()

                        ensureRequestLimit ()

                        let client = new Net.Http.HttpClient()
                        client.DefaultRequestHeaders.Clear()

                        client.DefaultRequestHeaders.Add(
                            "User-Agent",
                            "github.com/0.1 jkingry/adventofcode by joe-at-kingry.ca"
                        )

                        client.DefaultRequestHeaders.Add("Cookie", sprintf "session=%s" session)

                        let inputUrl = sprintf "https://adventofcode.com/%i/day/%i" year day |> Uri
                        printfn "Downloading %O" inputUrl
                        let task = client.GetStringAsync inputUrl
                        let htmlText = task.Result
                        printfn "Saving to %s" htmlCachePath
                        File.WriteAllText(htmlCachePath, htmlText)
                        htmlText

                let answerRegex =
                    System.Text.RegularExpressions.Regex "Your puzzle answer was <code>(.+?)</code>"

                let answers: string array =
                    answerRegex.Matches data |> Seq.map (fun m -> m.Groups[1].Value) |> Seq.toArray

                if answers.Length < part then
                    printfn "No answer for part %i" part
                    None
                else
                    printfn "Saving to %s" expectedPath
                    File.WriteAllText(expectedPath, answers.[part - 1])
                    answers.[part - 1] |> Some

        let getInput (year: int) (day: int) (inputType: InputType) =
            let path = getInputPath day inputType

            match readFile path with
            | Some data -> Some data
            | None when inputType = InputType.Default -> downloadInput year day path
            | None -> None

        let getExpected (year: int) (day: int) (inputType: InputType) (part: int) =
            let path = getExpectedPath day inputType part

            match readFile path with
            | Some data -> data |> text |> Some
            | None when inputType = InputType.Default -> downloadExpected year day part path
            | None -> None

        let runDay (d: Day) (inputType: InputType) (repeat: int) (silentOutput: bool) =
            let input =
                match getInput d.year d.day inputType with
                | Some s -> s
                | _ -> failwithf "No %A input exists for %d day %d" inputType d.year d.day

            let expected = Array.create MaxOutputs None
            expected[0] <- getExpected d.year d.day inputType 1
            expected[1] <- getExpected d.year d.day inputType 2

            let actualsToResults actuals =
                Array.zip actuals expected
                |> Array.map (function
                    | Some a, Some e when e <> a -> sprintf "Expected: '%s' Actual: '%s'" e a |> Error |> Some
                    | None, Some e -> sprintf "Expected: '%s', Actual: BLANK" e |> Error |> Some
                    | _, None -> None
                    | _ -> Ok() |> Some)
                |> Array.zip actuals

            let originalOut = Console.Out

            d.runs
            |> Seq.map (fun def ->
                let actuals = Array.create 10 None
                let name = def.name
                let thunk = def.run

                let output part result = actuals[part - 1] <- Some result
                let dummyOut part result = ()

                let w = Diagnostics.Stopwatch()

                try
                    if silentOutput then
                        Console.SetOut NullOut

                    if repeat > 1 then
                        thunk (BlackBox.GetValue input) dummyOut

                    w.Start()

                    for _ = 1 to repeat do
                        thunk (BlackBox.GetValue input) output

                    w.Stop()
                finally
                    if silentOutput then
                        Console.SetOut originalOut

                { day = d.day
                  name = name
                  results = actualsToResults actuals
                  elapsedMs = w.Elapsed.TotalMilliseconds })

        let tryParse (str: string) =
            match Int32.TryParse str with
            | true, int -> Some int
            | _ -> None

        let tryParseCommand (str: string) : Command option =
            match tryParse str with
            | Some _ -> None
            | None ->
                match Enum.TryParse(str, true) with
                | true, e when Enum.IsDefined(e) -> Some e
                | _ -> None

        let tryParseInputType (str: string) : InputType option =
            match Enum.TryParse(str, true) with
            | true, e when Enum.IsDefined(e) -> Some e
            | _ -> None

        let executeRun (dayIndexArg: int option) (inputType: InputType) (days: Day list) =
            let day =
                match dayIndexArg with
                | Some dayIndex -> days |> List.find (fun d -> d.day = dayIndex)
                | None -> days |> List.last

            printfn "%3s %8s %9s %4s %8s %s" "Day" "Method" "Time" "Part" "Status" "Value"

            let repeats = 1
            let silentOutput = false

            let resToStr (output, result) =
                let output = output |> Option.defaultValue "BLANK"

                let result =
                    match result with
                    | None -> "Unknown"
                    | Some(Ok _) -> "GOOD"
                    | Some(Error s) -> sprintf "ERROR: %s" s

                sprintf "%8s %s" result output

            for r in runDay day inputType repeats silentOutput do
                printfn "%3d %8s %9.3f %4d %s" r.day r.name r.elapsedMs 1 (resToStr r.results[0])
                printfn "%3d %8s %9s %4d %s" r.day "" "" 2 (resToStr r.results[1])

                for (p, pr) in r.results |> Array.indexed |> Array.skip 2 do
                    match pr with
                    | Some _, _ -> printfn "%3d %8s %4d %s" r.day "" (p + 1) (resToStr pr)
                    | _ -> ()

        let executeTest (dayIndexArg: int option) (repeats: int option) (days: Day list) =
            let days =
                match dayIndexArg with
                | Some dayIndex -> days |> List.find (fun d -> d.day = dayIndex) |> List.singleton
                | None -> days


            let silentOutput = true
            let repeats = repeats |> Option.defaultValue 1
            let mutable fastestTotalMs = 0.0
            let mutable slowestTotalMs = 0.0

            let mutable dayTimes = Map.empty

            printfn "By day:"
            printfn "%5s %9s %3s" "Day" "Time" "[S]"

            let resultsToState results =
                let (e, u) =
                    results
                    |> Array.take 2
                    |> Array.fold
                        (fun (e, u) (_, r) ->
                            match r with
                            | None -> (e, u + 1)
                            | Some(Error _) -> (e + 1, u)
                            | _ -> (e, u))
                        (0, 0)

                let es = if e > 0 then sprintf "E%i" e else ""
                let us = if u > 0 then sprintf "U%i" u else ""
                sprintf "%s%s" es us

            let multi slow fast =
                let factor = slow / fast

                if factor < 2.0 then
                    sprintf "%.2f%%" (100.0 * (factor - 1.0))
                else
                    sprintf "x%.2f" factor


            for day in days do
                let mutable fastestMs = Double.PositiveInfinity
                let mutable slowestMs = Double.NegativeInfinity

                for r in runDay day InputType.Default repeats silentOutput do
                    if fastestMs < Double.PositiveInfinity then
                        printfn
                            "%5d %9.3f %3s %s %s"
                            r.day
                            (r.elapsedMs / (float repeats))
                            (resultsToState r.results)
                            r.name
                            (multi fastestMs r.elapsedMs)
                    else
                        printfn
                            "%5d %9.3f %3s %s"
                            r.day
                            (r.elapsedMs / (float repeats))
                            (resultsToState r.results)
                            r.name

                    fastestMs <- min fastestMs r.elapsedMs
                    slowestMs <- max slowestMs r.elapsedMs

                dayTimes <- dayTimes |> Map.add day.day fastestMs
                fastestTotalMs <- fastestTotalMs + fastestMs
                slowestTotalMs <- slowestTotalMs + slowestMs

            printfn "%5s %9.3f" "Total" (slowestTotalMs / (float repeats))

            printfn "\nBy (fastest) time:"
            printfn "%5s %9s" "Day" "Time"

            for (day, time) in dayTimes |> Map.toList |> List.sortBy snd do
                printfn "%5d %9.3f" day (time / (float repeats))

            printfn "%5s %9.3f %s" "Total " (fastestTotalMs / (float repeats)) (multi slowestTotalMs fastestTotalMs)

    open Impl

    let runCommandLine (days: Day list) =
        let args = Environment.GetCommandLineArgs() |> Array.tail

        let mutable n = 0

        let parseArg (parseThunk: string -> 'a option) : 'a option =
            if args.Length <= n then
                None
            else
                match parseThunk args[n] with
                | Some v ->
                    n <- n + 1
                    Some v
                | None -> None

        let commandArg = parseArg tryParseCommand |> Option.defaultValue Command.Run

        let repeatsArg =
            if commandArg = Command.Test then
                parseArg tryParse
            else
                None

        let dayIndexArg = parseArg tryParse

        let inputTypeArg =
            parseArg tryParseInputType |> Option.defaultValue InputType.Default

        match commandArg with
        | Command.Run -> days |> executeRun dayIndexArg inputTypeArg
        | Command.Test -> days |> executeTest dayIndexArg repeatsArg
        | x -> failwithf "Unexpected command: %A" x

    open FSharp.Quotations.Evaluator
    open FSharp.Quotations

    let findDays () =
        let dayRegex = System.Text.RegularExpressions.Regex "Day([0-9]{2})"
        let yearRegex = System.Text.RegularExpressions.Regex "Y([0-9]{4})"

        let chooseValidYearDay (t: Type) =
            let yearMatch = yearRegex.Match t.FullName
            let dayMatch = dayRegex.Match t.Name

            if yearMatch.Success && dayMatch.Success then
                Some((int yearMatch.Groups[1].Value), (int dayMatch.Groups[1].Value))
            else
                None

        let isValidRunMethod (m: System.Reflection.MethodInfo) =
            let ps = m.GetParameters()

            if m.Name.StartsWith("run") && ps.Length = 2 then
                let firstIsByteArray = ps[0].ParameterType = typeof<byte array>
                let secondIsOutputAction = ps[1].ParameterType = typeof<(int -> string -> unit)>
                firstIsByteArray && secondIsOutputAction
            else
                false

        let chooseValidRuns (t: Type) =
            let validRunMethods = t.GetMethods() |> Array.filter isValidRunMethod

            if Array.isEmpty validRunMethods then
                None
            else
                Some validRunMethods

        let translateMethod (m: System.Reflection.MethodInfo) =
            let inputVar = Var("input", typeof<byte[]>)
            let inputVarExpr = Expr.Var inputVar
            let outputVar = Var("output", typeof<OutputThunk>)
            let outputVarExpr = Expr.Var outputVar

            let expr =
                Expr.Lambda(inputVar, Expr.Lambda(outputVar, Expr.Call(m, [ inputVarExpr; outputVarExpr ])))
                |> Expr.Cast<RunDayThunk>

            { run = QuotationEvaluator.Evaluate expr
              name = m.Name }

        let a = System.Reflection.Assembly.GetEntryAssembly()

        a.GetExportedTypes()
        |> Seq.choose (fun t -> chooseValidYearDay t |> Option.map (fun (year, day) -> (t, year, day)))
        |> Seq.choose (fun (t, year, day) -> chooseValidRuns t |> Option.map (fun runs -> (year, day, runs)))
        |> Seq.map (fun (year, day, runs) ->
            let typedRuns = runs |> Array.map translateMethod |> Array.toList

            { year = year
              day = day
              runs = typedRuns })
        |> List.ofSeq
        |> List.sortBy (fun d -> d.day)
