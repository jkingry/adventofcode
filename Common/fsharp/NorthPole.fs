namespace AdventOfCode.FSharp

module NorthPole =
    open System
    open System.IO

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

    let mutable executingYearDay: (int * int) option = None

    type Day =
        { year: int
          day: int
          runs: RunDef list }

    type DayResult =
        { year: int
          day: int
          name: string
          results: (string option * Result<unit, string> option)[]
          elapsedMs: float }

    type private BlackBox =
        [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
        static member public GetValue a = a

    let MaxOutputs = 10

    module internal Impl =
        open Spectre.Console
        let NullOut = new StreamWriter(Stream.Null)

        let RequestLimit = TimeSpan.FromSeconds 30

        let rec getDirectoryParents dir =
            seq {
                let mutable cd = dir

                while cd <> null do
                    yield cd
                    cd <- Path.GetDirectoryName cd
            }

        let sessionValuePath =
            lazy
                (Directory.GetCurrentDirectory()
                 |> getDirectoryParents
                 |> Seq.tryPick (fun dir ->
                     let sessionPath = Path.Combine(dir, ".adventofcode.session")

                     if File.Exists sessionPath then
                         sessionPath |> Some
                     else
                         None)
                 |> Option.defaultWith (fun () -> failwith "Could not find .adventofcode.session file"))

        let sessionValue = lazy (sessionValuePath.Force() |> File.ReadAllText)

        let getInputFolder (year: int) (day: int) =
            let possibleSuffixes =
                [ $"%04i{year}/inputs/%02i{day}"; $"inputs/%04i{year}/%02i{day}" ]

            let suffixParts = possibleSuffixes |> List.map (fun s -> s.Split('/'))
            let maxSuffixLen = suffixParts |> List.map Array.length |> List.max

            let findCreateDir dir i (suffixParts: string[]) =
                if i >= suffixParts.Length then
                    None
                else
                    let find, missing = suffixParts |> Array.splitAt i
                    let path = Array.append [| dir |] find |> Path.Combine

                    if Path.Exists path then
                        printfn "find=%A missing=%A dir=%A" find missing dir
                        let fullPath = Array.concat [ [| dir |]; find; missing ] |> Path.Combine
                        Directory.CreateDirectory fullPath |> ignore
                        Some fullPath
                    else
                        None

            [ maxSuffixLen..1 ]
            |> List.tryPick (fun i ->
                Directory.GetCurrentDirectory()
                |> getDirectoryParents
                |> Seq.tryPick (fun dir -> suffixParts |> List.tryPick (findCreateDir dir i)))
            |> Option.defaultWith (fun () ->
                let rootPath = sessionValuePath.Force() |> Path.GetDirectoryName
                let fullPath = Path.Combine(rootPath, (possibleSuffixes |> List.head))
                Directory.CreateDirectory fullPath |> ignore
                fullPath)

        let getInputPath (year: int) (day: int) (inputType: InputType) =

            let filename =
                match inputType with
                | InputType.Default -> "input.txt"
                | InputType.Test -> "test.txt"
                | InputType.Alt -> "alt.txt"
                | e -> failwithf "Unsupported value: %A" e

            let inputFolder = getInputFolder year day

            Path.Combine(inputFolder, filename)

        let getExpectedPath (year: int) (day: int) (inputType: InputType) (part: int) =
            let inputPath = getInputPath year day inputType

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

        let delayWithProgress delayMs =
            let watch = System.Diagnostics.Stopwatch.StartNew()
            let p = AnsiConsole.Progress()
            p.AutoClear <- true

            let mutable columns: ProgressColumn list =
                [ new TaskDescriptionColumn()
                  new ProgressBarColumn()
                  new RemainingTimeColumn() ]

            p.Columns(columns |> List.toArray) |> ignore

            let delay = System.TimeSpan.FromMilliseconds delayMs
            let delayMs = (delay - watch.Elapsed).TotalMilliseconds
            let description = sprintf "Sleeping for %3.2fs" delay.TotalSeconds

            p.Start(fun ctx ->
                let delayTask = ctx.AddTask(description, maxValue = delayMs)

                while not ctx.IsFinished do
                    delayTask.Value <- watch.Elapsed.TotalMilliseconds
                    let sleepTime = min (delay - watch.Elapsed) (System.TimeSpan.FromMilliseconds 100)
                    System.Threading.Thread.Sleep(sleepTime)
                    ctx.Refresh())

        let ensureRequestLimit () =
            let aocTrackerPath = Path.Combine(Path.GetTempPath(), "aoc.tracker")
            let now = DateTime.UtcNow

            if File.Exists aocTrackerPath then
                let lastRequestTime = File.GetLastWriteTimeUtc aocTrackerPath
                let waitTime = (lastRequestTime + RequestLimit) - now

                if waitTime > TimeSpan.Zero then
                    delayWithProgress waitTime.TotalMilliseconds
            else
                (File.Create aocTrackerPath).Close()

            File.SetLastWriteTimeUtc(aocTrackerPath, DateTime.UtcNow)

        let getReleaseTime year day =
            new DateTime(year, 12, day, 5, 0, 0, DateTimeKind.Utc)

        let downloadInput year day inputPath =
            let releaseDate = getReleaseTime year day

            if releaseDate > DateTime.UtcNow then
                printf "%A is in the future, no input file yet" releaseDate
                None
            else
                let session = sessionValue.Force()

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
                        let session = sessionValue.Force()

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
            let path = getInputPath year day inputType

            match readFile path with
            | Some data -> Some data
            | None when inputType = InputType.Default -> downloadInput year day path
            | None -> None

        let getExpected (year: int) (day: int) (inputType: InputType) (part: int) =
            let path = getExpectedPath year day inputType part

            match readFile path with
            | Some data -> data |> Text.Encoding.ASCII.GetString |> Some
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
                executingYearDay <- (d.year, d.day) |> Some

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
                    executingYearDay <- None

                    if silentOutput then
                        Console.SetOut originalOut

                { year = d.year
                  day = d.day
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

        let executeRun (inputType: InputType) (day: Day) =
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

        let executeTest (repeats: int option) (days: Day list) =
            let silentOutput = true
            let repeats = repeats |> Option.defaultValue 1
            let mutable fastestTotalMs = 0.0
            let mutable slowestTotalMs = 0.0

            let mutable dayTimes = Map.empty

            printfn "By day:"
            printfn "%4s %3s %9s %-3s" "Year" "Day" "Time" "[S]"

            let resultsToState results =
                results
                |> Array.take 2
                |> Array.fold
                    (fun s (_, r) ->
                        match r with
                        | None -> s + "❔"
                        | Some(Error _) -> s + "❌"
                        | _ -> s + "✅")
                    ""

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
                            "%4d %3d %9.3f %-3s %s %s"
                            r.year
                            r.day
                            (r.elapsedMs / (float repeats))
                            (resultsToState r.results)
                            r.name
                            (multi fastestMs r.elapsedMs)
                    else
                        printfn
                            "%4d %3d %9.3f %-3s %s"
                            r.year
                            r.day
                            (r.elapsedMs / (float repeats))
                            (resultsToState r.results)
                            r.name

                    fastestMs <- min fastestMs r.elapsedMs
                    slowestMs <- max slowestMs r.elapsedMs

                dayTimes <- dayTimes |> Map.add (day.year, day.day) fastestMs
                fastestTotalMs <- fastestTotalMs + fastestMs
                slowestTotalMs <- slowestTotalMs + slowestMs

            printfn "%5s %9.3f" "Total" (slowestTotalMs / (float repeats))

            // printfn "%3s %4s %3s %9s" "Rnk" "Year" "Day" "Time"

            // for index, ((year, day), time) in dayTimes |> Map.toList |> List.sortBy snd |> List.indexed do
            //     printfn "%3d %4d %3d %9.3f" (index + 1) year day (time / (float repeats))

            let totalAvgFastestMs = fastestTotalMs / (float repeats)
            let n = dayTimes.Count |> float
            let dayAvg = totalAvgFastestMs / n
            let dayStddev = dayTimes |> Map.values |> Seq.sumBy (fun t -> (t - dayAvg) ** 2)
            let dayStddev = dayStddev / n |> sqrt
            let expectedMs = 250.0 * (float dayTimes.Count)

            printfn "\nBy (fastest) time:"

            let minTime = dayTimes.Values |> Seq.min
            let maxTime = dayTimes.Values |> Seq.max

            let timeToColor t =
                let red = 255.0 * (t - minTime) / (maxTime - minTime)
                let green = 255.0 - red

                new Color((byte red), (byte green), 0uy)

            dayTimes
            |> Map.toList
            |> List.sortBy snd
            |> List.indexed
            |> Seq.fold
                (fun (bc: BarChart) (index, ((year, day), time)) ->
                    bc.AddItem($"[bold]%3d{index}[/] [green]%d{year}[/] %2d{day}", time, (timeToColor time)))
                (BarChart())
            |> AnsiConsole.Write

            let years = dayTimes |> Map.keys |> Seq.toList |> List.groupBy fst

            printfn
                "%6s %9.1f or %s faster then slowest"
                "Total"
                totalAvgFastestMs
                (multi slowestTotalMs fastestTotalMs)

            printfn "%6s %9.1f a difference of %.1fms" "Expect" expectedMs (totalAvgFastestMs - expectedMs)

            printfn "%6s %10.2f%%" "Grade" (100.0 * expectedMs / totalAvgFastestMs)

            let randomColor () =
                let color = Array.zeroCreate 3
                System.Random.Shared.NextBytes color
                Color(color[0], color[1], color[2])

            if years.Length > 1 then
                years
                |> List.map (fun (year, items) -> year, (items |> List.sumBy (fun k -> dayTimes[k])))
                |> List.fold
                    (fun (bc: BreakdownChart) (year, totalTime) -> bc.AddItem($"{year}", totalTime, (randomColor ())))
                    (BreakdownChart())
                |> AnsiConsole.Write

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

        let a = System.Reflection.Assembly.GetCallingAssembly()

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
