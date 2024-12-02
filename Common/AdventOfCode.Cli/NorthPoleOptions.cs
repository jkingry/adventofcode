namespace AdventOfCode.Cli;

public class NorthPoleOptions
{
    public int DefaultOutputs { get; set; } = 2;

    public string SessionFileName { get; set; } = ".adventofcode.session";

    public string[] InputFolderPatterns { get; set; } =
        [
            "{0:0000}/inputs/{1:00}",
            "y{0:0000}/inputs/day{1:00}",
            "inputs/{0:0000}/{1:00}",
            "inputs/Y{0:0000}/day{1:00}",
        ];

    public Dictionary<FileType, List<string>> FileNamePatterns { get; set; } =
        new()
        {
            [FileType.Input] = ["input.txt"],
            [FileType.ExampleInput] = ["example.txt"],
            [FileType.HtmlPage] = ["day.html"],
            [FileType.ExpectedOutput] = ["input.s{2}.txt"],
            [FileType.ExampleExpectedOutput] = ["example.s{2}.txt"],
        };

    public string AdventOfCodeUrl { get; set; } = "https://adventofcode.com";
    public TimeSpan RequestLimit { get; set; } = TimeSpan.FromSeconds(10.0);
}
/*

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

            let mutable results = []

            for day in days do
                let mutable fastestMs = Double.PositiveInfinity
                let mutable slowestMs = Double.NegativeInfinity

                for r in runDay day InputType.Default repeats silentOutput do
                    if interactive then
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

                    let resultTypes =
                        r.results
                        |> Array.take 2
                        |> Array.map (function
                            | _, Some(Error _) -> ResultType.Error
                            | _, Some(Ok _) -> ResultType.Ok
                            | None, _ -> ResultType.Missing
                            | _, _ -> ResultType.Unknown)

                    let result: TestResult =
                        { year = r.year
                          day = r.day
                          name = r.name
                          results = resultTypes
                          elapsedMs = r.elapsedMs }

                    results <- result :: results

                    fastestMs <- min fastestMs r.elapsedMs
                    slowestMs <- max slowestMs r.elapsedMs

                dayTimes <- dayTimes |> Map.add (day.year, day.day) fastestMs
                fastestTotalMs <- fastestTotalMs + fastestMs
                slowestTotalMs <- slowestTotalMs + slowestMs

            if interactive then
                printfn "%5s %9.3f" "Total" (slowestTotalMs / (float repeats))

                // printfn "%3s %4s %3s %9s" "Rnk" "Year" "Day" "Time"

                // for index, ((year, day), time) in dayTimes |> Map.toList |> List.sortBy snd |> List.indexed do
                //     printfn "%3d %4d %3d %9.3f" (index + 1) year day (time / (float repeats))

                let totalAvgFastestMs = fastestTotalMs / (float repeats)
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
                        bc.AddItem(
                            $"[bold]%3d{(index + 1)}[/] [green]%d{year}[/] %2d{day}",
                            time / (float repeats),
                            (timeToColor time)
                        ))
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
                        (fun (bc: BreakdownChart) (year, totalTime) ->
                            bc.AddItem($"{year}", totalTime, (randomColor ())))
                        (BreakdownChart())
                    |> AnsiConsole.Write

            results

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
*/