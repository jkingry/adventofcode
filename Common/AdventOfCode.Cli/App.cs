using System.CommandLine;
using System.Reflection;

using BenchmarkDotNet.Running;

using Spectre.Console;

namespace AdventOfCode.Cli;

public static class App
{
    public static NorthPole? NorthPole { get; private set; }

    internal static async Task<int> RunCommand(IEnumerable<Solution> solutions, InputType inputTypeOption, CancellationToken cancellationToken = default)
    {
        var options = new RunOptions
        {
            InputType = inputTypeOption
        };

        AnsiConsole.MarkupLine(
            "{0,4}:{1,3} {2,11} {3,8} {4,4} {5,7} {6}",
            "Year", "Day", "Method", "Time", "Part", "Status", "Value");

        foreach (var solution in solutions)
        {
            if (NorthPole == null)
            {
                throw new InvalidOperationException("NorthPole is not initialized.");
            }

            AnsiConsole.MarkupInterpolated(
                $"{solution.Year,4:0000}:{solution.Day,-3:00} {solution.Name,11}");

            var output = await NorthPole.RunAsync(solution, options);

            var (result, actual) = output.PartOutputs[0];

            AnsiConsole.MarkupLineInterpolated($" {output.ElapsedMs,8:0.000} {1,4} {result,7} {actual}");

            for (var part = 1; part < output.PartOutputs.Length; ++part)
            {
                (result, actual) = output.PartOutputs[part];

                AnsiConsole.MarkupLineInterpolated($"{string.Empty,29} {part + 1,4} {result,7} {actual}");
            }
        }

        return 0;
    }

    internal static async Task<int> TestCommand(IEnumerable<Solution> selected, IEnumerable<Solution> possible, int repeats = 1, CancellationToken cancellationToken = default)
    {
        if (NorthPole == null)
        {
            throw new InvalidOperationException("NorthPole is not initialized.");
        }

        var options = new RunOptions
        {
            Repeats = repeats,
            SilentOutput = true,
        };

        string PartOutputsToString(PartOutput[] outputs)
        {
            var result = new char[outputs.Length];
            for (var i = 0; i < outputs.Length; i++)
            {
                result[i] = outputs[i].Result switch
                {
                    ResultType.Ok => '✅',
                    ResultType.Error => '❌',
                    _ => '❔',
                };
            }

            return new string(result);
        }

        string MultipleToString(double slowMs, double fastMs)
        {
            var factor = slowMs / fastMs;

            if (factor < 2.0)
            {
                var percent = 100.0 * (factor - 1.0);
                return $"{percent:0.00}%";
            }

            return $"x{factor:0.00}";
        }

        var fastestTotalMs = 0.0;
        var slowestTotalMs = 0.0;
        var outputs = new List<SolutionOutputs>();

        AnsiConsole.WriteLine("By day:");
        AnsiConsole.WriteLine("{0,4} {1,3} {2,9} {3,-3}", "Year", "Day", "Time", "[S]");

        bool IsAllOk(SolutionOutputs output)
        {
            return output.PartOutputs.All(p => p.Result == ResultType.Ok);
        }

        foreach (var day in selected.GroupBy(s => (s.Year, s.Day)).OrderBy(g => g.Key))
        {
            var fastestMs = double.PositiveInfinity;
            var slowestMs = double.NegativeInfinity;
            var success = false;

            foreach (var solution in day)
            {
                var output = await NorthPole.RunAsync(solution, options);

                outputs.Add(output);

                var formatString = fastestMs < double.PositiveInfinity
                    ? "{0,4} {1,3} {2,9:0.000} {3,-3} {4} {5}"
                    : "{0,4} {1,3} {2,9:0.000} {3,-3} {4}";

                AnsiConsole.WriteLine(formatString,
                    output.Year,
                    output.Day,
                    output.ElapsedMs,
                    PartOutputsToString(output.PartOutputs),
                    output.Name,
                    MultipleToString(output.ElapsedMs, fastestMs));

                if (IsAllOk(output))
                {
                    success = true;
                    fastestMs = Math.Min(fastestMs, output.ElapsedMs);
                    slowestMs = Math.Max(slowestMs, output.ElapsedMs);
                }
            }

            if (success)
            {
                fastestTotalMs += fastestMs;
                slowestTotalMs += slowestMs;
            }
        }

        var byFastestTime = outputs
            .Where(IsAllOk)
            .GroupBy(o => (o.Year, o.Day))
            .Select(o => o.MinBy(o => o.ElapsedMs) ?? throw new InvalidOperationException("No min found"))
            .OrderBy(o => o.ElapsedMs)
            .Index()
            .ToList();

        var chart = new BarChart().Label("By (fastest) time").Width(60);
        var minTime = byFastestTime.Min(o => o.Item.ElapsedMs);
        var maxTime = byFastestTime.Max(o => o.Item.ElapsedMs);

        foreach (var (index, output) in byFastestTime)
        {
            var color = GetColor(1.0 - (1.0 * (output.ElapsedMs - minTime) / (maxTime - minTime)));

            chart.AddItem(
                $"[bold]{index + 1,7}[/] [green]{output.Year}[/] {output.Day,2}",
                output.ElapsedMs,
                color);
        }

        AnsiConsole.WriteLine();
        AnsiConsole.Write(chart);
        AnsiConsole.MarkupLineInterpolated($"{"Total",7} {fastestTotalMs,11:0.000} ms or {MultipleToString(slowestTotalMs, fastestTotalMs)} faster then slowest");

        var expectedMs = 250.0 * byFastestTime.Count;
        AnsiConsole.MarkupLineInterpolated($"{"Expect",7} {expectedMs,9:0.0} a difference of {fastestTotalMs - expectedMs:0.0}ms");

        var yearGroups = byFastestTime.Select(o => o.Item)
            .GroupBy(o => o.Year)
            .OrderBy(g => g.Key)
            .Select(g => new
            {
                Year = g.Key,
                AverageDuration = g.Sum(o => o.ElapsedMs) / g.Count()
            })
            .ToList();

        if (yearGroups.Count > 1)
        {
            var yearBreakdownChart = new BarChart().Width(60).Label("Year Avg. Duration (Log)");
            var minDuration = yearGroups.Min(o => Math.Log10(o.AverageDuration));
            var maxDuration = yearGroups.Max(o => Math.Log10(o.AverageDuration));
            var deltaDuration = maxDuration - minDuration;

            foreach (var yearGroup in yearGroups)
            {
                var color = GetColor(1.0 - (Math.Log10(yearGroup.AverageDuration) - minDuration) / deltaDuration);
                yearBreakdownChart.AddItem($"{yearGroup.Year,6}", Math.Log10(yearGroup.AverageDuration), color);
            }

            AnsiConsole.Write(yearBreakdownChart);
        }

        var possibleYears = possible.Select(o => o.Year).Distinct().OrderBy(x => x).ToList();
        var yearChart = new BarChart().Width(60).Label("Year Completion");

        void AddChartLine(BarChart chart, string label, int completed, int expected)
        {
            var color = GetColor(1.0 * completed / expected);

            chart.AddItem(
                $"[bold]{label,6}[/] {completed,3} of {expected,3}",
                100.0 * completed / expected,
                color);
        }

        foreach (var year in possibleYears)
        {
            var completed = byFastestTime.Where(o => o.Item.Year == year).Count();
            var yearExpected = possible.Where(o => o.Year == year).Count();
            AddChartLine(yearChart, year.ToString(), completed, yearExpected);

        }
        AddChartLine(yearChart, "Total", byFastestTime.Count, possible.Count());

        AnsiConsole.Write(yearChart);

        var dayGroups = byFastestTime.Select(o => o.Item)
            .GroupBy(o => o.Day)
            .OrderBy(g => g.Key)
            .Select(g => new
            {
                Day = g.Key,
                AverageDuration = g.Sum(o => o.ElapsedMs) / g.Count()
            })
            .ToList();

        if (dayGroups.Count > 1)
        {
            var dayBreakdownChart = new BarChart().Width(60).Label("Day Avg. Duration (Log)");
            var minDuration = dayGroups.Min(o => Math.Log10(o.AverageDuration));
            var maxDuration = dayGroups.Max(o => Math.Log10(o.AverageDuration));
            var deltaDuration = maxDuration - minDuration;


            foreach (var dayGroup in dayGroups)
            {
                var color = GetColor(1.0 - (Math.Log10(dayGroup.AverageDuration) - minDuration) / deltaDuration);
                dayBreakdownChart.AddItem($"{dayGroup.Day,6}", Math.Log10(dayGroup.AverageDuration), color);
            }

            AnsiConsole.Write(dayBreakdownChart);
        }

        var possibleDays = possible.Select(o => o.Day).Distinct().OrderBy(x => x).ToList();
        var dayChart = new BarChart().Width(60).Label("Day Completion");
        foreach (var day in possibleDays)
        {
            var completed = byFastestTime.Where(o => o.Item.Day == day).Count();
            var dayExpected = possible.Where(o => o.Day == day).Count();
            AddChartLine(dayChart, day.ToString(), completed, dayExpected);

        }
        AddChartLine(dayChart, "Total", byFastestTime.Count, possible.Count());

        AnsiConsole.Write(dayChart);

        return 0;
    }

    public static async Task<int> RunAsync(IEnumerable<Solution> days)
    {
        try
        {
            RootCommand rootCommand = CreateRootCommand(days);

            var args = Environment.GetCommandLineArgs().Skip(1).ToArray();

            var parseResult = rootCommand.Parse(args);

            return await parseResult.InvokeAsync();
        }
        catch (InvalidSessionFileException ex)
        {
            Console.Error.WriteLine("Please visit http://adventofcode.com and login to create a session cookie.");
            Console.Error.WriteLine($"Then save the cookie to a file named '{ex.Message}' in this directory.");

            return 1;
        }
    }

    private static RootCommand CreateRootCommand(IEnumerable<Solution> days)
    {
        NorthPole = NorthPoleBuilder.CreateNorthPole();

        var minYear = days.Min(d => d.Year);
        var maxYear = days.Max(d => d.Year);
        var minDay = days.Min(d => d.Day);
        var maxDay = days.Max(d => d.Day);

        var solutionParser = new SolutionParser(days, NorthPole);

        Argument<Solution[]> solutionsArgument = new("year:day")
        {
            Description = $"The year ({minYear}-{maxYear}) and/or the day ({minDay}-{maxDay}) to run",
            DefaultValueFactory = results => solutionParser.ParseArguments(results, true),
            CustomParser = results => solutionParser.ParseArguments(results, true),
            Arity = ArgumentArity.ZeroOrMore
        };

        Option<InputType> inputTypeOption = new("--inputType", "-i")
        {
            DefaultValueFactory = _ => InputType.Official
        };

        Command runCommand = new("run", "Run the specified day's solution")
            {
                solutionsArgument,
                inputTypeOption
            };

        runCommand.SetAction((parseResult, ct) => RunCommand(
            parseResult.GetRequiredValue(solutionsArgument),
            parseResult.GetValue(inputTypeOption),
            ct));

        Argument<Solution[]> multipleSolutionsArgument = new("year:day")
        {
            Description = $"The year ({minYear}-{maxYear}) and/or the day ({minDay}-{maxDay}) to run",
            DefaultValueFactory = results => solutionParser.ParseArguments(results, false),
            CustomParser = results => solutionParser.ParseArguments(results, false),
            Arity = ArgumentArity.ZeroOrMore
        };

        Option<int> repeatsOption = new Option<int>("--repeats", "-r")
        {
            Description = "The number of times to repeat each solution for benchmarking purposes",
            DefaultValueFactory = _ => 1
        };

        Command testCommand = new("test", "Test and benchmark solutions")
        {
            multipleSolutionsArgument,
            repeatsOption
        };

        SolutionParser possiblesParser = new(NorthPole.AllPossibleDates(), NorthPole);

        testCommand.SetAction((parseResult, ct) =>
        {
            var solutionsResult = parseResult.GetResult(multipleSolutionsArgument) ?? throw new InvalidOperationException("No solutions parsed");
            var possibles = possiblesParser.ParseArguments(solutionsResult, false);

            return TestCommand(
               parseResult.GetRequiredValue(multipleSolutionsArgument),
               possibles,
               parseResult.GetValue(repeatsOption),
               ct);
        });

        Command benchCommand = new("bench", "Benchmark solutions");
        benchCommand.SetAction(parseResult =>
            BenchmarkRunner.Run(Assembly.GetEntryAssembly() ?? throw new InvalidOperationException(), null, parseResult.UnmatchedTokens.ToArray()));

        RootCommand rootCommand = new("Advent of Code CLI")
        {
            Subcommands = { runCommand, testCommand, benchCommand },
        };

        return rootCommand;
    }

    private static Color GetColor(double amount)
    {
        return ColorUtil.HslToRgb(amount * 120, 100.0, 50.0);
    }
}
