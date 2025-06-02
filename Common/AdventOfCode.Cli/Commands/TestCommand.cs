using Microsoft.CodeAnalysis.CSharp.Syntax;
using Spectre.Console;
using Spectre.Console.Cli;

namespace AdventOfCode.Cli.Commands;

internal class TestCommand : AsyncCommand<TestCommand.Settings>
{
    public override async Task<int> ExecuteAsync(CommandContext context, Settings settings)
    {
        var days = context.Data as IEnumerable<Solution>
            ?? throw new InvalidOperationException("No days available.");

        var selected = new HashSet<Solution>();

        if (settings.Targets.Length == 0)
        {
            var maxYear = days.Max(d => d.Year);
            selected.UnionWith(days.Where(d => d.Year == maxYear));
        }
        else
        {
            foreach (var target in settings.Targets)
            {
                var (negate, matchedDays) = ParseYearDays(days, target);

                if (negate)
                {
                    selected.ExceptWith(matchedDays);
                }
                else
                {
                    selected.UnionWith(matchedDays);
                }
            }
        }

        var northPole = NorthPoleBuilder.CreateNorthPole();

        var options = new RunOptions
        {
            Repeats = settings.Repeats,
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
                var output = await northPole.RunAsync(solution, options);

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

        var chart = new BarChart();
        var minTime = byFastestTime.Min(o => o.Item.ElapsedMs);
        var maxTime = byFastestTime.Max(o => o.Item.ElapsedMs);

        foreach (var (index, output) in byFastestTime)
        {
            var red = 255.0 * (output.ElapsedMs - minTime) / (maxTime - minTime);
            var green = 255.0 - red;
            var color = new Color((byte)red, (byte)green, 0);

            chart.AddItem(
                $"[bold]{index + 1}[/] [green]{output.Year}[/] {output.Day,2}",
                output.ElapsedMs,
                color);
        }

        AnsiConsole.WriteLine();
        AnsiConsole.WriteLine("By (fastest) time:");
        AnsiConsole.Write(chart);
        AnsiConsole.MarkupLineInterpolated($"{"Total",7} {fastestTotalMs,11:0.000} ms or {MultipleToString(slowestTotalMs, fastestTotalMs)} faster then slowest");

        var expectedMs = 250.0 * byFastestTime.Count;
        AnsiConsole.MarkupLineInterpolated($"{"Expect",7} {expectedMs,9:0.0} a difference of {fastestTotalMs - expectedMs:0.0}ms");
        AnsiConsole.MarkupLineInterpolated($"{"Grade",7} {expectedMs / fastestTotalMs,11:0.00%}");

        Color RandomColor()
        {
            var color = new byte[3];
            Random.Shared.NextBytes(color);
            return new Color(color[0], color[1], color[2]);
        }

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
            var yearBreakdownChart = new BreakdownChart();
            var minDuration = yearGroups.Min(o => o.AverageDuration);

            foreach (var yearGroup in yearGroups)
            {
                yearBreakdownChart.AddItem($"{yearGroup.Year}", yearGroup.AverageDuration / minDuration, RandomColor());
            }

            AnsiConsole.Write(yearBreakdownChart);
        }

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
            var dayBreakdownChart = new BreakdownChart();
            var minDuration = dayGroups.Min(o => o.AverageDuration);

            foreach (var dayGroup in dayGroups)
            {
                dayBreakdownChart.AddItem($"{dayGroup.Day}", dayGroup.AverageDuration / minDuration, RandomColor());
            }

            AnsiConsole.Write(dayBreakdownChart);
        }

        return 0;
    }

    private static IEnumerable<int> ParseRangeList(string input)
    {
        return input.Split(',')
            .SelectMany(term => term.Split('-') switch
            {
                ["*"] => Enumerable.Range(1, 2050),
                [var x] => new[] { int.Parse(x) },
                [var a, var b] => Enumerable.Range(int.Parse(a), int.Parse(b) - int.Parse(a) + 1),
                _ => throw new InvalidOperationException($"Failed to parse: {term}")
            });
    }

    private static (bool, IEnumerable<Solution>) ParseYearDays(IEnumerable<Solution> days, string input)
    {
        // <null> -> highest year & all days
        // 23 -> highest year & day 23
        // 2016 -> highest day of year 2016
        // 2016-2020 -> all days from 2016 to 2020
        // 2016,2016 -> all days from 2016 and 2017
        // 2016:3,4 -> 2016 days 3 and 4
        // 1-10 -> days 1 to 10 of current year
        // 2016-2020:1-23 days 1 to 23 of year 2016 to 2020

        var negate = false;
        if (input.StartsWith("~"))
        {
            input = input.Substring(1);
            negate = true;
        }

        var maxYear = days.Max(d => d.Year);

        var matcher = ParseMatchFunc(input, maxYear);

        return (negate, days.Where(matcher));
    }

    private static Func<Solution, bool> ParseMatchFunc(string input, int maxYear)
    {
        var parts = input.Split(':');

        switch (parts.Length)
        {
            case 1:
                {
                    var values = ParseRangeList(parts[0]).ToList();

                    if (values.Max() <= 25)
                    {
                        return d => d.Year == maxYear && values.Contains(d.Day);
                    }

                    return d => values.Contains(d.Year);
                }
            case 2:
                {
                    var yearValues = ParseRangeList(parts[0]).ToList();
                    var dayValues = ParseRangeList(parts[1]).ToList();

                    return d => yearValues.Contains(d.Year) && dayValues.Contains(d.Day);
                }
            default:
                throw new InvalidOperationException($"Too many parts: {input}");
        }
    }

    internal class Settings : CommandSettings
    {
        [CommandOption("-n|--repeats")]
        public int Repeats { get; set; } = 1;

        [CommandOption("--json")]
        public bool Json { get; set; }

        [CommandArgument(0, "[year:days]")]
        public string[] Targets { get; set; } = [];
    }
}