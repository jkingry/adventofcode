using System.CommandLine.Parsing;

namespace AdventOfCode.Cli;

internal sealed class SolutionParser(IEnumerable<Solution> days, NorthPole? northPole)
{
    public int DefaultYear => northPole?.Options.DefaultYear ?? days.Max(d => d.Year);

    public Solution[] ParseArguments(ArgumentResult result, bool defaultOneDay = false)
    {
        try
        {
            var solutions = Parse([.. result.Tokens.Select(t => t.Value)], defaultOneDay);
            if (solutions.Length == 0)
            {
                result.AddError($"Failed to find any solutions for the specified year/day.");
                return [];
            }

            return solutions;
        }
        catch (InvalidOperationException ex)
        {
            result.AddError($"Failed to parse year/day: {ex.Message}");
            return [];
        }
    }

    public Solution[] Parse(string[] values, bool defaultOneDay = false)
    {
        var selected = new HashSet<Solution>();

        if (values.Length == 0)
        {
            if (defaultOneDay)
            {
                var defaultDay = days.Where(d => d.Year == DefaultYear).MaxBy(d => d.Day);
                if (defaultDay != null)
                {
                    selected.Add(defaultDay);
                }
            }
            else
            {
                selected.UnionWith(days.Where(d => d.Year == DefaultYear));
            }
        }
        else
        {
            foreach (var target in values)
            {
                var (negate, matchedDays) = ParseYearDays(target);

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

        return selected.OrderBy(x => x.Year).ThenBy(x => x.Day).ToArray();
    }

    private (bool negate, IEnumerable<Solution> values) ParseYearDays(string input)
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

        var matcher = ParseMatchFunc(input, DefaultYear);

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

    private static IEnumerable<int> ParseRangeList(string input)
    {
        int[] ParseSingle(string input)
        {
            if (!int.TryParse(input, out var value))
            {
                throw new InvalidOperationException($"Failed to parse integer: {input}");
            }
            return [value];
        }

        int[] ParseRange(string start, string end)
        {
            if (!int.TryParse(start, out var startValue))
            {
                throw new InvalidOperationException($"Failed to parse integer: {start}");
            }
            if (!int.TryParse(end, out var endValue))
            {
                throw new InvalidOperationException($"Failed to parse integer: {end}");
            }
            if (endValue < startValue)
            {
                throw new InvalidOperationException($"Invalid range: {start}-{end}");
            }
            return Enumerable.Range(startValue, endValue - startValue + 1).ToArray();
        }


        return input.Split(',')
            .SelectMany(term => term.Split('-') switch
            {
                ["*"] => Enumerable.Range(1, 2050),
                [var x] => ParseSingle(x),
                [var a, var b] => ParseRange(a, b),
                _ => throw new InvalidOperationException($"Failed to parse: {term}")
            });
    }
}
