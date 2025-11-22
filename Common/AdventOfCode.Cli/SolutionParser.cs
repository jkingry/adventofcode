using System.CommandLine.Parsing;
using System.Text.RegularExpressions;

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
                var maxDay = days.Where(d => d.Year == DefaultYear).Max(d => d.Day);
                selected.UnionWith(days.Where(d => d.Year == DefaultYear && d.Day == maxDay));
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
                    var part = parts[0];

                    var matcher = ParseRangeList(parts[0]);

                    switch (matcher.Type)
                    {
                        case MatchType.All:
                            return _ => true;
                        case MatchType.Number:
                            return d => matcher.IsMatch(d.Year) || (d.Year == maxYear && matcher.IsMatch(d.Day));
                        case MatchType.Regex:
                            return d => d.Year == maxYear && matcher.IsMatch(d.Name);
                        default:
                            throw new InvalidOperationException($"Unknown match type: {matcher.Type}");
                    }
                }
            case 2:
                {
                    var yearOrDayMatcher = ParseRangeList(parts[0]);
                    var dayOrNameMatcher = ParseRangeList(parts[1]);

                    switch (dayOrNameMatcher.Type)
                    {
                        case MatchType.Number:
                            return d => yearOrDayMatcher.IsMatch(d.Year) && dayOrNameMatcher.IsMatch(d.Day);
                        case MatchType.Regex:
                            return d => d.Year == maxYear && yearOrDayMatcher.IsMatch(d.Day) && dayOrNameMatcher.IsMatch(d.Name);
                        case MatchType.All:
                            return d => yearOrDayMatcher.IsMatch(d.Year) || (d.Year == maxYear && yearOrDayMatcher.IsMatch(d.Day));
                        default:
                            throw new InvalidOperationException($"Unknown match type: {dayOrNameMatcher.Type}");
                    }
                }
            case 3:
                {
                    var yearMatcher = ParseRangeList(parts[0]);
                    var dayMatcher = ParseRangeList(parts[1]);
                    var nameMatcher = ParseRangeList(parts[2]);

                    if (yearMatcher.Type == MatchType.Regex || dayMatcher.Type == MatchType.Regex)
                    {
                        throw new InvalidOperationException($"Year and Day parts cannot be regex: {input}");
                    }

                    return d => yearMatcher.IsMatch(d.Year) && dayMatcher.IsMatch(d.Day) && nameMatcher.IsMatch(d.Name);
                }
            default:
                throw new InvalidOperationException($"Too many parts: {input}");
        }
    }

    enum MatchType
    {
        All,
        Number,
        Regex
    }

    record MatchRange
    {
        public MatchType Type { get; init; }
        public Func<object, bool> IsMatch { get; init; } = _ => false;
    }

    private static MatchRange ParseRangeList(string input)
    {
        MatchRange ParseSingle(string input)
        {
            if (!int.TryParse(input, out var value))
            {
                return new MatchRange { Type = MatchType.Regex, IsMatch = obj => (string)obj == "*" || Regex.IsMatch((string)obj, input, RegexOptions.IgnoreCase) };
            }

            return new MatchRange { Type = MatchType.Number, IsMatch = obj => (int)obj == value };
        }

        MatchRange ParseRange(string start, string end)
        {
            if (!int.TryParse(start, out var startValue))
            {
                throw new InvalidOperationException($"Failed to parse range start: {start}");
            }
            if (!int.TryParse(end, out var endValue))
            {
                throw new InvalidOperationException($"Failed to parse range end: {end}");
            }
            if (endValue < startValue)
            {
                throw new InvalidOperationException($"Invalid range: {start}-{end}");
            }
            return new MatchRange { Type = MatchType.Number, IsMatch = obj => startValue <= (int)obj && (int)obj <= endValue };
        }


        var matchers = input.Split(',')
            .Select(term => term.Split('-') switch
            {
                ["*"] => new MatchRange { Type = MatchType.All, IsMatch = _ => true },
                [var x] => ParseSingle(x),
                [var a, var b] => ParseRange(a, b),
                _ => throw new InvalidOperationException($"Failed to parse: {term}")
            })
            .ToList();

        if (matchers.Count == 1)
        {
            return matchers[0];
        }

        if (matchers.Any(m => m.Type != matchers[0].Type))
        {
            throw new InvalidOperationException($"Mixed match types in: {input}");
        }

        return new MatchRange
        {
            Type = matchers[0].Type,
            IsMatch = obj => matchers.Any(m => m.IsMatch(obj))
        };
    }
}
