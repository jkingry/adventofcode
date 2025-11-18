using System.Text;
using System.Text.RegularExpressions;

namespace AdventOfCode.CSharp.Y2020;

public static class Day16
{
    enum ParseMode
    {
        rules,
        your,
        nearby
    }

    class FieldRule
    {
        public string? Name;
        public List<(int min, int max)> Ranges = new List<(int min, int max)>();
    }

    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        long Part1()
        {
            var mode = ParseMode.rules;

            var ruleRanges = new List<(int min, int max)>();

            var errors = 0;

            foreach (var line in Input)
            {
                if (string.IsNullOrWhiteSpace(line)) continue;

                switch (mode)
                {
                    case ParseMode.rules:
                        if (line == "your ticket:")
                        {
                            mode = ParseMode.your;
                        }
                        else
                        {
                            var m = Regex.Match(line, @"(.+)\: ([0-9]+)\-([0-9]+) or ([0-9]+)\-([0-9]+)");

                            ruleRanges.Add((int.Parse(m.Groups[2].Value), int.Parse(m.Groups[3].Value)));
                            ruleRanges.Add((int.Parse(m.Groups[4].Value), int.Parse(m.Groups[5].Value)));
                        }
                        break;
                    case ParseMode.your:
                        if (line == "nearby tickets:")
                        {
                            mode = ParseMode.nearby;
                        }
                        else
                        {

                        }
                        break;
                    case ParseMode.nearby:
                        bool invalid = false;

                        foreach (var v in line.Split(',').Select(int.Parse))
                        {
                            if (ruleRanges.All(r => v < r.min || v > r.max))
                            {
                                invalid = true;
                                break;
                            }
                        }

                        if (!invalid)
                        {

                        }
                        break;
                }

            }

            return errors;
        }

        long Part2()
        {
            var mode = ParseMode.rules;

            var fields = new List<FieldRule>();

            var fieldMask = new Dictionary<int, HashSet<string>>();

            int[]? your = null;

            foreach (var line in Input)
            {
                if (string.IsNullOrWhiteSpace(line)) continue;

                switch (mode)
                {
                    case ParseMode.rules:
                        if (line == "your ticket:")
                        {
                            mode = ParseMode.your;
                        }
                        else
                        {
                            var m = Regex.Match(line, @"(.+)\: ([0-9]+)\-([0-9]+) or ([0-9]+)\-([0-9]+)");
                            Console.WriteLine(m.Groups[1].Value);
                            fields.Add(new FieldRule
                            {
                                Name = m.Groups[1].Value,
                                Ranges =
                            {
                                (int.Parse(m.Groups[2].Value), int.Parse(m.Groups[3].Value)),
                                (int.Parse(m.Groups[4].Value), int.Parse(m.Groups[5].Value)),
                            }
                            });
                        }
                        break;
                    case ParseMode.your:
                        if (line == "nearby tickets:")
                        {
                            mode = ParseMode.nearby;
                            foreach (var i in Enumerable.Range(0, fields.Count))
                            {
                                fieldMask[i] = new HashSet<string>(fields.Select(x => x.Name ?? ""));
                            }
                        }
                        else
                        {
                            your = line.Split(',').Select(int.Parse).ToArray();
                        }
                        break;
                    case ParseMode.nearby:
                        bool valid = true;
                        foreach (var v in line.Split(',').Select(int.Parse))
                        {
                            if (fields.SelectMany(f => f.Ranges).All(r => v < r.min || v > r.max))
                            {
                                valid = false;
                            }
                        }

                        if (valid)
                        {
                            var i = 0;
                            foreach (var v in line.Split(',').Select(int.Parse))
                            {
                                if (fieldMask[i].Count == 1)
                                {
                                    i += 1;
                                    continue;
                                }

                                var ivf =
                                from f in fields
                                where f.Ranges.All(r => v < r.min || v > r.max)
                                select f.Name;


                                fieldMask[i].ExceptWith(ivf);

                                void cleanup(HashSet<string> input)
                                {
                                    if (input.Count != 1) return;

                                    var name = input.First();
                                    Console.WriteLine("Found " + name);

                                    var changed = new List<HashSet<string>>();

                                    foreach (var kv in fieldMask)
                                    {
                                        if (kv.Value == input || kv.Value.Count == 1) continue;

                                        kv.Value.Remove(name);
                                        changed.Add(kv.Value);
                                    }

                                    foreach (var value in changed)
                                    {
                                        cleanup(value);
                                    }
                                }

                                cleanup(fieldMask[i]);

                                i += 1;
                            }
                        }
                        break;
                }
            }

            var parsed = new Dictionary<string, int>();

            if (your == null) throw new InvalidOperationException();

            foreach (var kv in fieldMask)
            {
                parsed[kv.Value.First()] = your[kv.Key];
            }

            Console.WriteLine(System.Text.Json.JsonSerializer.Serialize(parsed));

            return parsed.Where(kv => kv.Key.StartsWith("departure")).Select(x => (long)x.Value).Aggregate((long x, long y) => x * y);
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }
}