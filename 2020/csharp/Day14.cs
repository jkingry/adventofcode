using System.Text;
using System.Text.RegularExpressions;

namespace AdventOfCode.CSharp.Y2020;

public static class Day14
{

    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        long Part1()
        {
            long setmask = 0;
            long unsetmask = 0;

            Dictionary<long, long> a = new Dictionary<long, long>();

            foreach (var line in Input)
            {
                if (line.StartsWith("mask = "))
                {
                    var maskText = line.Substring(7);
                    setmask = 0;
                    unsetmask = 0;
                    for (var i = 0; i < maskText.Length; ++i)
                    {
                        switch (maskText[i])
                        {
                            case 'X':
                                break;
                            case '1':
                                setmask |= (1L << (35 - i));
                                break;
                            case '0':
                                unsetmask |= (1L << (35 - i));
                                break;
                        }
                    }
                    continue;
                }

                var m = Regex.Match(line, @"mem\[([0-9]+)\] = ([0-9]+)");
                var mem = long.Parse(m.Groups[1].Value);
                var v = long.Parse(m.Groups[2].Value);

                a[mem] = (v | setmask) & ~unsetmask;
            }

            return a.Values.Sum();
        }

        long Part2()
        {
            long setmask = 0;
            long floatmask = 0;
            List<long> floats = new List<long>();

            Dictionary<long, long> a = new Dictionary<long, long>();

            foreach (var line in Input)
            {
                if (line.StartsWith("mask = "))
                {
                    var maskText = line.Substring(7);
                    setmask = 0;
                    floatmask = 0;
                    floats.Clear();
                    floats.Add(0);

                    for (var i = 0; i < maskText.Length; ++i)
                    {
                        var bit = (1L << (35 - i));

                        switch (maskText[i])
                        {
                            case 'X':
                                floatmask |= bit;

                                foreach (var f in floats.ToArray())
                                {
                                    floats.Add(f | bit);
                                }

                                break;
                            case '1':
                                setmask |= bit;
                                break;
                            case '0':
                                break;
                        }
                    }
                    continue;
                }

                var m = Regex.Match(line, @"mem\[([0-9]+)\] = ([0-9]+)");
                var mem = long.Parse(m.Groups[1].Value);
                var v = long.Parse(m.Groups[2].Value);

                var basemem = (mem | setmask) & ~floatmask;

                foreach (var f in floats)
                {
                    long finalmem = basemem | (f & floatmask);

                    a[finalmem] = v;
                }

            }

            return a.Values.Sum();
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }
}