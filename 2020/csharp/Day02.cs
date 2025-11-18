using System.Text;
using System.Text.RegularExpressions;

namespace AdventOfCode.CSharp.Y2020;

public static class Day02
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        int Part1()
        {
            bool checkValid(string line)
            {
                var m = Regex.Match(line, @"([0-9]+)\-([0-9]+) ([a-z])\: ([a-z]+)");
                if (!m.Success)
                {
                    throw new InvalidOperationException($"Line doesn't match: {line}");
                }

                var min = int.Parse(m.Groups[1].Value);
                var max = int.Parse(m.Groups[2].Value);
                var c = m.Groups[3].Value[0];
                var p = m.Groups[4].Value;

                var ccount = p.Where(x => x == c).Count();

                return ccount >= min && ccount <= max;
            }

            var numbers =
                from line in Input
                where checkValid(line)
                select line;

            return numbers.Count();
        }

        int Part2()
        {
            bool checkValid(string line)
            {
                var m = Regex.Match(line, @"([0-9]+)\-([0-9]+) ([a-z])\: ([a-z]+)");
                if (!m.Success)
                {
                    throw new InvalidOperationException($"Line doesn't match: {line}");
                }

                var p1 = int.Parse(m.Groups[1].Value) - 1;
                var p2 = int.Parse(m.Groups[2].Value) - 1;
                var c = m.Groups[3].Value[0];
                var password = m.Groups[4].Value;

                return password[p1] == c ^ password[p2] == c;
            }

            var numbers =
                from line in Input
                where checkValid(line)
                select line;

            return numbers.Count();
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }
}