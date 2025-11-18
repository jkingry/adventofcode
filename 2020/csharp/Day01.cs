using System.Text;

namespace AdventOfCode.CSharp.Y2020;

public static class Day01
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        int Part1()
        {
            var numbers = from line in Input
                          where !string.IsNullOrWhiteSpace(line)
                          select int.Parse(line);

            var na = numbers.ToArray();

            var find =
                from x in na
                from y in na
                where x != y && (x + y) == 2020
                select x * y;


            return find.First();
        }

        int Part2()
        {
            var numbers = from line in Input
                          where !string.IsNullOrWhiteSpace(line)
                          select int.Parse(line);

            var na = numbers.ToArray();

            var find =
                from x in na
                from y in na
                from z in na
                where x != y && y != z && (x + y + z) == 2020
                select x * y * z;

            return find.First();
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }
}