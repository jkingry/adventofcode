namespace AdventOfCode.CSharp.Y2018;

public static class Day02
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        int Part1()
        {
            var a2 = 0;
            var a3 = 0;
            foreach (var line in Input)
            {
                var hasTwo = false;
                var hasThree = false;
                foreach (var c in line)
                {
                    var cc = line.Count(x => x == c);
                    if (cc == 2) hasTwo = true;
                    if (cc == 3) hasThree = true;
                }
                if (hasTwo) a2 += 1;
                if (hasThree) a3 += 1;
            }
            return a2 * a3;
        }

        output(1, Part1().ToString());
    }
}